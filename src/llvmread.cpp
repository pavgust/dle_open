#include "stdafx.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Operator.h>
#include <llvm/ADT/DepthFirstIterator.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
//#include <llvm/Support/FormattedStream.h>

#include "factgen.h"

#ifdef _MSC_VER
#ifdef _DEBUG
#   define LLVM_LIBRARY_SUFFIX "-d"
#else
#   define LLVM_LIBRARY_SUFFIX ""
#endif

#pragma comment( lib, "LLVMCore" LLVM_LIBRARY_SUFFIX )
#pragma comment( lib, "LLVMSupport" LLVM_LIBRARY_SUFFIX )
#pragma comment( lib, "LLVMBitReader" LLVM_LIBRARY_SUFFIX )
#pragma comment( lib, "LLVMBinaryFormat" LLVM_LIBRARY_SUFFIX )
#endif

using namespace llvm;
char const* const builtin_alloc_name = "__builtin_alloc";
char const* const builtin_vararg_name = "__builtin_vararg";

Module* md = nullptr;
LLVMContext llctxt;

struct statistics
{
    size_t num_functions{0};
    size_t num_basic_blocks{0};
};

struct global_values
{
    bt_unordered_map<llvm::Value const*,rref<def_tc>> defs;
    bt_unordered_map<llvm::Value const*,rref<def_tc>> non_pointers;

    bt_unordered_map<Value const*,unsigned> non_pointer_names;
    bt_unordered_map<Value const*,unsigned> pointer_names;

    GlobalVariable* constant_non_pointer;
    GlobalVariable* universe;
    

    auto new_global_def( Value const& val, rref<def_tc> const def )
    {
        defs.emplace( &val, def );
        if( val.getType()->isPointerTy() ) {
            set_pointer_typed( def );
        }
    }
};

namespace {
    global_values globals;
};

auto inline to_sref( llvm::StringRef const& x )
{
    return string_ref( x.data(), x.size() );
}

auto inline to_sref( llvm::Twine const& x )
{
    return string_ref( x.str() );
}

auto inline to_rref_str( Twine const& x )
{
    return rref_str( x.str() );
}

template< typename T >
auto handle_error( llvm::ErrorOr<T> res ) -> T
{
    if( error_code ec = res.getError() ) {
        cerr << ec.message() << endl;
        exit( 1 );
    }
    return move( *res );
}

template< typename T >
auto handle_error(llvm::Expected<T> res) -> T
{
	if( auto err = res.takeError() ) {
		llvm::logAllUnhandledErrors(std::move(err), llvm::errs(), "Error: " );
		exit(1);
	}
	return move(*res);
}

template< typename T >
auto get_id(
    bt_unordered_map<T, unsigned>& c,
    T const& x )
    -> unsigned
{
    return c.emplace( x, (unsigned)c.size() ).first->second;
}

// TODO: use more operators
auto get_gep( 
    GEPOperator const* v, 
    rref<def_tc> const ptr_def,
    rref_str const global_name ) 
    -> rref<def_tc>
{
    rref<def_tc> base;
    auto& dl = md->getDataLayout();
    APInt ap_offset( 
        dl.getPointerSizeInBits( v->getPointerAddressSpace() ), 0 );
    if( v->accumulateConstantOffset( dl, ap_offset ) ) {
        base = g_ctxt.construct_gep(
            ptr_def, 
            g_ctxt.construct_integer( 
                (int32_t)ap_offset.getSExtValue() ) );
    }
    else {
        base = g_ctxt.construct_gep(
            ptr_def, 
            g_ctxt.construct_non_ptr( global_name ) );
    }
    return base;
}

auto contains_pointers( Type* ty ) -> bool
{
    if( ty->isPointerTy() ) {
        return true;
    }
    auto num_contained = ty->getNumContainedTypes();
    for( size_t i = 0; i < num_contained; ++i ) {
        if( contains_pointers( ty->getContainedType( i ) ) ) {
            return true;
        }
    }
    return false;
}

auto get_constant_value( Value const* p ) -> rref<def_tc>
{
    Constant const* v = dyn_cast<Constant>( p );
    if( v->getType()->isPointerTy() ) {
        if( auto* gv = dyn_cast<GlobalVariable>(v) ) {
            if( gv->isConstant() 
                && !contains_pointers( gv->getValueType() ) ) 
            {
                return must_find( globals.defs, globals.constant_non_pointer );
            }
        }

        if( auto* cexpr = dyn_cast<ConstantExpr>(v) ) {
            auto const op = cexpr->getOpcode();
            if( auto* gep = dyn_cast<GEPOperator>(cexpr) ) {
                return get_gep( 
                    gep, 
                    get_constant_value( gep->getPointerOperand() ),
                    "@" + to_string( get_id( 
                        globals.pointer_names, dyn_cast<Value>( v ) ) ) );
            }
            else if( op == Instruction::BitCast
                || op == Instruction::PtrToInt
                || op == Instruction::IntToPtr ) 
            {
                return get_constant_value( v->getOperand( 0 ) );
            }
            else {
                outs() << *v << '\n'; outs().flush();
                die();
            }
        }
        else if( auto* nullp = dyn_cast<ConstantPointerNull>(v) ) {
            return g_ctxt.construct_non_ptr( "@null" );
        }
        else if( auto* nullp = dyn_cast<UndefValue>(v) ) {
            return g_ctxt.construct_non_ptr( "@null" );
        }
        return must_find( globals.defs, v );
    }
    else {
        auto const iter = globals.non_pointers.emplace(
            v,
            rref<def_tc>() );
        if( iter.second ) {
            iter.first->second = g_ctxt.construct_non_ptr(
                "@" + to_string( get_id( 
                    globals.pointer_names, dyn_cast<Value>( v ) ) ) );
        }
        return iter.first->second;
    }
}

auto get_i32( int32_t const i )
{
    return ConstantInt::get( Type::getInt32Ty( llctxt ), i );
}

auto create_initializers( 
    GlobalVariable* global,
    Constant* global_gep,
    rref<def_tc> const gdef,
    Constant* init,
    int32_t const init_offset )
    -> void
{
    auto* const init_ty = init->getType();
    if( init_ty->isAggregateType() || init_ty->isVectorTy() ) {
        for( size_t i = 0;
            auto* elem = init->getAggregateElement( i );
            ++i )
        {
            auto* next_global_gep = ConstantExpr::getInBoundsGetElementPtr( 
                nullptr, global_gep, 
                ArrayRef<Value*>{ get_i32(0), get_i32(i) } );
            create_initializers( 
                global, next_global_gep, gdef, elem, init_offset );                  
        }
    }
    else if( auto* cexpr = dyn_cast<ConstantExpr>(init) ) 
    {
        auto const op = cexpr->getOpcode();
        if( op == Instruction::BitCast
            || op == Instruction::PtrToInt
            || op == Instruction::IntToPtr ) 
        {
            create_initializers( 
                global, global_gep, gdef, 
                dyn_cast<Constant>( init->getOperand( 0 ) ), 
                init_offset );
        }
        else if( auto* gep = dyn_cast<GEPOperator>(init) ) {
            auto& dl = md->getDataLayout();
            APInt ap_offset( 
                dl.getPointerSizeInBits( gep->getPointerAddressSpace() ), 0 );
            auto const res = gep->accumulateConstantOffset( dl, ap_offset );
            my_assert( res );
            create_initializers( 
                global, global_gep, gdef, 
                dyn_cast<Constant>( init->getOperand( 0 ) ), 
                init_offset + ap_offset.getSExtValue() );
        }
        else {
            outs() << "Unhandled initializer: " << init << "\n"; 
            outs().flush();
            exit( 1 );
        }
    }
    else if( init_ty->isPointerTy() ) {
        //outs() << *init->getType() << "\t" << *global_gep->getType() << "\t"
        //    << *global_gep->getType()->getContainedType(0) 
        //    << "\n"; 
        //outs().flush();

        int32_t global_offset;
        if( auto* const gep_op = dyn_cast<GEPOperator>( global_gep ) ) {
            auto& dl = md->getDataLayout();
            APInt ap_offset( 
                dl.getPointerSizeInBits( 
                    gep_op->getPointerAddressSpace() ), 0 );
            auto const res = gep_op->accumulateConstantOffset( dl, ap_offset );
            my_assert( res );
            global_offset = (int32_t)ap_offset.getSExtValue();
        }
        else {
            global_offset = 0;
        }

        auto const val = get_constant_value( init );
        //outs()
        //    << "\t" << *global
        //    << "\t" << global_offset
        //    << "<--" << *val_base
        //    << "\t" << val_offset
        //    << "\n"; outs().flush();

        construct<global_initializer_tc>( 
            gdef,
            global_offset,
            val,
            init_offset );
    }
}

auto undecorate_intrinsic_name( StringRef const name )
    -> string_ref
{
    if( name.startswith( "llvm.memcpy" ) ) {
        return "llvm.memcpy";
    }
    else if( name.startswith( "llvm.memmove" ) ) {
        return "llvm.memmove";
    }
    else if( name.startswith( "llvm.memset" ) ) {
        return "llvm.memset";
    }
    return to_sref( name );
}

struct inst_visitor : llvm::InstVisitor<inst_visitor>
{
    using parent_type = llvm::InstVisitor<inst_visitor>;

    Function const& func;
    function_context& fctxt;
    block curr_block;
    bt_unordered_map<Value const*,unsigned> pointer_names;

    bt_unordered_map<llvm::BasicBlock const*,block> blocks;
    bt_unordered_map<llvm::Value const*,rref<def_tc>> defs;
    bt_unordered_map<llvm::Value const*,rref<def_tc>> non_pointers;
    unsigned current_line{0};

    auto new_local_def( Value const& val, rref<def_tc> const def )
    {
        defs.emplace( &val, def );
        if( val.getType()->isPointerTy() ) {
            set_pointer_typed( def );
        }
    }

    inst_visitor( Function const& func0 )
        : func( func0 ),
        fctxt( ( g_ctxt.fctxts.emplace_back( to_rref_str( func0.getName() ) ),
            g_ctxt.fctxts.back() ) )
    {
        rref_vector<def_tc> args;
        for( auto const& arg : func.args() ) {
            auto const arg_def = construct<def_tc::argument>( 
                fctxt.func,
                to_rref_str( to_local_name( arg ) ) );
            if( arg.getType()->isPointerTy() ) {
                new_local_def( 
                    arg, 
                    arg_def );
            }
            args.push_back( arg_def );
        }
        auto const name = undecorate_intrinsic_name( func.getName() );
        construct<function_tc>(
            construct<location_tc>( 0, 0, 0, 0 ),
            name,
            concat( args ),
            nil<statement_tc>(),
            must_find( globals.defs, &func ) );
    }

    auto get( llvm::BasicBlock const& b ) -> block 
    {
        auto const ret = blocks.emplace( &b, block{} );
        if( ret.second ) {
            ret.first->second = fctxt.new_block();
        }
        return ret.first->second;
    }
    
    auto get( llvm::Value const* v ) -> rref<def_tc>
    {
        if( isa<Instruction const>( v ) || isa<Argument const>( v ) ) {
            if( v->getType()->isPointerTy() ) {
                return must_find( defs, v );
            }
            else {
                auto const iter = non_pointers.emplace(
                    v,
                    rref<def_tc>() );
                if( iter.second ) {
                    iter.first->second = g_ctxt.construct_non_ptr(
                        to_global_name( *v ) );
                }
                return iter.first->second;
            }
        }
        else {
            if( v->getType()->isPointerTy() ) {
                if( auto* cexpr = dyn_cast<ConstantExpr>(v) ) {
                    return get_constant_value( v );
                }
                else if( auto* nullp = dyn_cast<ConstantPointerNull>(v) ) {
                    return g_ctxt.construct_non_ptr( "@null" );
                }
                else if( auto* nullp = dyn_cast<UndefValue>(v) ) {
                    return g_ctxt.construct_non_ptr( "@null" );
                }
                else if( auto* nullp = dyn_cast<InlineAsm>(v) ) {
                    // TODO?
                    return g_ctxt.construct_non_ptr( "@null" );
                }
                return must_find( globals.defs, v );
            }
            else {
                auto const iter = globals.non_pointers.emplace(
                    v,
                    rref<def_tc>() );
                if( iter.second ) {
                    iter.first->second = g_ctxt.construct_non_ptr(
                        rref_str( to_global_name( *v ) ) );
                }
                return iter.first->second;
            }
        }
    }

    auto to_local_name( llvm::Value const& v ) -> string
    {
        if( !v.getName().empty() ) {
            return v.getName();
        }
        else {
            return '%' + to_string(
                get_id( pointer_names, &v ) );
        }
    }

    auto to_global_name( llvm::Value const& v ) -> rref_str
    {
        if( auto* i = dyn_cast<Instruction>( &v ) ) {
            return to_rref_str( i->getParent()->getParent()->getName()
                + "/" + to_local_name( v ) );
        }
        else if( auto* i = dyn_cast<Argument>( &v ) ) {
            return to_rref_str( i->getParent()->getName().str()
                + "/" + to_local_name( v ) );
        }
        else if( !v.getName().empty() ) {
            return to_rref_str( v.getName() );
        }
        else {
            return "@" + to_string( 
                get_id( globals.pointer_names, &v ) );
        }
    }

    auto check_uses( llvm::Instruction const& i )
    {
        if( i.getType()->isPointerTy() && !isa<PHINode>( i ) ) {
            for( auto const& u : i.operands() ) {
                if( llvm::isa<llvm::Instruction>( u ) 
                    && cast<Instruction const>( u )->getType()->isPointerTy()
                    && !map_contains( defs, cast<Instruction const>( u ) ) ) 
                {
                    llvm::outs() << i << '\n';
                    llvm::outs() << "    Uses " << *u << '\n';
                    llvm::outs() << "******* not found\n";
                }

            }
        }
        //for( auto const& u : i.users() ) {
        //    llvm::outs() << "    Users " << *u << '\n';
        //    if( !set_contains( defs, u ) ) {
        //        //llvm::outs() << "******* not found\n";
        //    }

        //}
    }

    void visit( llvm::BasicBlock& BB ) {
        parent_type::visit( BB.begin(), BB.end() );
    }

    auto visit( llvm::Instruction& i ) -> void
    {
        if( i.getDebugLoc() ) {
            current_line = i.getDebugLoc().getLine();
        }
        parent_type::visit( i );
    }

    // Terminators
    auto visitTerminatorInst( llvm::TerminatorInst const & i ) -> void
    {
        auto const num_succ = i.getNumSuccessors();
        for( auto j = 0u; j < num_succ; ++j ) {
            auto const* succ = i.getSuccessor( j );
            add_edge( fctxt, curr_block, get( *succ ) );
        }
        check_uses( i );
    }
    auto visitReturnInst( llvm::ReturnInst const& i ) -> void
    {
        if( i.getNumOperands() > 0
            && i.getOperand( 0 )->getType()->isPointerTy() ) 
        {
            fctxt.returns.emplace_back( 
                get( i.getOperand( 0 ) ),
                curr_block.num );
            check_uses( i );
        }
    }

    // Casts
    auto visitBitCastInst( llvm::BitCastInst const& i ) -> void
    {
        if( i.getType()->isPointerTy() ) {
           new_local_def( 
                i, 
                get( i.getOperand( 0 ) ) );
            check_uses( i );
        }
    }
    auto visitAddrSpaceCastInst( llvm::AddrSpaceCastInst const& i ) -> void
    {
        if( i.getType()->isPointerTy() ) {
            new_local_def( 
                i, 
                get( i.getOperand( 0 ) ) );
            check_uses( i );
        }
    }
    auto visitIntToPtrInst( llvm::IntToPtrInst const& i ) -> void
    {
        auto const def = g_ctxt.construct_global( universe_def );
        new_local_def( i, def );
        check_uses( i );
    }

    // Unary instructions
    auto visitAllocaInst( llvm::AllocaInst const& i ) -> void
    {
        new_local_def( 
            i, 
            fctxt.construct_def<def_tc::alloc_a>(
                fctxt.func,
                curr_block.num,
                to_local_name( i ) ) );
        fresh_block( curr_block, fctxt );
        check_uses( i );
    }
    auto visitLoadInst( llvm::LoadInst const& i ) -> void
    {
        new_local_def( 
            i,
            fctxt.construct_def<def_tc::load>(
                fctxt.func,
                curr_block.num,
                current_line,
                get( i.getOperand( 0 ) ) ) );
        check_uses( i );
    }
    auto visitGetElementPtrInst( llvm::GetElementPtrInst const& i ) -> void
    {
        auto* op = dyn_cast<GEPOperator>( &i );
        rref<def_tc> base = get_gep( 
            op,
            get( op->getPointerOperand() ),
            to_global_name( i ) );
        new_local_def( i, base );
        check_uses( i );
    }
    
    auto visitSelectInst( llvm::SelectInst const& i ) -> void
    {
        if( i.getType()->isPointerTy() ) {
            auto def = fctxt.construct_def<def_tc::other>(
                fctxt.func,
                curr_block.num,
                to_local_name( i ) );
            for( auto& op : i.operands() ) {
                g_ctxt.assigns.insert( { 
                    to_name( get( op.get() ) ), 
                    to_name( def ) } );
            }
            new_local_def( i, def );
        }
    }

    auto visitVAArgInst( llvm::VAArgInst const& i ) -> void
    {
        auto const def = g_ctxt.construct_global( 
            universe_def );
        new_local_def( i, def );
        check_uses( i );
    }

    // Instruction
    auto visitCallSite( llvm::CallSite const& cs ) -> void
    {
        auto* const func = cs.getCalledFunction();
        auto const& i = *cs.getInstruction();

        if( func && func->getName().startswith( "llvm.dbg" ) ) {
            return;
        }
        if( func && func->getName() == builtin_alloc_name ) {
            new_local_def( 
                i,
                fctxt.construct_def<def_tc::alloc>(
                    fctxt.func,
                    curr_block.num,
                    current_line,
                    to_local_name( i ) ) );
        }
        else {
            auto const args = transform_to_list( 
                cs.args(),
                [&]( Use const& arg ) { return get( arg.get() ); } );
            auto const target = get( cs.getCalledValue() );
            new_local_def( 
                i, 
                fctxt.construct_def<def_tc::invoke>(
                    fctxt.func,
                    curr_block.num,
                    current_line,
                    target,
                    args ) );
        }
        check_uses( i );
        fresh_block( curr_block, fctxt );
    }
    auto visitStoreInst( llvm::StoreInst const& i ) -> void
    {
        fctxt.construct_store(
            fctxt.func,
            curr_block.num,
            current_line,
            get( i.getOperand( 1 ) ),
            get( i.getOperand( 0 ) ) );
        check_uses( i );
        fresh_block( curr_block, fctxt );
    }
    auto visitLandingPadInst( llvm::LandingPadInst const& i ) -> void
    {
        //new_local_def( i, rref<def_tc>() );
        //check_uses( i );
    }
    auto visitPHINode( llvm::PHINode const& i ) -> void
    {
        if( i.getType()->isPointerTy() ) {
            auto const d = fctxt.construct_def<def_tc::other>(
                fctxt.func,
                curr_block.num,
                to_local_name( i ) );
            new_local_def( i, d );
        }
        //check_uses( i );
    }
    auto visitExtractValueInst( llvm::ExtractValueInst const& i ) -> void
    {
        //my_assert( 0 );
        //new_local_def( i, rref<def_tc>() );
        //check_uses( i );
    }
    auto visitInsertValueInst( llvm::InsertValueInst const& i ) -> void
    {
        //my_assert( 0 );
        //new_local_def( i, rref<def_tc>() );
        //check_uses( i );
    }
};

auto llvm_read( fs::path const& file_path ) -> void
{
    llvm::llvm_shutdown_obj lshutdown;  // Call llvm_shutdown() on exit.

    unique_ptr<llvm::MemoryBuffer> membuf = handle_error(
        llvm::MemoryBuffer::getFileOrSTDIN( file_path.string() ) );

    unique_ptr<llvm::Module> pmd = handle_error(
        llvm::parseBitcodeFile( membuf->getMemBufferRef(), llctxt ) );
    md = pmd.get();

    globals.constant_non_pointer = dyn_cast<GlobalVariable>(
        md->getOrInsertGlobal( "@__constant", Type::getInt1Ty( llctxt ) ) );
    globals.constant_non_pointer->setConstant( true );
    globals.universe = dyn_cast<GlobalVariable>(
        md->getOrInsertGlobal( "@__universe", Type::getInt1Ty( llctxt ) ) );

    for( Function const& gv : md->functions() ) {
        auto const addr = g_ctxt.construct_global( 
            undecorate_intrinsic_name( gv.getName() ) );
        if( gv.hasExternalLinkage() ) {
            set_extern( addr );
        }
        globals.new_global_def( gv, addr );
        if( gv.isVarArg() ) {
            set_vararg( addr );
        }
    }

    for( GlobalVariable const& gv : md->globals() ) {
        auto const def = g_ctxt.construct_global( to_sref( gv.getName() ) );
        globals.new_global_def( gv, def );
        if( gv.isConstant() ) {
            set_constant( def );
        }

        // TODO: external global variables
    }

    // Set up initializers after creating function and global defs
    for( GlobalVariable& gv : md->globals() ) {
        if( gv.hasInitializer() ) {
            auto gdef = g_ctxt.construct_global( to_sref( gv.getName() ) );
            create_initializers(
                &gv,
                ConstantExpr::getInBoundsGetElementPtr( 
                    gv.getValueType(), &gv, 
                    ArrayRef<Value*>{ get_i32(0) } ),
                gdef, 
                gv.getInitializer(), 0 );
       }
    }

    for( llvm::Function& f : *md ) {
        if( f.isDeclaration() ) {
            if( !f.isIntrinsic() 
                && f.getName() != builtin_alloc_name
                && g_opt.verbose >= 1 ) 
            {
                cerr << bt::format( "External function: %s\n" )
                    % to_sref( f.getName() );
            }
            
            auto const func_name = undecorate_intrinsic_name( f.getName() );
            construct<function_tc>(
                construct<location_tc>( 0, 0, 0, 0 ),
                func_name,
                nil<def_tc>(),
                nil<statement_tc>(),
                must_find( globals.defs, &f ) );
            continue;
        }

        inst_visitor visitor{ f };

        for( llvm::BasicBlock* b : llvm::depth_first( &f ) ) {
            visitor.curr_block = visitor.get( *b );
            visitor.visit( *b );
        }

        for( auto i = inst_begin( f ); i != inst_end( f ); ++i ) {
            if( auto* phi = dyn_cast<PHINode>( &*i ) ) {
                if( phi->getType()->isPointerTy() ) {
                    auto const phidef = must_find( visitor.defs, phi );
                    for( auto const& op : phi->operands() ) {
                        g_ctxt.assigns.emplace(
                            to_name( visitor.get( op ) ),
                            to_name( phidef ) );
                    }
                }
            }
        }
        visitor.fctxt.order_blocks();
    }

    write_tables();
}
