#include "stdafx.h"
#include "utility.h"

#include "main.h"
#include "table.h"

#include <llvm/IR/Type.h>
#include <llvm/IR/TypeBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Constants.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>

int32_t const i32_invalid_index = -1;

#ifdef _MSC_VER
#   include <io.h>
#   include <Windows.h>
#   define lseek _lseek
#   ifdef _M_IX86
#       define HEADER_SUFFIX "32"
#       define SYMBOL_PREFIX "_"
#   else
#       define HEADER_SUFFIX "64"
#       define SYMBOL_PREFIX 
#   endif
#   ifdef _DEBUG
#       define LLVM_LIBRARY_SUFFIX "-d"
#   else
#       define LLVM_LIBRARY_SUFFIX ""
#   endif
#else
#   define SYMBOL_PREFIX
#   ifndef __x86_64
#       define HEADER_SUFFIX "32"
#   else
#       define HEADER_SUFFIX "64"
#   endif
#   define LLVM_LIBRARY_SUFFIX ""
#endif

#ifdef _MSC_VER
#pragma comment(lib, "LLVMLTO" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMPasses" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMSymbolize" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMDebugInfoPDB" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMDebugInfoDWARF" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMFuzzMutate" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMTableGen" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMLineEditor" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMOrcJIT" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMCoverage" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMMIRParser" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMObjectYAML" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMLibDriver" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMOption" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMWindowsManifest" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86Disassembler" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86AsmParser" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86CodeGen" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMGlobalISel" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMSelectionDAG" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMAsmPrinter" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMDebugInfoCodeView" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMDebugInfoMSF" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86Desc" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMMCDisassembler" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86Info" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86AsmPrinter" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMX86Utils" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMMCJIT" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMInterpreter" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMExecutionEngine" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMRuntimeDyld" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMCodeGen" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMTarget" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMCoroutines" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMipo" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMInstrumentation" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMVectorize" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMScalarOpts" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMLinker" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMIRReader" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMAsmParser" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMInstCombine" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMTransformUtils" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMBitWriter" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMAnalysis" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMProfileData" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMObject" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMMCParser" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMMC" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMBitReader" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMCore" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMBinaryFormat" LLVM_LIBRARY_SUFFIX ".lib" )
#pragma comment(lib, "LLVMSupport" LLVM_LIBRARY_SUFFIX ".lib" )

HANDLE win_heap;
extern "C" void* my_malloc( size_t sz )
{
    return HeapAlloc( win_heap, 0, sz );
}

extern "C" void* my_realloc( void* ptr, size_t sz, size_t old_sz )
{
    if( !ptr ) {
        return my_malloc( sz );
    }
    return HeapReAlloc( win_heap, 0, ptr, sz );
}

extern "C" void my_free( void* ptr, size_t sz )
{
    if( !ptr ) {
        return;
    }
    HeapFree( win_heap, 0, ptr );
}
#endif 

chrono::high_resolution_clock::time_point start_time;
extern "C" void high_performance_reset_timer()
{
    start_time = chrono::high_resolution_clock::now();
}

extern "C" void high_performance_print_timer()
{
    chrono::duration<double> const elapsed = 
        chrono::high_resolution_clock::now() - start_time;
    printf( "%.6fs", elapsed.count() );
}

extern "C" void high_performance_log_timer()
{
    static chrono::high_resolution_clock::time_point log_time;
    auto const now = chrono::high_resolution_clock::now();
    chrono::duration<double> const elapsed = now - log_time;
    log_time = now;
    fprintf( stderr, "%.9fs: ", elapsed.count() );
}


extern "C" void my_exit( int i )
{
    fflush( stdout );
    fflush( stderr );
    exit( i );
}

namespace boost 
{
template< typename T >
struct range_mutable_iterator< ::llvm::iterator_range<T> >
{
    typedef T type;
};
template< typename T >
struct range_const_iterator< ::llvm::iterator_range<T> >
{
    typedef T type;
};
}

namespace codegen2 {
using namespace llvm;
LLVMContext llctxt;

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
	if (auto err = res.takeError()) {
		llvm::logAllUnhandledErrors(std::move(err), llvm::outs(), "Error: ");
		exit(1);
	}
	return move(*res);
}

auto sref( rref_str const x )
{
    auto const y = x.str();
    return StringRef( y.data(), y.size() );
}

auto sref( string_ref const& x )
{
    return StringRef( x.data(), x.size() );
}

template< typename T, typename Rng >
auto to_ptr_vector( Rng const& range )
{
    vector<T*> vec;
    for( auto& x : range ) {
        vec.push_back( &x );
    }
    return vec;
}

template< typename T >
auto check( T* const p ) -> T*
{
    my_assert( p );
    return p;
}

template< typename T >
auto check( rref<T> const p ) -> rref<T>
{
    my_assert( p );
    return p;
}

template< typename T >
auto deref( T* const p ) -> T&
{
    my_assert( p );
    return *p;
}

template< typename T >
auto at( T const& x, size_t const i ) -> decltype(auto)
{
    auto iter = x.begin();
    advance( iter, i );
    return *iter;
}

template< typename T >
auto slice( vector<T> const& x, size_t from ) -> ArrayRef<T>
{
    return ArrayRef<T>( x.data() + from, x.data() + x.size() );
}

struct function_context;
struct bb_context
{
    function_context& fctxt;
    BasicBlock* const bb;
    IRBuilder<> builder;
    bb_context* idom{ nullptr };
    unordered_map< 
        rref<typed_term_tc>,
        Value*, 
        unlocated_hash,
        unlocated_equality > term_map;
    bb_context( function_context& fctxt0, BasicBlock* const bb0 )
        : fctxt( fctxt0 ),
        bb( bb0 ),
        builder( bb0 )
    {}
    bb_context( bb_context const& ) = delete;
    auto find_term( rref<typed_term_tc> const t ) -> Value*&
    {
        auto*& res = term_map[t];
        if( res ) {
            return res;
        }
        else if( idom != this ) {
            auto*& res2 = idom->find_term( t );
            if( res2 ) {
                return res2;
            }
        }
        return res;
    }
    auto create_dom_block( Twine const& name = "" )
        -> bb_context*;
    auto create_self_dom_block( Twine const & name )
        -> bb_context*;
};

struct function_context
{
    Function* const func;
    IRBuilder<> builder;
    deque<bb_context> bctxts;
    Value* return_val{ nullptr };

    function_context( Function* const func0 )
        : func( func0 ),
        builder( BasicBlock::Create( llctxt, "", func0 ) )
    {}

    auto create_block(
        bb_context* const idom,
        Twine const& name = "" ) 
        -> bb_context*
    {
        auto* bb = BasicBlock::Create( llctxt, name, func );
        bctxts.emplace_back( *this, bb );
        bctxts.back().idom = idom;
        return &bctxts.back();
    }

    auto create_self_dom_block( Twine const& name = "start" ) 
        -> bb_context*
    {
        auto* bb = BasicBlock::Create( llctxt, name, func );
        bctxts.emplace_back( *this, bb );
        bctxts.back().idom = &bctxts.back();
        return &bctxts.back();
    }

    auto create_start_block()
        -> bb_context*
    {
        auto* start_b = create_self_dom_block();
        auto* entry_br = builder.CreateBr( start_b->bb );
        builder.SetInsertPoint( entry_br );
        return start_b;
    }
};

auto bb_context::create_dom_block(
    Twine const& name )
    -> bb_context*
{
    return fctxt.create_block( this, name );
}

auto bb_context::create_self_dom_block(
    Twine const& name )
    -> bb_context*
{
    return fctxt.create_self_dom_block( name );
}

auto start_unification_block(
    bb_context*& b )
    -> bb_context*
{
    auto* new_b = b->create_dom_block( "unification_start" );
    b->builder.CreateBr( new_b->bb );
    auto* ret_b = b->create_dom_block( "unification_fail" );
    b = new_b;
    return ret_b;
}

struct environment
{
    Module* md;
    Type* objid32_type{nullptr};
    Type* size_type{nullptr};
    Type* boolean_type{nullptr};

    Type* term_set_type{nullptr};
    Type* term_set_stable_type{nullptr};
    Type* array_type{nullptr};
    Type* list_type{nullptr};
    Type* hash_set_type{nullptr};
    Type* bitmap_type{nullptr};
    Type* worklist_type{nullptr};

    Type* list_iter_type{nullptr};
    Type* array_iter_type{nullptr};

    Type* env_type{nullptr};
    Value* env{ nullptr };

    StructType* root_type{nullptr};
    Value* root{ nullptr };

    Function* main_func{nullptr};
    Function* init_term_set_func{nullptr};
    Function* init_term_set_stable_func{nullptr};
    Function* init_array_func{nullptr};
    Function* init_list_func{nullptr};
    Function* init_hash_set_func{nullptr};
    Function* init_bitmap_func{nullptr};
    Function* init_worklist_func{nullptr};

    Function* printf_func{nullptr};
    Function* set_show_fd_func{nullptr};
    Function* show_cstring_func{nullptr};
    Function* show_i32_func{nullptr};
    Function* show_string_func{nullptr};
	Function* cat_string_func{nullptr};
	Function* match_string_func{nullptr};
    Function* exit_func{nullptr};

    Function* flush_file_func{nullptr};
    Function* reset_timer_func{nullptr};
    Function* print_timer_func{nullptr};

    vector<Function*> all_funcs;
    rref_map<index_structure_tc,StructType*> structures;
    rref_map<adorned_typed_predicate_tc,Function*> direct_functions;
    rref_map<index_structure_tc,Function*> init_functions;
    rref_map<typed_function_tc,Function*> user_functions;
    bt_unordered_map<pred_scc_set const*,Function*> scc_fix_function;

    bt_unordered_map<string,Value*> global_strings;
    bt_unordered_map<join_edge const*,Value*> join_scan_counts;
    bt_unordered_map<join_edge const*,Value*> join_edge_counts;
    bt_unordered_map<join_edge_set const*,Value*> join_edge_set_counts;
    rref_map<predicate_index_tc,Value*> 
        insertion_counts,
        insertion_success_counts;
    environment()
    {
    }
};

environment cenv;

auto get_descr_type( rref<index_info_tc> const idx_info ) -> Type*
{
    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        return cenv.array_type;
    TYPE_CASE_X( hash_map )
        return cenv.term_set_stable_type;
    TYPE_CASE_X( boolean )
        return cenv.boolean_type;
    TYPE_CASE_X( value )
        return ArrayType::get( cenv.objid32_type, x.args.size() );
    TYPE_CASE_X( value_list )
        return cenv.list_type;
    TYPE_CASE_X( hash_set )
        return cenv.hash_set_type;
    TYPE_CASE_X( bitmap )
        return cenv.bitmap_type;
    TYPE_CASE_X( replace )
        return ArrayType::get( cenv.objid32_type, x.args.size() );
    TYPE_CASE_X( join )
        return cenv.objid32_type;
    TYPE_CASE_DIE( worklist )
    TYPE_CASE_DIE( direct )
    TYPE_CASE_DIE( source )
    TYPE_CASE_DIE( sink )
    TYPE_CASE_DIE( aggregate )
    TYPE_CASE_DIE( builtin )
    END_TYPE_SWITCH_
}

auto get_node_type( rref<index_node_tc> const node ) -> Type*
{
    BEGIN_TYPE_SWITCH_( index_node_tc, node )
    TYPE_CASE_DIE( root )
    TYPE_CASE_X( single_map )
        return get_descr_type( must_find( env.index_infos, node ) );
    TYPE_CASE_X( multi_map )
        return get_descr_type( must_find( env.index_infos, node ) );
    TYPE_CASE_X( leaf )
        return get_descr_type( must_find( env.index_infos, node ) );
    TYPE_CASE_X( term_set )
        return cenv.term_set_type;
    TYPE_CASE_X( worklist )
        return cenv.worklist_type;
    END_TYPE_SWITCH_
}


auto get_structure_type( rref<index_structure_tc> const strt ) -> Type*
{
    auto*& ret = cenv.structures[strt];
    if( ret ) {
        return ret;
    }

    vector<Type*> children;
    for( auto const y : strt->children ) {
        children.push_back( get_node_type( y ) );
    }
    return ret = StructType::create( 
        llctxt, children, 
        "index." + to_string( cenv.structures.size() - 1 ) );
}

auto get_type_from_ptr_arg( Function* p, size_t idx = 0 ) 
{
    return cast<PointerType>( 
        at( p->args(), idx ).getType() )->getElementType();
}

auto set_inline( CallInst* f ) -> CallInst*
{
    if( g_opt.inline_level >= 2 ) {
        f->addAttribute( 
			AttributeList::FunctionIndex,
            Attribute::AlwaysInline );
    }
    return f;
}

auto set_noinline( Function* f ) -> Function*
{
    f->addAttribute( 
		AttributeList::FunctionIndex,
        Attribute::NoInline );
    return f;
}

auto get_element_type( Type* t )
{
    return cast<PointerType>( t )->getElementType();
}

auto get_pointer_type( Type* t )
{
    return PointerType::getUnqual( t );
}

auto get_i1( bool const i )
{
    return ConstantInt::get( Type::getInt1Ty( llctxt ), i );
}

auto get_i8( uint8_t const i )
{
    return ConstantInt::get( Type::getInt8Ty( llctxt ), i );
}

auto get_i32( int32_t const i )
{
    return ConstantInt::get( Type::getInt32Ty( llctxt ), i );
}

auto get_i64( int64_t const i )
{
    return ConstantInt::get( Type::getInt64Ty( llctxt ), i );
}


auto get_invalid_idx()
{
    return get_i32( i32_invalid_index );
}

auto get_i32_zero()
{
    return ConstantInt::get( Type::getInt32Ty( llctxt ), 0 );
}

auto get_size_t( size_t const i )
{
    return ConstantInt::get( cenv.size_type, i );
}

auto get_str( string_ref const& x )
{
    return ConstantDataArray::getString( llctxt, sref( x ) );
}

auto get_str_ptr( string_ref const& x ) -> Value*
{
    auto*& res = cenv.global_strings[string(x)];
    if( res ) {
        return res;
    }

    auto* str = get_str( x );
    auto* strvar = new GlobalVariable{
        *cenv.md,
        str->getType(), true, 
        GlobalValue::PrivateLinkage,
        str };
    strvar->setUnnamedAddr( GlobalValue::UnnamedAddr::Global );
    return res = ConstantExpr::getInBoundsGetElementPtr(
        strvar->getType()->getContainedType( 0u ),
        strvar,
        ArrayRef<Constant*>{ get_i32( 0 ), get_i32( 0 ) } );
}

auto get_function( StringRef const name )
{
    return check( cenv.md->getFunction( name ) );
}

auto get_vararg_function( StringRef const name, size_t const i )
{
    return check( 
        cenv.md->getFunction( ( Twine( name ) + Twine( i ) ).str() ) );
}

auto get_structure_size( rref<index_structure_tc> const x ) -> Value*
{
    return ConstantExpr::getIntegerCast(
        ConstantExpr::getSizeOf( get_structure_type( x ) ),
        cenv.size_type,
        false );
}

auto get_interned_string( string_ref const str ) -> Value*
{
    return get_i32( (int32_t)construct<interned_string_tc>( str ).id );
}

auto debug_function_call( Function* f, ArrayRef<Value*> args ) -> void
{
    for( auto const& pr : zip( args, f->args() ) ) {
        outs() << *get<0>( pr ) << " : "
            << *get<0>( pr )->getType() 
            << " expecting " << *get<1>( pr ).getType() << '\n';
    }
    outs().flush();
}

auto get_counter( join_edge const& x, bool const scan ) -> Value*
{
    Value*& v = !scan 
        ? cenv.join_edge_counts[&x]
        : cenv.join_scan_counts[&x];
    if( !v ) {
        v = new GlobalVariable{
            *cenv.md,
            Type::getInt64Ty( llctxt ), false,
            GlobalValue::PrivateLinkage,
            get_i64( 0 ) };
    }
    return v;
}

auto get_counter( join_edge_set const& x ) -> Value*
{
    Value*& v = cenv.join_edge_set_counts[&x];
    if( !v ) {
        v = new GlobalVariable{
            *cenv.md,
            Type::getInt64Ty( llctxt ), false, 
            GlobalValue::PrivateLinkage,
            get_i64( 0 ) };
    }
    return v;
}


auto get_counter( 
    rref<predicate_index_tc> const x, bool const succeeded ) 
    -> Value*
{
    Value*& v = succeeded 
        ? cenv.insertion_success_counts[x] 
        : cenv.insertion_counts[x];
    if( !v ) {
        v = new GlobalVariable{
            *cenv.md,
            Type::getInt64Ty( llctxt ), false, 
            GlobalValue::PrivateLinkage,
            get_i64( 0 ) };
    }
    return v;
}

auto gen_increment( Value* const ptr, bb_context* const b )
{
    auto* val = b->builder.CreateLoad( ptr );
    b->builder.CreateStore(
        b->builder.CreateAdd( 
            val, 
            ConstantInt::get( get_element_type( ptr->getType() ), 1 ) ),
        ptr );
}

auto gen_increment_scan_counter( join_edge const& edge, bb_context* const b )
{
    if( g_opt.counter >= 2 ) {
        gen_increment( get_counter( edge, true ), b );
    }
}

auto all_bound(
    rref_list<typed_term_tc> const args,
    bb_context& b )
    -> bool;

auto is_bound(
    rref<typed_term_tc> const t,
    bb_context& b )
    -> bool
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        return true;
    TYPE_CASE_X( string )
        return true;
    TYPE_CASE_X( variable )
        return b.find_term( t ) != nullptr;
    TYPE_CASE_X( anonymous )
        return false;
    TYPE_CASE_X( compound )
        return all_bound( x.inner, b );
    TYPE_CASE_X( function )
        return all_bound( x.inner, b );
    TYPE_CASE_X( conditional )
        return is_bound( x.cond, b )
            && is_bound( x.true_block, b ) 
            && is_bound( x.false_block, b );
    TYPE_CASE_X( aggregate )
        return all_bound( x.input, b );
    END_TYPE_SWITCH_
}

auto all_bound(
    rref_list<typed_term_tc> const args,
    bb_context& b )
    -> bool
{
    return ag::all_of(
        args,
        [&]( rref<typed_term_tc> const t )
        { return is_bound( t, b ); } );
}

auto pack_term(
    rref<typed_term_tc> const t,
    bb_context*& b )
    -> Value*;

auto pack_terms(
    rref_list<typed_term_tc> const ts,
    bb_context*& b,
    vector<Value*>& args )
    -> void;

auto get_var_name( rref<typed_term_tc> const t ) -> string_ref
{
    if( t.is<typed_term_tc::variable>() ) {
        return t.get<typed_term_tc::variable>().name.str();
    }
    else {
        return "";
    }
}

auto gen_alloca_from_terms(
    rref_list<typed_term_tc> const ts,
    function_context& fctxt,
    vector<Value*>& args )
{
    for( auto const arg : ts ) {
        auto const name = get_var_name( arg );
        auto* arg_alloca = fctxt.builder.CreateAlloca(
            cenv.objid32_type, nullptr, 
            name.empty() ? Twine( "" ) : Twine{ sref( name ) + "_p" } );
        args.push_back( arg_alloca );
    }
}

auto unpack_terms_from_alloca(
    ArrayRef<Value*> const& vals,
    rref_list<typed_term_tc> const ts,
    bb_context*& b,
    bb_context* const ret_b )
    -> void;

auto gen_user_function(
    rref<typed_function_tc> const tfunc )
    -> Function*;

template< typename... Args >
auto gen_printf(
    string_ref const str, 
    bb_context* const b,
    Args* const... args )
{
    b->builder.CreateCall( 
        cenv.printf_func, 
        { get_str_ptr( str ), args... } );
}

auto gen_write_cstring( string_ref const str, bb_context* const b )
{
    b->builder.CreateCall( 
        cenv.show_cstring_func, 
        { cenv.env, get_str_ptr( str ) } );
}

auto gen_set_show_fd( Value* val_fd, bb_context* const b )
{
    b->builder.CreateCall( 
        cenv.set_show_fd_func, 
        { cenv.env, val_fd } );
}

auto gen_set_show_fd( int val, bb_context* const b )
{
    gen_set_show_fd( get_i32( val ), b );
}

auto gen_flush_file( int val, bb_context* const b )
{
    b->builder.CreateCall( 
        cenv.flush_file_func, 
        { cenv.env, get_i32( val ) } );
}

auto gen_write_comma_delimited(
    ArrayRef<Value*> const& vals,
    rref_vector<proper_type_tc> types,
    bb_context*& b )
    -> void
{
    auto const show_decl = get_function_declaration( 
        dummy_loc(), "show", 1 );

    bool first = true;
    for( auto const& x : zip( vals, types ) ) {
        if( !first ) {
            gen_write_cstring( ",", b );
        }
        auto const tfunc = construct<typed_function_tc>(
            show_decl,
            env.void_type,
            rref_list<proper_type_tc>{ 
                get<1>( x ) } );
        b->builder.CreateCall( 
            gen_user_function( tfunc ), 
            { get<0>( x ) } );
        first = false;
    }
}

auto gen_write_literal(
    rref<typed_literal_tc> const lit,
    bb_context*& b )
    -> void
{
    auto const pred_name = lit->apred->pred->decl->name.to_string();
    gen_write_cstring( pred_name + '(', b );

    vector<Value*> vals;
    rref_vector<proper_type_tc> types;
    for( auto const t : lit->args ) {
        if( is_bound( t, *b ) ) {
            vals.push_back( pack_term( t, b ) );
            types.push_back( term_to_proper_type( t ) );
        }
        else {
            vals.push_back( get_interned_string( "_" ) );
            types.push_back( env.string_type );
        }
    }
    gen_write_comma_delimited( vals, types, b );
    gen_write_cstring( ")", b );

}

auto gen_user_function(
    rref<typed_function_tc> const tfunc )
    -> Function*
{
    Function*& func = cenv.user_functions[tfunc];
    if( func ) {
        return func;
    }

    // find first override that unifies
    rref<function_decl_tc> fdecl = tfunc->decl;
    auto const& decls = env.function_overrides[tfunc->decl];
    for( auto const pdecl : decls | ad::reversed ) {
        my_assert( pdecl->arg_types.size() == tfunc->decl->arg_types.size() );
        variable_type_map tyvar_types;
        bool const res 
            = try_unify_type_variables( 
                tyvar_types, { tfunc->result_type }, { pdecl->result_type } )
            && try_unify_type_variables(
                tyvar_types, tfunc->arg_types, pdecl->arg_types );
        if( res ) {
            fdecl = pdecl;
            break;
        }
    }

    auto const num_args = tfunc->arg_types.size();
    auto* func_type = FunctionType::get(
        tfunc->result_type == env.void_type
            ? Type::getVoidTy( llctxt )
            : cenv.objid32_type,
        vector<Type*>( num_args, cenv.objid32_type ),
        false );

    func = Function::Create( 
        func_type, Function::ExternalLinkage,
        ( bt::format( "%s" ) % tfunc ).str(), cenv.md );
    func->addAttribute( 
        AttributeList::FunctionIndex, 
        Attribute::InlineHint );
    cenv.all_funcs.push_back( func );
    function_context fctxt{ func };
    auto* const start_b = fctxt.create_self_dom_block();
    auto* const entry_br = fctxt.builder.CreateBr( start_b->bb );
    fctxt.builder.SetInsertPoint( entry_br );

    auto const& fdefns = env.function_defns[fdecl->name];
    if( fdefns.empty() ) {
        cerr << bt::format( "No definition found for function '%s'.\n" )
            % tfunc;
        exit( 1 );
    }

    auto* b = start_b->create_dom_block();
    start_b->builder.CreateBr( b->bb );
    for( auto const fdefn : fdefns ) {
        auto* next_b = start_b->create_dom_block();

        auto const ifunc = instantiate_function( tfunc, fdefn );
        auto const targs = ifunc.first;
        auto const fbody = ifunc.second;

        vector<Value*> arg_val_ptrs;
        for( auto& arg : func->args() ) {
            auto* arg_ptr = b->fctxt.builder.CreateAlloca( arg.getType() );
            b->builder.CreateStore( &arg, arg_ptr );
            arg_val_ptrs.push_back( arg_ptr );
        }

        unpack_terms_from_alloca(
            arg_val_ptrs,
            targs,
            b, next_b );

        auto* ret_val = pack_term( fbody, b );
        if( tfunc->result_type == env.void_type ) {
            b->builder.CreateRetVoid();
        }
        else {
            my_assert( ret_val );
            b->builder.CreateRet( ret_val );
        }
        b = next_b;
    }

    if( !g_opt.no_check ) {
        auto const fmt = bt::format( 
            "Pattern matching failure while executing "
            "function '%s' with arguments (" )
            % tfunc;
        gen_set_show_fd( 2, b );
        gen_write_cstring( fmt.str(), b );
        gen_write_comma_delimited( 
            to_ptr_vector<Value>( func->args() ),
            to_vector( tfunc->arg_types ),
            b );
        gen_write_cstring( ").\n", b );
        gen_flush_file( 2, b );
        b->builder.CreateCall( cenv.exit_func, { get_i32( 1 ) } );
    }
    b->builder.CreateUnreachable();
    return func;
}

auto gen_function(
    rref<typed_function_tc> const f,
    rref_list<typed_term_tc> args,
    bb_context*& b )
    -> Value*
{
    vector<Value*> arg_vals;
    pack_terms( args, b, arg_vals );

    auto const func_name = f->decl->name;
    if( func_name == "builtin_show_i32" ) {
        my_assert( arg_vals.size() == 1 );
        b->builder.CreateCall( 
            cenv.show_i32_func, 
            { cenv.env, arg_vals[0] } );
        return nullptr;
    }
    else if( func_name == "builtin_show_string" ) {
        my_assert( arg_vals.size() == 1 );
        b->builder.CreateCall( 
            cenv.show_string_func, 
            { cenv.env, arg_vals[0] } );
        return nullptr;
    }
    else if( func_name == "reset_timer" ) {
        b->builder.CreateCall( cenv.reset_timer_func, {} );
        return nullptr;
    }
    else if( func_name == "print_timer" ) {
        b->builder.CreateCall( cenv.print_timer_func, {} );
        return nullptr;
    }
    else if( func_name == "fprintf" || func_name == "printf" ) {
        if( func_name == "printf" ) {
            args.push_front( 
                construct<typed_term_tc::i32>( dummy_loc(), 1 ) );
            arg_vals.insert( arg_vals.begin(), get_i32( 1 ) );
        }
        my_assert( arg_vals.size() >= 2 );
        auto* fd_val = arg_vals[0];
        auto const fmt_term = args[1];
        if( !fmt_term.is<typed_term_tc::string>() ) {
            cerr << bt::format( 
                "%s: Format string for printf must be a string constant.\n" )
                % get_location( fmt_term );
            exit( 1 );
        }

        auto const show_decl = get_function_declaration(
            dummy_loc(), "show", 1 );
        gen_set_show_fd( fd_val, b );

        auto const fmt = args[1].get<typed_term_tc::string>().value.str();
        auto str = fmt.begin();
        auto fmt_args = args.rest().rest();
        auto arg_no = 2;
        auto is_fmt_spec = [&]( char const* p )
        {
            return *p == '%' && ( p+1 == fmt.end() || *(p+1) != '%' );
        };

        for( auto i = fmt.begin(); ; ) {
            if( i == fmt.end() || is_fmt_spec( i ) ) {
                // print what we have up to now
                if( str != i ) {
                    gen_write_cstring( { str, (size_t)( i - str ) }, b );
                }
                if( i == fmt.end() ) {
                    break;
                }
            }
            if( is_fmt_spec( i ) ) {
                // ignore type specifier
                ++i;
                if( i == fmt.end() || fmt_args.empty() ) {
                    cerr << bt::format( 
                        "%s: Malformed format string.\n" )
                        % get_location( fmt_term );
                    exit( 1 );
                }
                // print argument
                auto const tfunc = construct<typed_function_tc>(
                    show_decl,
                    env.void_type,
                    rref_list<proper_type_tc>{ 
                        term_to_proper_type( fmt_args.head() ) } );
                b->builder.CreateCall( 
                    gen_user_function( tfunc ), 
                    { arg_vals[arg_no] } );
                fmt_args = fmt_args.rest();
                arg_no += 1;
                str = i+1;
            }
            ++i;
        }
        return nullptr;
    }
    else if( func_name == "multiply" ) {
        return b->builder.CreateMul( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "divide" ) {
        return b->builder.CreateSDiv( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "add" ) {
        return b->builder.CreateAdd( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "sub" ) {
        return b->builder.CreateSub( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "less_than" ) {
        return b->builder.CreateIntCast(
            b->builder.CreateICmpSLT( arg_vals[0], arg_vals[1] ),
            Type::getInt32Ty( llctxt ), false );
    }
    else if( func_name == "less_than_eq" ) {
        return b->builder.CreateIntCast(
            b->builder.CreateICmpSLE( arg_vals[0], arg_vals[1] ),
            Type::getInt32Ty( llctxt ), false );
    }
    else if( func_name == "equals" ) {
        return b->builder.CreateIntCast(
            b->builder.CreateICmpEQ( arg_vals[0], arg_vals[1] ),
            Type::getInt32Ty( llctxt ), false );
    }
    else if( func_name == "nequals" ) {
        return b->builder.CreateIntCast(
            b->builder.CreateICmpNE( arg_vals[0], arg_vals[1] ),
            Type::getInt32Ty( llctxt ), false );
    }
    else if( func_name == "bitwise_and" ) {
        return b->builder.CreateAnd( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "bitwise_xor" ) {
        return b->builder.CreateXor( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "bitwise_or" ) {
        return b->builder.CreateOr( arg_vals[0], arg_vals[1] );
    }
    else if( func_name == "and" ) {
        return b->builder.CreateIntCast(
            b->builder.CreateAnd(
                b->builder.CreateICmpNE( arg_vals[0], get_i32_zero() ),
                b->builder.CreateICmpNE( arg_vals[1], get_i32_zero() ) ),
            Type::getInt32Ty( llctxt ), false );
    }
    else if( func_name == "or" ) {
        return b->builder.CreateIntCast(
            b->builder.CreateOr(
                b->builder.CreateICmpNE( arg_vals[0], get_i32_zero() ),
                b->builder.CreateICmpNE( arg_vals[1], get_i32_zero() ) ),
            Type::getInt32Ty( llctxt ), false );
    }
	else if( func_name == "cat" ) {
		return b->builder.CreateCall(
			cenv.cat_string_func,
			{ cenv.env, arg_vals[0], arg_vals[1] } );
	}
	else if( func_name == "match" ) {
		return b->builder.CreateCall(
			cenv.match_string_func,
			{ cenv.env, arg_vals[0], arg_vals[1] } );
	}
    else if( func_name == "seq" ) {
        return arg_vals[1];
    }
    else if( func_name == "void" ) {
        return nullptr;
    }
    else {
        auto* func = check( gen_user_function( f ) );
        return b->builder.CreateCall( func, arg_vals );
    }
}
auto contains_side_effects(
    rref<typed_term_tc> const t )
    -> bool
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        return false;
    TYPE_CASE_X( string )
        return false;
    TYPE_CASE_X( variable )
        return false;
    TYPE_CASE_X( anonymous )
        return false;
    TYPE_CASE_X( compound )
        return ag::any_of( x.inner, &contains_side_effects );
    TYPE_CASE_X( function )
        // TODO: identify non-pure user functions
        auto const func_name = x.func->decl->name.str();
        return func_name == "show"
            || func_name == "printf"
            || func_name == "builtin_show_i32"
            || func_name == "builtin_show_string"
            || ag::any_of( x.inner, &contains_side_effects );        
    TYPE_CASE_X( conditional )
        return contains_side_effects( x.true_block ) 
            || contains_side_effects( x.false_block );
    TYPE_CASE_X( aggregate )
        return false;
    END_TYPE_SWITCH_
}

using gen_next_join_function = function< 
    void (bb_context*, bb_context*, Value*) >;

auto gen_scan(
    Twine const& join_no,
    join_edge const* edge,
    Value* const index_ptr,
    char const* const func_name,
    rref<index_structure_tc> const strt, // null if terminal
    rref_list<typed_term_tc> args,
    bb_context*& b,
    gen_next_join_function const& func )
    -> void;

template< typename T >
auto gen_lookup(
    Twine const& join_no,
    Value* const index_ptr,
    char const* const func_name,
    bool const is_insert,
    rref<index_structure_tc> const strt, // null if terminal
    rref_list<typed_term_tc> args,
    bb_context*& b,
    T const& func )
    -> decltype( func( nullptr, nullptr, nullptr ) );

auto pack_index_inner(
    rref<typed_term_tc> const t,
    rref<adorned_typed_predicate_tc> const apred,
    rref_list<list_tc<typed_term_tc>> const tss,
    Value* const base,
    bb_context*& b )
    -> Value*
{
    auto gen_inner_lookup_or_scan = [&](
        Value* index_ptr,
        char const* const lookup_name,
        char const* const next_name,
        rref_list<typed_term_tc> const args,
        rref<index_structure_tc> const structure,
        rref<index_info_tc> const inner_descr )
        -> Value*
    {
        auto const next_func = [&]( 
            bb_context* inner_b, 
            bb_context* ret_b, 
            Value* next_base ) 
        { 
            auto* ret = pack_index_inner(
                t, apred, tss.rest(),
                next_base, inner_b );
            inner_b->builder.CreateBr( ret_b->bb );
            return ret;
        };

        my_assert( all_bound( args, *b ) );

        return gen_lookup(
            Twine( "index" ), index_ptr,
            lookup_name, false,
            structure, args, b,
            next_func );
    };

    auto const primary = env.primary_index.at( apred );
    my_assert( primary );
    auto const idx_info = primary->idx_info;

    BEGIN_TYPE_SWITCH( index_info_tc, idx_info )
    TYPE_CASE_DEFAULT
        cerr << bt::format( "%s: Only bit-vectors supported.\n" )
            % get_location( t );
        exit( 1 );
    TYPE_CASE_X( array_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.array_type );
        return gen_inner_lookup_or_scan( 
            index_ptr, "lookup_array_", "next_array_",
            tss.head(), 
            x.structure, x.descr );
    TYPE_CASE_X( hash_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.term_set_stable_type );
        return gen_inner_lookup_or_scan( 
            index_ptr, "lookup_hash_", "next_hash_",
            tss.head(), 
            x.structure, x.descr );
    END_TYPE_SWITCH_
}

auto pack_term(
    rref<typed_term_tc> const t,
    bb_context*& b )
    -> Value*
{
    my_assert( b->idom );
    auto& val = b->find_term( t );
    if( val && !contains_side_effects( t ) ) {
        return val;
    }

    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        val = get_i32( x.value.get() );
    TYPE_CASE_X( string )
        val = get_interned_string( x.value.get() );
    TYPE_CASE_DIE( variable ) // must be in term_map
    TYPE_CASE_DIE( anonymous )
    TYPE_CASE_X( compound )
        auto* const term_set_ptr = b->builder.CreateStructGEP(
            cenv.root_type, cenv.root, 
            env.dconstr_offset.const_at( x.dconstr ),
            Twine{ sref( x.dconstr->type->name ) } +
                "::" + sref( x.dconstr->name ) );

        auto const tdconstr = construct<typed_dconstr_tc>(
            x.dconstr->type->name,
            x.dconstr->name );
        auto const tag = env.dconstr_tag.const_at( tdconstr );

        vector<Value*> args = { term_set_ptr, get_i32( (int32_t)tag ) };
        pack_terms( x.inner, b, args );
        auto const pack_func = get_vararg_function(
            "pack_", x.dconstr->args.size() );
        val = set_inline( b->builder.CreateCall( pack_func, args ) );
    TYPE_CASE_X( function )
        val = gen_function( x.func, x.inner, b );
    TYPE_CASE_X( conditional )
        bool const is_void = x.type == env.void_type;
        auto* cond = pack_term( x.cond, b );
        auto* true_b = b->create_dom_block( "true" );
        auto* false_b = b->create_dom_block( "false" );
        auto* join_b = b->create_dom_block( "join" );
        auto* cmp = b->builder.CreateICmpNE( cond, get_i32_zero() );
        auto* res_ptr = b->fctxt.builder.CreateAlloca( cenv.objid32_type );
        b->builder.CreateCondBr( cmp, true_b->bb, false_b->bb );

        // true
        auto* true_val = pack_term( x.true_block, true_b );
        if( !is_void ) {
            true_b->builder.CreateStore( true_val, res_ptr );
        }
        true_b->builder.CreateBr( join_b->bb );

        // false
        auto* false_val = pack_term( x.false_block, false_b );
        if( !is_void ) {
            false_b->builder.CreateStore( false_val, res_ptr );
        }
        false_b->builder.CreateBr( join_b->bb );

        if( !is_void ) {
            val = join_b->builder.CreateLoad( res_ptr );
        }
        b = join_b;
    TYPE_CASE_X( aggregate )
        auto* const func = must_find( cenv.direct_functions, x.apred );
        vector<Value*> args;
        pack_terms( x.input, b, args );
        val = b->builder.CreateCall( func, args );
    END_TYPE_SWITCH_

    my_assert( term_to_proper_type( t ) == env.void_type || val );
    return val;
}

auto pack_terms(
    rref_list<typed_term_tc> const ts,
    bb_context*& b,
    vector<Value*>& args )
    -> void
{
    for( auto const t : ts ) {
        args.push_back( pack_term( t, b ) );
    }
}

auto has_anscentor( bb_context* b, bb_context* const ans )
{
    do {
        if( b == ans ) {
            break;
        }
        b = b->idom;
    } while( b->idom != b );
    return b->idom != b;
}

auto unpack(
    Value* const v,
    rref<typed_term_tc> const t,
    bb_context*& b,
    bb_context* const ret_b )
    -> void
{
    my_assert( b->idom );
    // unification failure block can't dominate current block
    // (current block may have partially succeeded unifications
    // that are started in the term_map)
    my_assert( !has_anscentor( ret_b, b ) );

    // More efficient to unpack compound terms even if bound
    if( !t.is<typed_term_tc::compound>() && is_bound( t, *b ) ) {
        pack_term( t, b );
    }

    auto*& packed = b->find_term( t );
    if( packed ) {
        auto* const cmp = b->builder.CreateICmpEQ( 
            v, packed );
        auto* true_b = b->create_dom_block( "unified" );
        auto* const br = b->builder.CreateCondBr( 
            cmp, true_b->bb, ret_b->bb );
        b = true_b;
    }
    else if( t.is<typed_term_tc::variable>() ) {
        packed = v;
    }
    else if( t.is<typed_term_tc::compound>() ) {
        auto const& x = t.get<typed_term_tc::compound>();
        auto const term_set_ptr = b->builder.CreateStructGEP(
            cenv.root_type, cenv.root, 
            env.dconstr_offset.const_at( x.dconstr ),
            Twine{ sref( x.dconstr->type->name ) } +
                "::" + sref( x.dconstr->name ) );

        auto const tdconstr = construct<typed_dconstr_tc>(
            x.dconstr->type->name,
            x.dconstr->name );
        auto const tag = env.dconstr_tag.const_at( tdconstr );

        auto const unpack_func = get_vararg_function(
            "unpack_", x.dconstr->args.size() );

        vector<Value*> args{ term_set_ptr, v, get_i32( (int32_t)tag ) };
        gen_alloca_from_terms( x.inner, b->fctxt, args );

        auto* call = set_inline( 
            b->builder.CreateCall( unpack_func, args ) );

        auto* unpacked_b = b->create_dom_block( "unpacked" );
        b->builder.CreateCondBr( call, unpacked_b->bb, ret_b->bb );
        unpack_terms_from_alloca(
            slice( args, 3 ),
            x.inner, 
            unpacked_b, ret_b );
        b = unpacked_b;

        // if term doesn't contain an anonymous variable, bind it
        if( is_bound( t, *b ) ) {
            packed = v;
        }
    }
    else if( t.is<typed_term_tc::anonymous>() ) {
        // nothing
    }
    else {
        die();
    }
}

auto unpack_terms_from_alloca(
    ArrayRef<Value*> const& vals,
    rref_list<typed_term_tc> const ts,
    bb_context*& b,
    bb_context* const ret_b )
    -> void
{
    my_assert( bt::size( vals ) == ts.size() );
    for( auto const& x : zip( vals, ts ) ) {
        auto* const arg_alloca = get<0>( x );
        auto const argt = get<1>( x );
        my_assert( arg_alloca->getType()->isPointerTy() );

        auto const name = get_var_name( argt );
        auto* const argv = b->builder.CreateLoad( 
            arg_alloca, 
            sref( name ) );
        unpack( argv, argt, b, ret_b );
    }
}

auto gen_join_node( 
    bb_context*& b,
    join_edge_set const& edges )
    -> void;

auto gen_scan(
    Twine const& join_no,
    join_edge const* edge,
    Value* const index_ptr,
    char const* const func_name,
    rref<index_structure_tc> const strt, // null if terminal
    rref_list<typed_term_tc> args,
    bb_context*& b,
    gen_next_join_function const& func )
    -> void
{
    auto* const next_func = get_vararg_function( func_name, args.size() );
    auto* const iter_alloca = b->fctxt.builder.CreateAlloca( 
        cenv.size_type, nullptr, "iter" );
    b->builder.CreateStore( 
        Constant::getNullValue( cenv.size_type ), 
        iter_alloca );

    auto* loop_header = b->create_dom_block( 
        "#" + Twine( join_no ) + "_scan_header" );
    b->builder.CreateBr( loop_header->bb );

    vector<Value*> arg_vals{ index_ptr, iter_alloca };
    if( strt ) { 
        arg_vals.push_back( get_structure_size( strt ) );
    }

    gen_alloca_from_terms( args, loop_header->fctxt, arg_vals );
    auto* const call = set_inline(
        loop_header->builder.CreateCall( next_func, arg_vals ) );
    my_assert( (bool)strt == call->getType()->isPointerTy() );
    auto cond_var = strt
        ? loop_header->builder.CreateIsNotNull( call )
        : call;

    auto* loop_body = loop_header->create_dom_block(
        "#" + join_no + "_scan_body" );
    auto* loop_exit = loop_header->create_dom_block(
        "#" + join_no + "_scan_exit" );
    loop_header->builder.CreateCondBr(
        cond_var,
        loop_body->bb, loop_exit->bb );
    if( edge ) {
        gen_increment_scan_counter( *edge, loop_body );
    }

    unpack_terms_from_alloca(
        slice( arg_vals, strt ? 3 : 2 ),
        args, 
        loop_body, loop_header );

    auto* next_base = strt ?
        loop_body->builder.CreateBitCast( call,
            PointerType::getUnqual( get_structure_type( strt ) ) )
        : nullptr;

    func( loop_body, loop_header, next_base );
    b = loop_exit;
}

template< typename T >
auto gen_lookup(
    Twine const& join_no,
    Value* const index_ptr,
    char const* const func_name,
    bool const is_insert,
    rref<index_structure_tc> const strt, // null if terminal
    rref_list<typed_term_tc> args,
    bb_context*& b,
    T const& func )
    -> decltype( func( nullptr, nullptr, nullptr ) )
{
    my_assert( !is_insert || strt );
    auto* const lookup_func = get_vararg_function( func_name, args.size() );

    vector<Value*> arg_vals{ index_ptr };
    if( strt ) {
        arg_vals.push_back( get_structure_size( strt ) );
        if( is_insert ) {
            arg_vals.push_back( must_find( cenv.init_functions, strt ) );
        }
    }
    pack_terms( args, b, arg_vals );

    auto* lookup_call = set_inline(
        b->builder.CreateCall( lookup_func, arg_vals ) );
    auto* next_base = strt ?
        b->builder.CreateBitCast( lookup_call,
            PointerType::getUnqual( get_structure_type( strt ) ) )
        : nullptr;
            
    if( is_insert ) {
        auto* ret_b = b->create_dom_block( "finished" );
        auto* old_b = b;
        b = ret_b;
        return func( old_b, ret_b, next_base );
    }
    else {
        auto* true_b = b->create_dom_block( 
            "#" + join_no + "_lookup_true" );
        auto* false_b = b->create_dom_block( 
            "#" + join_no + "_lookup_false" );
        b->builder.CreateCondBr(
            b->builder.CreateIsNotNull( lookup_call ),
            true_b->bb, false_b->bb );

        b = false_b;
        return func( true_b, false_b, next_base );
    }
}

auto get_trace_info( 
    join_edge const& edge )
    -> join_edge::info const*
{
    for( auto const& info : edge.infos ) {
        auto const decl = info.rule.head->apred->pred->decl;
        if( set_contains( env.traced_preds, decl ) ) {
            return &info;
        }
    }
    return nullptr;
}

auto gen_join_inner(
    join_edge const& edge,
    join_edge_set const& next_edges,
    rref<typed_literal_tc> const lit,
    rref<predicate_index_tc> const pidx,
    rref<index_info_tc> const idx_info,
    Value* const base,
    bb_context*& b,
    Value* negation_res_ptr,
    bb_context* negation_landing )
    -> void
{
    auto gen_next = [&]( 
        bb_context* inner_b, bb_context* ret_b )
    {
        auto const* info = get_trace_info( edge );
        if( info ) {
            auto const fmt = bt::format( "%sREAD VALUE " )
                % indenter{ edge.join_no };
            gen_set_show_fd( 1, inner_b );
            gen_write_cstring( fmt.str(), inner_b );
            gen_write_literal( lit, inner_b );
            if( !lit->negated.get() ) {
                gen_write_cstring( "\n", inner_b );
            }
            else {
                gen_write_cstring( " NEGATION FAILURE\n", inner_b );
            }
        }

        if( negation_res_ptr ) {
            my_assert( negation_landing );
            inner_b->builder.CreateStore(
                get_i1( false ),
                negation_res_ptr );
            inner_b->builder.CreateBr( negation_landing->bb );
        }
        else {
            if( g_opt.counter >= 2 ) {
                gen_increment( get_counter( edge, false ), inner_b );
            }
            gen_join_node( inner_b, next_edges );
            inner_b->builder.CreateBr( ret_b->bb );
        }
    };

    auto gen_inner_lookup_or_scan = [&](
        Value* index_ptr,
        char const* const lookup_name,
        char const* const next_name,
        rref_list<typed_term_tc> const args,
        rref<index_structure_tc> const structure,
        rref<index_info_tc> const inner_descr )
    {
        auto const next_func = [&]( 
            bb_context* inner_b, 
            bb_context* ret_b, 
            Value* next_base ) 
        { 
            gen_join_inner(
                edge, next_edges, lit, pidx, inner_descr, next_base, inner_b,
                negation_res_ptr, negation_landing );
            if( !inner_b->bb->getTerminator() ) {
                inner_b->builder.CreateBr( ret_b->bb );
            }
        };

        if( all_bound( args, *b ) ) {
            gen_lookup(
                Twine( edge.join_no ), index_ptr,
                lookup_name, false,
                structure, args, b,
                next_func );
        }
        else {
            gen_scan(
                Twine( edge.join_no ), &edge, index_ptr,
                next_name,
                structure, args, b,
                next_func );
        }
    };

    auto gen_value_scan = [&](
        rref_list<i32_tc> const args,
        uint32_t const offset )
    {
        gen_increment_scan_counter( edge, b );
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            offset );
        vector<Value*> val_ptrs;
        for( auto const arg : args | ad::indexed() ) {
            val_ptrs.push_back( b->builder.CreateInBoundsGEP(
                index_ptr,
                { get_i32_zero(), get_i32( (int32_t)arg.index() ) } ) );
        }

        auto* have_val_b = b->create_dom_block( "have_val" );
        auto* no_val_b = b->create_dom_block( "no_val" );
        auto* first_val = b->builder.CreateLoad( val_ptrs.front() );
        auto* cmp = b->builder.CreateICmpNE( first_val, get_invalid_idx() );
        b->builder.CreateCondBr( cmp, have_val_b->bb, no_val_b->bb );

        unpack_terms_from_alloca(
            val_ptrs,
            slice( lit->args, args ), 
            have_val_b, no_val_b );

        gen_next( have_val_b, no_val_b );
        b = no_val_b;
    };

    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.array_type );
        auto const args = cons( lit->args[x.arg] );
        gen_inner_lookup_or_scan( 
            index_ptr, "lookup_array_", "next_array_",
            args, 
            x.structure, x.descr );
    TYPE_CASE_X( hash_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.term_set_stable_type );
        auto const args = slice( lit->args, x.args );
        gen_inner_lookup_or_scan( 
            index_ptr, "lookup_hash_", "next_hash_",
            args, 
            x.structure, x.descr );
    TYPE_CASE_X( boolean )
        gen_increment_scan_counter( edge, b );
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.boolean_type );
        auto* val = b->builder.CreateLoad( index_ptr );
        auto* success = b->create_dom_block( "success" );
        auto* failure = b->create_dom_block( "failure" );
        auto* cmp = b->builder.CreateCondBr( val, success->bb, failure->bb );
        gen_next( success, failure );
        b = failure;
    TYPE_CASE_X( value )
        gen_value_scan( x.args, x.offset );
    TYPE_CASE_X( value_list )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.list_type );
        gen_scan(
            Twine( edge.join_no ), &edge, index_ptr,
            "next_list_",
            rref<index_structure_tc>(),
            slice( lit->args, x.args ),
            b,
            [&]( bb_context* inner_b, bb_context* ret_b, Value* ) 
            { gen_next( inner_b, ret_b ); } );
    TYPE_CASE_X( hash_set )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.hash_set_type );

        auto const args = slice( lit->args, x.args );
        if( all_bound( args, *b ) ) {
            gen_increment_scan_counter( edge, b );
            gen_lookup(
                Twine( edge.join_no ), index_ptr,
                "lookup_hash_set_",
                false,
                rref<index_structure_tc>(),
                args,
                b,
                [&]( bb_context* inner_b, bb_context* ret_b, Value* ) 
                { gen_next( inner_b, ret_b ); } );
        }
        else {
            gen_scan(
                Twine( edge.join_no ), &edge, index_ptr,
                "next_hash_set_",
                rref<index_structure_tc>(),
                args,
                b,
                [&]( bb_context* inner_b, bb_context* ret_b, Value* ) 
                { gen_next( inner_b, ret_b ); } );
        }
    TYPE_CASE_X( bitmap )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.bitmap_type );
        
        auto const args = cons( lit->args[x.arg] );
        if( all_bound( args, *b ) ) {
            gen_increment_scan_counter( edge, b );
            gen_lookup(
                Twine( edge.join_no ), index_ptr,
                "lookup_bitmap_",
                false,
                rref<index_structure_tc>(),
                args,
                b,
                [&]( bb_context* inner_b, bb_context* ret_b, Value* ) 
                { gen_next( inner_b, ret_b ); } );
        }
        else {
            gen_scan(
                Twine( edge.join_no ), &edge, index_ptr,
                "next_bitmap_",
                rref<index_structure_tc>(),
                args,
                b,
                [&]( bb_context* inner_b, bb_context* ret_b, Value* ) 
                { gen_next( inner_b, ret_b ); } );
        }
    TYPE_CASE_X( replace )
        gen_value_scan( x.args, x.offset );
    TYPE_CASE_X( join )
        gen_increment_scan_counter( edge, b );
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.objid32_type );
        auto* have_val_b = b->create_dom_block( "have_val" );
        auto* no_val_b = b->create_dom_block( "no_val" );
        auto* join_val = b->builder.CreateLoad( index_ptr );
        auto* cmp = b->builder.CreateICmpNE( join_val, get_invalid_idx() );
        b->builder.CreateCondBr( cmp, have_val_b->bb, no_val_b->bb );

        
        unpack( join_val, lit->args[x.arg], have_val_b, no_val_b );
        gen_next( have_val_b, no_val_b );
        b = no_val_b;
    TYPE_CASE_X( worklist )
        auto* const worklist_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, x.offset );
        auto* const index_ptr = b->builder.CreateStructGEP(
            cenv.worklist_type, worklist_ptr, 
            0 );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.list_type );
        gen_scan(
            Twine( edge.join_no ), &edge, index_ptr,
            "next_list_",
            rref<index_structure_tc>(),
            lit->args,
            b,
            [&]( bb_context* inner_b, bb_context* ret_b, Value* ) 
            { gen_next( inner_b, ret_b ); } );
    TYPE_CASE_X( direct )
        gen_increment_scan_counter( edge, b );
        auto* func = must_find( cenv.direct_functions, pidx->apred );

        vector<Value*> arg_val_ptrs;
        for( auto& arg : func->args() ) {
            auto* arg_ptr = b->fctxt.builder.CreateAlloca( arg.getType() );
            b->builder.CreateStore( &arg, arg_ptr );
            arg_val_ptrs.push_back( arg_ptr );
        }

        auto* exit = start_unification_block( b );
        unpack_terms_from_alloca(
            arg_val_ptrs,
            lit->args,
            b, exit );
        gen_next( b, exit );
        b = exit;
    TYPE_CASE_X( source )
        auto* open_func = check( cenv.md->getFunction( "open_file" ) );
        auto* file_context_type = get_type_from_ptr_arg( open_func );
        auto* file_context_ptr = b->fctxt.builder.CreateAlloca( 
            file_context_type );

        auto const types_ptr = b->fctxt.builder.CreateAlloca( 
            cenv.objid32_type,
            get_i32( (int32_t)lit->args.size() ) );
        for( auto const ty : x.types | ad::indexed( 0 ) ) {
            b->builder.CreateStore(
                get_i32( ty.value().get() ),
                b->builder.CreateInBoundsGEP( 
                    types_ptr, { get_i32( (int32_t)ty.index() ) } ) );
        }

        b->builder.CreateCall( 
            open_func, 
            { file_context_ptr, 
                get_str_ptr( x.file_name.str() ),
                get_str_ptr( "r" ),
                get_size_t( lit->args.size() ),
                types_ptr } );
        auto const join_no = edge.join_no;

        auto* loop_header = b->create_dom_block( 
            "#" + Twine( join_no ) + "_scan_header" );
        b->builder.CreateBr( loop_header->bb );

        auto const args_ptr = b->fctxt.builder.CreateAlloca( 
            cenv.objid32_type, get_i32( (int32_t)lit->args.size() ) );
        vector<Value*> arg_vals{ cenv.env, file_context_ptr, args_ptr };
        auto* const call = loop_header->builder.CreateCall( 
            check( cenv.md->getFunction( "scan_file" ) ), 
            arg_vals );

        auto* loop_body = loop_header->create_dom_block(
            "#" + Twine( join_no ) + "_scan_body" );
        auto* loop_exit = loop_header->create_dom_block(
            "#" + Twine( join_no ) + "_scan_exit" );
        loop_header->builder.CreateCondBr(
            call,
            loop_body->bb, loop_exit->bb );
        gen_increment_scan_counter( edge, loop_body );

        vector<Value*> arg_val_ptrs;
        for( size_t i = 0; i < lit->args.size(); ++i ) {
            arg_val_ptrs.push_back( loop_body->builder.CreateInBoundsGEP(
                args_ptr, { get_i32( (int32_t)i ) } ) );
        }
        unpack_terms_from_alloca(
            arg_val_ptrs,
            lit->args, 
            loop_body, loop_header );

        gen_next( loop_body, loop_header );

        loop_exit->builder.CreateCall( 
            check( cenv.md->getFunction( "close_file" ) ),
            { file_context_ptr } );
        b = loop_exit;
    TYPE_CASE_DIE( sink )
    TYPE_CASE_DIE( aggregate )
    TYPE_CASE_X( builtin )
        gen_increment_scan_counter( edge, b );
        if( x.op.is<builtin_pred_tc::equals>() ) {
            auto const num_bound = rg::count_if( 
                lit->args,
                [&]( auto const y ) { return is_bound( y, *b ); } );
            if( num_bound == 1 ) {
                auto bound_free = is_bound( lit->args[0], *b )
                    ? make_pair( lit->args[0], lit->args[1] )
                    : make_pair( lit->args[1], lit->args[0] );
                auto* val = pack_term( bound_free.first, b );

                auto* ret_b = start_unification_block( b );
                unpack( val, bound_free.second, b, ret_b );
                gen_next( b, ret_b );
                b = ret_b;
            }
            else if( num_bound == 2 ) {
                auto* left = pack_term( lit->args[0], b );
                auto* right = pack_term( lit->args[1], b );
                auto* success = b->create_dom_block( "success" );
                auto* failure = b->create_dom_block( "failure" );
                auto* cmp = b->builder.CreateICmpEQ( 
                    left, right );

                b->builder.CreateCondBr(
                    cmp,
                    success->bb,
                    failure->bb );

                gen_next( success, failure );
                b = failure;
            }
            else {
                die();
            }
        }
        else if( x.op.is<builtin_pred_tc::not_zero>() ) {
            my_assert( lit->args.size() == 1  
                && is_bound( lit->args[0], *b ) );
            auto* val = pack_term( lit->args[0], b );
            auto* success = b->create_dom_block( "success" );
            auto* failure = b->create_dom_block( "failure" );
            auto* cmp = b->builder.CreateICmpNE( val, get_i32( 0 ) );

            b->builder.CreateCondBr(
                cmp,
                success->bb,
                failure->bb );

            gen_next( success, failure );
            b = failure;
        }
        else {
            die();
        }
    END_TYPE_SWITCH_
}

auto gen_join(
    join_edge const& edge,
    join_edge_set const& next_edges,
    rref<typed_literal_tc> const lit,
    rref<predicate_index_tc> const pidx,
    rref<index_info_tc> const idx_info,
    Value* const base,
    bb_context*& b )
    -> void
{
    if( lit->negated.get() ) {
        auto negation_res_ptr = b->fctxt.builder.CreateAlloca(
            IntegerType::get( llctxt, 1 ) );
        auto negation_landing = b->create_dom_block( "neg_landing" );
        b->builder.CreateStore(
            get_i1( true ),
            negation_res_ptr );
        gen_join_inner(
            edge, next_edges, lit, pidx, idx_info, base, b,
            negation_res_ptr, negation_landing );
        b->builder.CreateBr( negation_landing->bb );
        auto* negation_res = negation_landing->builder.CreateLoad(
            negation_res_ptr );
        auto* neg_success = negation_landing->create_dom_block(
            "neg_success" );
        auto* neg_failure = negation_landing->create_dom_block(
            "neg_failure" );

        negation_landing->builder.CreateCondBr(
            negation_res,
            neg_success->bb,
            neg_failure->bb );
        if( g_opt.counter >= 2 ) {
            gen_increment( get_counter( edge, false ), neg_success );
        }
        gen_join_node( neg_success, next_edges );
        neg_success->builder.CreateBr( neg_failure->bb );
        b = neg_failure;
    }
    else {
        gen_join_inner(
            edge, next_edges, lit, pidx, idx_info, base, b,
            nullptr, nullptr );
    }
}

auto gen_insert(
    join_edge const& edge,
    rref<typed_literal_tc> const lit,
    rref<predicate_index_tc> const pidx,
    rref<index_info_tc> const idx_info,
    Value* const base,
    bb_context*& b )
    -> void;

auto gen_unique_insert(
    join_edge const& edge,
    join_edge_set const& next_edges,
    rref<typed_literal_tc> const lit,
    rref<predicate_index_tc> const pidx,
    rref<index_info_tc> const idx_info,
    Value* const base,
    bb_context*& b )
    -> void;

auto gen_insert_list_helper(
    Value* const index_ptr,
    rref_list<typed_term_tc> args_t,
    bb_context*& b )
{
    auto* const insert_func = get_vararg_function( 
        "insert_list_", args_t.size() );

    vector<Value*> args{ index_ptr };
    pack_terms( args_t, b, args );
    set_inline( 
        b->builder.CreateCall( insert_func, args ) );
}

auto gen_insert(
    join_edge const& edge,
    rref<typed_literal_tc> const lit,
    rref<predicate_index_tc> const pidx,
    rref<index_info_tc> const idx_info,
    Value* const base,
    bb_context*& b )
    -> void
{
    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.array_type );

        auto const args = cons( lit->args[x.arg] );
        auto const next_func = [&]( 
            bb_context* inner_b, bb_context* ret_b, Value* next_base )
        { 
            gen_insert( edge, lit, pidx, x.descr, next_base, inner_b ); 
            inner_b->builder.CreateBr( ret_b->bb );
        };

        gen_lookup(
            "insert", index_ptr, "lookup_insert_array_", true,
            x.structure, args, b,
            next_func );
    TYPE_CASE_X( hash_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.term_set_stable_type );

        auto const args = slice( lit->args, x.args );
        auto const next_func = [&]( 
            bb_context* inner_b, bb_context* ret_b, Value* next_base ) 
        { 
            gen_insert( edge, lit, pidx, x.descr, next_base, inner_b ); 
            inner_b->builder.CreateBr( ret_b->bb );
        };

        gen_lookup(
            "insert", index_ptr, "lookup_insert_hash_", true,
            x.structure, args, b,
            next_func );    
    TYPE_CASE_X( boolean )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.boolean_type );
        b->builder.CreateStore( get_i1( true ), index_ptr );
    TYPE_CASE_X( value )
        gen_unique_insert( edge, {}, lit, pidx, idx_info, base, b );
    TYPE_CASE_X( value_list )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.list_type );
        gen_insert_list_helper( index_ptr, slice( lit->args, x.args ), b );
    TYPE_CASE_X( hash_set )
        gen_unique_insert( edge, {}, lit, pidx, idx_info, base, b );
    TYPE_CASE_X( bitmap )
        gen_unique_insert( edge, {}, lit, pidx, idx_info, base, b );
    TYPE_CASE_X( replace )
        gen_unique_insert( edge, {}, lit, pidx, idx_info, base, b );
    TYPE_CASE_X( join )
        gen_unique_insert( edge, {}, lit, pidx, idx_info, base, b );
    TYPE_CASE_X( worklist )
        auto* const worklist_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        auto* const list_ptr = b->builder.CreateStructGEP(
            cenv.worklist_type, worklist_ptr, 
            1 );
        my_assert( 
            get_element_type( list_ptr->getType() ) == cenv.list_type );
        gen_insert_list_helper( list_ptr, lit->args, b );
    TYPE_CASE_X( direct )
        auto* func = must_find( cenv.direct_functions, pidx->apred );
        vector<Value*> arg_vals;
        pack_terms( lit->args, b, arg_vals );
        auto* const insert_call = b->builder.CreateCall( func, arg_vals );
    TYPE_CASE_DIE( source )
    TYPE_CASE_X( sink )
#if 0
        auto* const func = check( 
            cenv.md->getFunction( "write_formatted" ) );
        string fmtstr;
        for( auto const t : lit->args ) {
            if( !fmtstr.empty() ) {
                fmtstr += "\t";
            }
            fmtstr += "%" PRIu32;
        }
        fmtstr += "\n";
        vector<Value*> args{ 
            cenv.env, 
            get_i32( x.file_descriptor.get() ),
            get_str_ptr( fmtstr ) };
        pack_terms( lit->args, b, args );
        b->builder.CreateCall( func, args );
#else
        gen_set_show_fd( x.file_descriptor.get(), b );
        gen_write_literal( lit, b );
        gen_write_cstring( "\n", b );
#endif
    TYPE_CASE_X( aggregate )
        // TODO: check unique-ness
        auto* const val = pack_term( lit->args.head(), b );
        b->builder.CreateStore( val, check( b->fctxt.return_val ) );
    TYPE_CASE_DIE( builtin )
    END_TYPE_SWITCH_
}

auto gen_unique_insert_list_helper(
    Value* const index_ptr,
    char const* const func_name,
    rref_list<typed_term_tc> args_t,
    bb_context*& b,
    function< void( bb_context*&, bb_context* ) > const& func )
{    
    auto* const insert_func = check( cenv.md->getFunction( 
        ( func_name + Twine( args_t.size() ) ).str() ) );

    vector<Value*> args{ index_ptr };
    pack_terms( args_t, b, args );
    auto* const insert_call = set_inline(
        b->builder.CreateCall( insert_func, args ) );

    auto* insert_true = b->create_dom_block( "insert_true" );
    auto* insert_false = b->create_dom_block( "insert_false" );
    b->builder.CreateCondBr(
        insert_call,
        insert_true->bb, insert_false->bb );

    func( insert_true, insert_false );
    b = insert_false;
}

auto gen_unique_insert(
    join_edge const& edge,
    join_edge_set const& next_edges,
    rref<typed_literal_tc> const lit,
    rref<predicate_index_tc> const pidx,
    rref<index_info_tc> const idx_info,
    Value* const base,
    bb_context*& b )
    -> void
{
    auto gen_next = [&]( 
        bb_context* inner_b, bb_context* const ret_b )
        -> void
    {
        auto const* info = get_trace_info( edge );
        if( info ) {
            auto const fmt = bt::format( 
                "%sINSERT SUCCEEDED\n" )
                % indenter{ edge.join_no };
            gen_set_show_fd( 1, inner_b );
            gen_write_cstring( fmt.str(), inner_b );
        }

        if( g_opt.counter >= 1 ) {
            gen_increment( get_counter( pidx, true ), inner_b );
        }

        if( g_opt.counter >= 2 ) {
            gen_increment( get_counter( edge, false ), inner_b );
        }
        gen_join_node( inner_b, next_edges );
        inner_b->builder.CreateBr( ret_b->bb );
    };

    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.array_type );

        auto const args = cons( lit->args[x.arg] );
        auto const next_func 
            = [&]( bb_context* inner_b, bb_context* ret_b, Value* next_base ) 
        { 
            gen_unique_insert(
                edge, next_edges, lit, pidx, x.descr, next_base, inner_b );
            inner_b->builder.CreateBr( ret_b->bb );
        };

        gen_lookup(
            "uinsert", index_ptr, "lookup_insert_array_", true,
            x.structure, args, b,
            next_func );
    TYPE_CASE_X( hash_map )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.term_set_stable_type );

        auto const args = slice( lit->args, x.args );
        auto const next_func 
            = [&]( bb_context* inner_b, bb_context* ret_b, Value* next_base ) 
        { 
            gen_unique_insert(
                edge, next_edges, lit, pidx, x.descr, next_base, inner_b );
            inner_b->builder.CreateBr( ret_b->bb );
        };

        gen_lookup(
            "uinsert", index_ptr, "lookup_insert_hash_", true,
            x.structure, args, b,
            next_func );
    TYPE_CASE_X( boolean )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.boolean_type );
        auto* val = b->builder.CreateLoad( index_ptr );
        auto* insert_b = b->create_dom_block( "insert_true" );
        auto* no_insert_b = b->create_dom_block( "insert_false" );
        auto* cmp = b->builder.CreateCondBr( 
            val, no_insert_b->bb, insert_b->bb );
        insert_b->builder.CreateStore( get_i1( true ), index_ptr );
        gen_next( insert_b, no_insert_b );
        b = no_insert_b;
    TYPE_CASE_X( value )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );

        Value* same = get_i1( true );
        Value* no_val{ nullptr };
        for( auto const arg : slice( lit->args, x.args ) | ad::indexed() ) {
            auto* val_ptr = b->builder.CreateInBoundsGEP(
                index_ptr,
                { get_i32_zero(), get_i32( (int32_t)arg.index() ) } );

            auto* val = b->builder.CreateLoad( val_ptr );
            if( arg.index() == 0 ) {
                no_val = b->builder.CreateICmpEQ( val, get_invalid_idx() );
            }
            auto* new_val = pack_term( arg.value(), b );
            auto* cmp = b->builder.CreateICmpEQ( val, new_val );
            same = b->builder.CreateAnd( same, cmp );
            b->builder.CreateStore( new_val, val_ptr );
        }

        auto* no_violation_b = b->create_dom_block( "no_violation" );
        if( !g_opt.no_check ) {
            auto* violation_b = b->create_dom_block( "violation" );
            auto* no_violation = b->builder.CreateOr( no_val, same );
            b->builder.CreateCondBr( 
                no_violation, no_violation_b->bb, violation_b->bb );
            auto const fmt = bt::format( 
                "%s: Functional dependency violation while inserting '" )
                % edge.front_info().rule.loc;
            gen_set_show_fd( 2, violation_b );
            gen_write_cstring( fmt.str(), violation_b );
            gen_write_literal( lit, violation_b );
            gen_write_cstring( "'.\n", violation_b );
            gen_flush_file( 2, violation_b );
            violation_b->builder.CreateCall( cenv.exit_func, { get_i32( 1 ) } );
            violation_b->builder.CreateUnreachable();
        }
        else {
            b->builder.CreateBr( no_violation_b->bb );
        }

        auto* insert_b = no_violation_b->create_dom_block( "insert_true" );
        auto* no_insert_b = no_violation_b->create_dom_block( "insert_false" );
        no_violation_b->builder.CreateCondBr( 
            same, no_insert_b->bb, insert_b->bb );

        gen_next( insert_b, no_insert_b );
        b = no_insert_b;
    TYPE_CASE_X( value_list )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.list_type );
        gen_increment( get_counter( pidx, false ), b );
        gen_unique_insert_list_helper( 
            index_ptr,
            "unique_insert_list_",
            slice( lit->args, x.args ),
            b,
            [&]( bb_context*& inner_b, bb_context* ret_b )
            { gen_next( inner_b, ret_b ); } );
    TYPE_CASE_X( hash_set )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.hash_set_type );
        gen_increment( get_counter( pidx, false ), b );
        gen_unique_insert_list_helper( 
            index_ptr,
            "unique_insert_hash_set_",
            slice( lit->args, x.args ),
            b,
            [&]( bb_context*& inner_b, bb_context* ret_b )
            { gen_next( inner_b, ret_b ); } );
    TYPE_CASE_X( bitmap )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.bitmap_type );
        gen_increment( get_counter( pidx, false ), b );
        gen_unique_insert_list_helper( 
            index_ptr,
            "unique_insert_bitmap_",
            cons( lit->args[x.arg] ),
            b,
            [&]( bb_context*& inner_b, bb_context* ret_b )
            { gen_next( inner_b, ret_b ); } );
    TYPE_CASE_X( replace )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );

        Value* same = get_i1( true );
        for( auto const arg : slice( lit->args, x.args ) | ad::indexed() ) {
            auto* val_ptr = b->builder.CreateInBoundsGEP(
                index_ptr,
                { get_i32_zero(), get_i32( (int32_t)arg.index() ) } );

            auto* val = b->builder.CreateLoad( val_ptr );
            auto* new_val = pack_term( arg.value(), b );
            auto* cmp = b->builder.CreateICmpEQ( val, new_val );
            same = b->builder.CreateAnd( same, cmp );
            b->builder.CreateStore( new_val, val_ptr );
        }

        auto* insert_b = b->create_dom_block( "insert_true" );
        auto* no_insert_b = b->create_dom_block( "insert_false" );
        b->builder.CreateCondBr( 
            same, no_insert_b->bb, insert_b->bb );

        gen_next( insert_b, no_insert_b );
        b = no_insert_b;
    TYPE_CASE_X( join )
        auto* const index_ptr = b->builder.CreateStructGEP(
            get_element_type( base->getType() ), base, 
            x.offset );
        my_assert( 
            get_element_type( index_ptr->getType() ) == cenv.objid32_type );
        auto* insert_val = pack_term( lit->args[x.arg], b );
        auto* old_val = b->builder.CreateLoad( index_ptr );

        auto* have_val_b = b->create_dom_block( "have_val" );
        auto* no_val_b = b->create_dom_block( "no_val" );
        auto* new_val_b = b->create_dom_block( "new_val" );
        auto* finished_b = b->create_dom_block( "finished" );

        auto* cmp = b->builder.CreateICmpNE( old_val, get_invalid_idx() );
        b->builder.CreateCondBr( 
            cmp, have_val_b->bb, no_val_b->bb );

        // have val
        auto* join_func = gen_user_function( x.function );
        Value* joined_val = have_val_b->builder.CreateCall( 
            join_func, { old_val, insert_val } );
        auto* cmp2 = have_val_b->builder.CreateICmpNE( old_val, joined_val );
        have_val_b->builder.CreateStore( joined_val, index_ptr );
        have_val_b->builder.CreateCondBr( 
            cmp2, new_val_b->bb, finished_b->bb );

        // no val
        no_val_b->builder.CreateStore( insert_val, index_ptr );
        no_val_b->builder.CreateBr( new_val_b->bb );

        // new val
        new_val_b->term_map[ lit->args[x.arg] ]
            = new_val_b->builder.CreateLoad( index_ptr );
        gen_next( new_val_b, finished_b );

        // finished
        // reassign term to new value
        finished_b->term_map[ lit->args[x.arg] ]
            = finished_b->builder.CreateLoad( index_ptr );
        b = finished_b;
    TYPE_CASE_DIE( worklist )
    TYPE_CASE_DIE( direct )
    TYPE_CASE_DIE( source )
    TYPE_CASE_DIE( sink )
    TYPE_CASE_DIE( aggregate )
    TYPE_CASE_DIE( builtin )
    END_TYPE_SWITCH_
}

auto gen_join_edge(
    bb_context*& b,
    join_edge const& jedge )
    -> void
{
    auto const is_insertion 
        = jedge.type == join_edge::UINSERT
        || jedge.type == join_edge::INSERT;

    // Generate traces
    auto const* info = get_trace_info( jedge );
    if( info ) {
        if( jedge.type == join_edge::JOIN ) {
            auto const fmt = bt::format(
                "%sSTART SCAN %s AT %s\n" )
                % indenter{ jedge.join_no } 
                % jedge.index
                % info->rule.loc;
            gen_set_show_fd( 1, b );
            gen_write_cstring( fmt.str(), b );
        }
        else if( jedge.type == join_edge::FIX ) {
            auto const fmt = bt::format(
                "%sFIX {%s}\n" )
                % indenter{ jedge.join_no } 
                % delimited( *jedge.scc );
            gen_set_show_fd( 1, b );
            gen_write_cstring( fmt.str(), b );
        }
        else if( is_insertion ) {
            auto const fmt = bt::format(
                "%sINSERT INTO %s VALUE " )
                % indenter{ jedge.join_no } 
                % jedge.index;
            gen_set_show_fd( 1, b );
            gen_write_cstring( fmt.str(), b );
            gen_write_literal( jedge.index.lit, b );
            auto const fmt2 = bt::format(
                " AT %s\n" )
                % info->rule.loc;
            gen_write_cstring( fmt2.str(), b );
        }
    }

    // Do join
    if( jedge.type == join_edge::JOIN ) {
        gen_join(
            jedge,
            jedge.next,
            jedge.index.lit,
            jedge.index.pidx,
            jedge.index.pidx->idx_info,
            cenv.root,
            b );
    }
    else if( jedge.type == join_edge::FIX ) {
        auto* const fix_func = must_find( 
            cenv.scc_fix_function, jedge.scc );
        b->builder.CreateCall( fix_func, {} );
    }
    else if( jedge.type == join_edge::UINSERT ) {
        gen_unique_insert(
            jedge,
            jedge.next,
            jedge.index.lit,
            jedge.index.pidx,
            jedge.index.pidx->idx_info,
            cenv.root,
            b );
    }
    else if( jedge.type == join_edge::INSERT ) {
        my_assert( jedge.next.empty() );
        gen_insert(
            jedge,
            jedge.index.lit,
            jedge.index.pidx,
            jedge.index.pidx->idx_info,
            cenv.root,
            b );
        if( g_opt.counter >= 1 ) {
            gen_increment( get_counter( jedge.index.pidx, true ), b );
        }

    }
}

auto gen_join_node( 
    bb_context*& b,
    join_edge_set const& edges )
    -> void
{
    if( g_opt.counter >= 2 ) {
        gen_increment( get_counter( edges ), b );
    }

    for( join_edge const& jedge : edges ) {
        gen_join_edge( b, jedge );
    }
}

auto gen_root_join_node(
    bb_context*& b,
    string_ref const name,
    join_edge_set const& edges )
    -> void
{
    if( g_opt.counter >= 2 ) {
        gen_increment( get_counter( edges ), b );
    }

    unsigned i = 0;
    for( join_edge const& jedge : edges ) {        
        auto* func_type = FunctionType::get(
            Type::getVoidTy( llctxt ),
            vector<Type*>( 0, cenv.objid32_type ),
            false );

        auto* const func = set_noinline( Function::Create( 
            func_type, Function::InternalLinkage,
            Twine( sref( name ) ) + "@" + Twine( i ), cenv.md ) );

        function_context fctxt{ func };
        auto* start_b = fctxt.create_start_block();

        gen_join_edge( start_b, jedge );
        start_b->builder.CreateRetVoid();

        b->builder.CreateCall( func, {} );
        cenv.all_funcs.push_back( func );
        ++i;
    }
}

auto gen_print_join_edge(
    Value* const parent_val,
    join_edge const& e, 
    bb_context* const b ) 
    -> void
{
    if( e.type == join_edge::JOIN || e.type == join_edge::UINSERT ) {
        auto* unified_val = b->builder.CreateLoad( 
            get_counter( e, false ) );
        Type* dtype = Type::getDoubleTy( llctxt );
        auto* clamped = b->builder.CreateSelect(
            b->builder.CreateIsNull( parent_val ),
            ConstantInt::get( unified_val->getType(), 1 ),
            parent_val );
        auto* factor = b->builder.CreateFDiv( 
            b->builder.CreateUIToFP( unified_val, dtype ),
            b->builder.CreateUIToFP( clamped, dtype ) );
        auto const* const fmt = e.type == join_edge::JOIN
            ? "%12" PRIu64 "%12" PRIu64 "%12" PRIu64" (%2.0f) %s"
            : "%12" PRIu64 "%12s" "%12" PRIu64" (%2.0f) %s";
        gen_printf( fmt, b, 
            parent_val,
            e.type == join_edge::JOIN
                ? b->builder.CreateLoad( get_counter( e, true ) )
                : get_str_ptr( "---" ),
            unified_val,
            factor,
            get_str_ptr( bt::lexical_cast<string>( e ) ) );
    }
    else {
        gen_printf( "%12" PRIu64 "%12s %12s      %s", b, 
            parent_val,
            get_str_ptr( "---" ),
            get_str_ptr( "---" ),
            get_str_ptr( bt::lexical_cast<string>( e ) ) );
    }

    auto* this_parent_val = b->builder.CreateLoad( get_counter( e.next ) );
    for( auto const& next_e : e.next ) {
        gen_print_join_edge( this_parent_val, next_e, b );
    }
}

auto get_predicate_indices(
    join_edge const& e, set<rref<predicate_index_tc>>& pidxs )
    -> void
{
    if( e.type == join_edge::INSERT || e.type == join_edge::UINSERT ) {
        pidxs.insert( e.index.pidx );
    }
    for( auto const& next_e : e.next ) {
        get_predicate_indices( next_e, pidxs );
    }
}

auto get_predicate_indices(
    join_root const& r )
{
    set<rref<predicate_index_tc>> pidxs;
    for( auto const& e : r.edb_edges ) {
        get_predicate_indices( e, pidxs );
    }
    for( auto const& e : r.edges ) {
        get_predicate_indices( e, pidxs );
    }
    for( auto const& pr : r.direct_apreds ) {
        for( auto const& e : pr.second ) {
            get_predicate_indices( e, pidxs );
        }
    }
    return pidxs;
}

auto codegen( 
    string_ref const& name,
    size_t const num_args,
    join_edge_set const& edges ) 
    -> Function*
{
    auto* func_type = FunctionType::get(
        Type::getVoidTy( llctxt ),
        vector<Type*>( num_args, cenv.objid32_type ),
        false );

    auto* const func = check( Function::Create( 
        func_type, Function::InternalLinkage,
        sref( name ), cenv.md ) );

    function_context fctxt{ func };
    auto* start_b = fctxt.create_start_block();

    gen_root_join_node( start_b, name, edges );
    start_b->builder.CreateRetVoid();
    return func;
}

auto get_init_function( rref<index_structure_tc> const strt ) -> Function*
{
    static bt_unordered_map<Type*, Function*> const init_functions{
        { cenv.term_set_type, cenv.init_term_set_func },
        { cenv.term_set_stable_type, cenv.init_term_set_stable_func },
        { cenv.array_type, cenv.init_array_func },
        { cenv.list_type, cenv.init_list_func },
        { cenv.hash_set_type, cenv.init_hash_set_func },
        { cenv.bitmap_type, cenv.init_bitmap_func },
        { cenv.worklist_type, cenv.init_worklist_func }
    };

    auto*& func = cenv.init_functions[strt];
    if( func ) {
        return func;
    }

    auto* init_func_type = FunctionType::get(
        TypeBuilder<void,false>::get( llctxt ),
        { TypeBuilder<void*,false>::get( llctxt ) }, 
        false );

    auto* strt_type = get_structure_type( strt );
    func = Function::Create(
        init_func_type,
        Function::InternalLinkage,
        "init",
        cenv.md );
    cenv.all_funcs.push_back( func );
    function_context fctxt{ func };

    auto* strt_ptr = fctxt.builder.CreateBitCast(
        &at( func->args(), 0 ), 
        PointerType::getUnqual( strt_type ) );

    for( auto const node : strt->children | ad::indexed( 0 ) ) {
        auto ptr = fctxt.builder.CreateStructGEP(
            strt_type, strt_ptr,
            node.index() );
        auto* child_type = get_node_type( node.value() );
        if( child_type == cenv.boolean_type ) {
            fctxt.builder.CreateStore( get_i1( false ), ptr );
        }
        else if( child_type == cenv.objid32_type ) {
            fctxt.builder.CreateStore( get_invalid_idx(), ptr );
        }
        else if( child_type->isArrayTy() 
            && child_type->getArrayElementType() == cenv.objid32_type ) 
        {
            auto const num = child_type->getArrayNumElements();
            for( size_t i = 0; i < num; ++i ) {
                auto* inner_ptr = fctxt.builder.CreateInBoundsGEP(
                    ptr, { get_i32_zero(), get_i32( i ) } );
                fctxt.builder.CreateStore( get_invalid_idx(), inner_ptr );
            }
        }
        else {
            set_inline( fctxt.builder.CreateCall(
                must_find( init_functions, child_type ),
                { ptr } ) );
        }
    }
    fctxt.builder.CreateRetVoid();
    return func;
}

auto any_of_predicates(
    pred_scc_set const& scc,
    rref_set<predicate_decl_tc> const& set ) -> bool
{
    return ag::any_of( scc,
        [&]( rref<adorned_typed_predicate_tc> const apred ) 
        { return set_contains( set, apred->pred->decl ); } );
}

auto do_codegen() -> unique_ptr<Module>
{
    bool const dump_bc = !g_opt.bc_output_path.empty();

    auto const* const header_file_name = dump_bc
        ? "header" HEADER_SUFFIX "-N.bc"
        : "header" HEADER_SUFFIX ".bc"; 
    FILE* header_file = open_file( header_file_name );
    auto const header_fd = fileno( header_file );
    uint64_t const header_sz = lseek( header_fd, 0, SEEK_END );
    lseek( header_fd, 0, SEEK_SET );

    unique_ptr<MemoryBuffer> membuf = handle_error( 
        MemoryBuffer::getOpenFile( 
            header_fd, header_file_name, header_sz ) );
    fclose( header_file );
    unique_ptr<Module> md = handle_error(
        parseBitcodeFile( membuf->getMemBufferRef(), llctxt ) );
    cenv.md = md.get();
    auto const triple = sys::getProcessTriple();
    auto const triple_elf = Triple( triple + "-elf" );

    VERBOSE_PRINT( 1 )
        cout << "Compiling to target " << md->getTargetTriple() << "." << endl;

    cenv.init_term_set_func = check( md->getFunction( "init_term_set" ) );
    cenv.init_term_set_stable_func = check( md->getFunction( "init_term_set_stable" ) );
    cenv.init_array_func = check( md->getFunction( "init_array" ) );
    cenv.init_list_func = check( md->getFunction( "init_list" ) );
    cenv.init_hash_set_func = check( md->getFunction( "init_hash_set" ) );
    cenv.init_bitmap_func = check( md->getFunction( "init_bitmap" ) );
    cenv.init_worklist_func = check( md->getFunction( "init_worklist" ) );

    auto* swap_worklist_func = check( md->getFunction( "swap_worklist" ) );

    auto* next_list_func = check( md->getFunction( "next_list_0" ) );
    auto* next_array_func = check( md->getFunction( "next_array_1" ) );

    cenv.printf_func = check( md->getFunction( 
        dump_bc ? "printf" : "my_printf" ) );
    cenv.set_show_fd_func = check( cenv.md->getFunction( "set_show_fd" ) );
    cenv.show_cstring_func = check( cenv.md->getFunction( "show_cstring" ) );
    cenv.show_i32_func = check( cenv.md->getFunction( "show_i32" ) );
    cenv.show_string_func = check( cenv.md->getFunction( "show_string" ) );
    cenv.cat_string_func = check( md->getFunction( "cat_string" ) );
    cenv.match_string_func = check( md->getFunction( "match_string" ) );
    cenv.exit_func = check( cenv.md->getFunction( 
        dump_bc ? "exit" : "my_exit" ) );

    cenv.flush_file_func = check( cenv.md->getFunction( "flush_file" ) );
    cenv.reset_timer_func = check( cenv.md->getFunction( "reset_timer" ) );
    cenv.print_timer_func = check( cenv.md->getFunction( "print_timer" ) );

    cenv.objid32_type = TypeBuilder<uint32_t,false>::get( llctxt );
    cenv.size_type = TypeBuilder<size_t,false>::get( llctxt );
    cenv.boolean_type= Type::getInt1Ty( llctxt );
    
    cenv.term_set_type = get_type_from_ptr_arg( cenv.init_term_set_func );
    cenv.term_set_stable_type = get_type_from_ptr_arg( cenv.init_term_set_stable_func );
    cenv.array_type = get_type_from_ptr_arg( cenv.init_array_func );
    cenv.list_type = get_type_from_ptr_arg( cenv.init_list_func );
    cenv.hash_set_type = get_type_from_ptr_arg( cenv.init_hash_set_func );
    cenv.bitmap_type = get_type_from_ptr_arg( cenv.init_bitmap_func );
    cenv.worklist_type = get_type_from_ptr_arg( cenv.init_worklist_func );

    cenv.list_iter_type = get_type_from_ptr_arg( next_list_func, 1 );
    cenv.array_iter_type = get_type_from_ptr_arg( next_array_func, 1 );

    auto* init_env_func = check( md->getFunction( "init_env" ) );
    cenv.env_type = get_type_from_ptr_arg( init_env_func );
    cenv.env = new GlobalVariable{
        *md,
        cenv.env_type, false,
        GlobalValue::InternalLinkage,
        ConstantAggregateZero::get( cenv.env_type ), 
        "env" };

    auto const root_structure = must_find( 
        env.index_structures, construct<index_node_tc::root>() );
    cenv.root_type = cast<StructType>( get_structure_type( root_structure ) );
    cenv.root = new GlobalVariable{
        *md,
        cenv.root_type, false, 
        GlobalValue::InternalLinkage,
        ConstantAggregateZero::get( cenv.root_type ), 
        "root" };

    auto* main_func_type = FunctionType::get( 
        Type::getVoidTy( llctxt ), {}, false );

    auto* main_func = Function::Create( 
        main_func_type, 
        Function::ExternalLinkage,
        "main",
        md.get() );
    function_context main_fctxt{ main_func };
    auto* main_b = main_fctxt.create_start_block();

    // init environment
    main_b->builder.CreateCall( 
        init_env_func,
        cenv.env );

    for( size_t i = 3; i < env.file_names.size(); ++i ) {
        auto const file_name = fs::path( env.file_names[i].to_string() );
        auto const file_path = file_name.is_absolute() 
            ? file_name
            : ( g_opt.output_directory / file_name );
        main_b->builder.CreateCall(
            md->getFunction( "open_file_handle" ),
            { cenv.env, get_str_ptr( file_path.string() ) } );
    }

    // init strings
    auto* string_init_func = Function::Create(
        FunctionType::get( Type::getVoidTy( llctxt ), {}, false ),
        Function::InternalLinkage,
        "init_strings",
        cenv.md );
    cenv.all_funcs.push_back( string_init_func );
    main_b->builder.CreateCall( string_init_func, {} );

    // create init functions
    for( auto const strt : env.index_structures | ad::map_values ) {
        get_init_function( strt );
    }

    // init root
    auto* root_void_ptr = main_b->builder.CreateBitCast(
        cenv.root,
        TypeBuilder<void*,false>::get( llctxt ) );
    main_b->builder.CreateCall(
        must_find( cenv.init_functions, root_structure ),
        { root_void_ptr } );

    // create aggregate functions
    for( auto const& pr : env.aggregate_join_edges ) {
        auto const& x = pr.first.get<typed_term_tc::aggregate>();
        auto const& jedge = pr.second;

        auto const num_args = x.input.size();
        auto* func_type = FunctionType::get(
            cenv.objid32_type,
            vector<Type*>( num_args, cenv.objid32_type ),
            false );
        auto* func = cenv.direct_functions[x.apred] = Function::Create( 
            func_type, Function::InternalLinkage,
            sref( x.apred->pred->decl->name ),
            cenv.md );

        function_context fctxt{ func };
        auto* start_b = fctxt.create_start_block();
        fctxt.return_val = fctxt.builder.CreateAlloca(
            cenv.objid32_type, nullptr, 
            "ret" );
        start_b->builder.CreateStore( get_i32( -1 ), fctxt.return_val );
        gen_join_edge( start_b, jedge );
        start_b->builder.CreateRet( 
            start_b->builder.CreateLoad( fctxt.return_val ) );
        cenv.all_funcs.push_back( func );
    }


    int jrootidx = 0;
    for( auto const& jroot : env.join_roots ) {
        bool const print_scc = g_opt.print_all_timings
            || any_of_predicates( jroot.scc, env.print_scc_preds );

        // initialize direct functions
        for( auto const apred : jroot.direct_apreds | ad::map_keys ) {
            auto const num_args = apred->pred->types.size();
            auto* func_type = FunctionType::get(
                Type::getVoidTy( llctxt ),
                vector<Type*>( num_args, cenv.objid32_type ),
                false );

            cenv.direct_functions[apred] = Function::Create( 
                func_type, Function::InternalLinkage,
                sref( apred->pred->decl->name ), cenv.md );
        }
        // generate direct functions
        for( auto const& pr : jroot.direct_apreds ) {
            auto* func = must_find( cenv.direct_functions, pr.first );
            function_context fctxt{ func };
            auto* start_b = fctxt.create_start_block();

            gen_join_node( start_b, pr.second );
            start_b->builder.CreateRetVoid();
            cenv.all_funcs.push_back( func );
        }

        if( jroot.edb_edges.empty() && jroot.edges.empty()
            && jroot.before_edges.empty() && jroot.after_edges.empty() ) 
        {
            continue;
        }

        if( !jroot.before_edges.empty() ) {
            auto const func_name = "scc_before@" + to_string( jrootidx );
            auto* func = codegen( func_name, 0, jroot.before_edges );
            cenv.all_funcs.push_back( func );
            main_b->builder.CreateCall( func, {} );
        }

        if( g_opt.print_all_timings || print_scc ) {
            stringstream scc_name;
            scc_name << bt::format(
                "Computing scc #%s: %s...\n" )
                % jrootidx
                % delimited( jroot.scc );
            gen_printf( scc_name.str(), main_b );
        }

        if( g_opt.print_all_timings || jrootidx == 0 ) {
            main_b->builder.CreateCall( cenv.reset_timer_func, {} );
        }

        if( !jroot.edb_edges.empty() ) {
            auto const edb_func_name = "scc_edb@" + to_string( jrootidx );
            auto* edb_func = codegen( edb_func_name, 0, jroot.edb_edges );
            set_noinline( edb_func );
            cenv.all_funcs.push_back( edb_func );
            main_b->builder.CreateCall( edb_func, {} );
        }

        if( !jroot.edges.empty() ) {
            auto const func_name = "scc@" + to_string( jrootidx );
            auto* delta_func = codegen( func_name, 0, jroot.edges );
            cenv.all_funcs.push_back( delta_func );

            auto* fix_func_type = FunctionType::get(
                Type::getVoidTy( llctxt ),
                vector<Type*>{},
                false );
            auto* const fix_func = Function::Create( 
                fix_func_type, Function::InternalLinkage,
                "fix_" + func_name, cenv.md );
            set_noinline( fix_func );
            cenv.all_funcs.push_back( fix_func );

            function_context fctxt{ fix_func };
            Value* sz = get_size_t( 0 );

            auto* loop_header = fctxt.create_self_dom_block();
            fctxt.builder.CreateBr( loop_header->bb );

            for( auto const apred : jroot.worklist_apreds ) {
                auto const worklist = env.worklist_index.at( apred );
                auto const& idx_info 
                    = worklist->idx_info.get<index_info_tc::worklist>();

                auto worklist_ptr = loop_header->builder.CreateStructGEP(
                    cenv.root_type, cenv.root, idx_info.offset );
                auto* worklist_sz = loop_header->builder.CreateCall( 
                    swap_worklist_func, { worklist_ptr } );
                sz = loop_header->builder.CreateAdd( sz, worklist_sz );            
            }

            auto* cmp = loop_header->builder.CreateICmpNE( 
                sz, get_size_t( 0 ) );
            auto* loop_body = loop_header->create_dom_block( "loop_body" );
            auto* loop_end = loop_header->create_dom_block( "loop_end" );
            loop_header->builder.CreateCondBr( 
                cmp, loop_body->bb, loop_end->bb );

            loop_body->builder.CreateCall( delta_func, {} );
            loop_body->builder.CreateBr( loop_header->bb );

            loop_end->builder.CreateRetVoid();
            main_b->builder.CreateCall( fix_func, {} );
            cenv.scc_fix_function.insert( { &jroot.scc, fix_func } );
        }

        if( g_opt.print_all_timings ) {
            main_b->builder.CreateCall( cenv.print_timer_func, {} );
            gen_printf( "\n", main_b );
        }

        if( !jroot.after_edges.empty() ) {
            auto const func_name = "scc_after@" + to_string( jrootidx );
            auto* func = codegen( func_name, 0, jroot.after_edges );
            cenv.all_funcs.push_back( func );
            main_b->builder.CreateCall( func, {} );
        }

        // Print counters
        if( g_opt.counter >= 1 ) {
            auto* const sumi = main_b->fctxt.builder.CreateAlloca( 
                Type::getInt64Ty( llctxt ) );
            main_b->builder.CreateStore( get_i64( 0 ), sumi );
            auto* const sums = main_b->fctxt.builder.CreateAlloca( 
                Type::getInt64Ty( llctxt ) );
            main_b->builder.CreateStore( get_i64( 0 ), sums );

            auto const pidxs = get_predicate_indices( jroot );
            auto const pidxs_sz = pidxs.size();
            if( pidxs_sz > 1 ) {
                gen_printf( "\n", main_b );
            }
            for( auto const pidx : pidxs ) {
                auto* vali = main_b->builder.CreateLoad( 
                    get_counter( pidx, false ) );
                auto* vals = main_b->builder.CreateLoad( 
                    get_counter( pidx, true ) );
                gen_printf( "%12" PRIu64 ":%12" PRIu64 " %s\n", 
                    main_b, vali, vals, 
                    get_str_ptr( bt::lexical_cast<string>( pidx ) ) );
                main_b->builder.CreateStore(
                    main_b->builder.CreateAdd( 
                        main_b->builder.CreateLoad( sumi ), vali ),
                    sumi );
                main_b->builder.CreateStore(
                    main_b->builder.CreateAdd( 
                        main_b->builder.CreateLoad( sums ), vals ),
                    sums );
            }
            if( pidxs_sz > 1 ) {
                gen_printf( "%12" PRIu64 ":%12" PRIu64 " Total\n\n", 
                    main_b, 
                    main_b->builder.CreateLoad( sumi ),
                    main_b->builder.CreateLoad( sums ) ); 
            }
        }
        if( g_opt.counter >= 2 ) {
            for( auto const& e : jroot.edb_edges ) {
                gen_print_join_edge( get_i64( 1 ), e, main_b );
                gen_printf( "\n", main_b );
            }
            auto* wl_counter = main_b->builder.CreateLoad( 
                get_counter( jroot.edges ) );
            for( auto const& e : jroot.edges ) {
                gen_print_join_edge( wl_counter, e, main_b );
                gen_printf( "\n", main_b );
            }
            for( auto const& pr : jroot.direct_apreds ) {
                auto* direct_counter = main_b->builder.CreateLoad( 
                    get_counter( pr.second ) );
                for( auto const& e : pr.second ) {
                    gen_print_join_edge( direct_counter, e, main_b );
                    gen_printf( "\n", main_b );
                }
            }
        }
        jrootidx += 1;
    }


    main_b->builder.CreateCall(
        md->getFunction( "destroy_env" ),
        { cenv.env } );
    main_b->builder.CreateRet( nullptr );
    cenv.all_funcs.push_back( main_func );
    cenv.main_func = main_func;

    // Emplace strings
    {
        function_context fctxt{ string_init_func };
        auto* put_string_func = check( md->getFunction( "put_string" ) );

        auto const sz = get_term_set_size<interned_string_tc>();
        for( size_t i = 0; i < sz; ++i ) {
            auto const str = get_data<interned_string_tc>( i ).value.str();
            auto* const str_ptr = get_str_ptr( str );
            fctxt.builder.CreateCall(
                put_string_func,
                { cenv.env, str_ptr, get_size_t( str.size() ) } );
        }
        fctxt.builder.CreateRetVoid();
    }
    
    // Print unoptimized
    if( g_opt.print_before_opt ) {
        outs() << 
            "*******************************************\n"
            "*************** UNOPTIMIZED ***************\n"
            "*******************************************\n";

        auto sorter = []( auto* l, auto* r ) 
            { return l->getName() < r->getName(); };
        auto structs = md->getIdentifiedStructTypes();
        rg::sort( structs, sorter );
        for( auto* t : structs ) {
            outs() << *t << '\n';
        }

        vector<GlobalVariable*> globals;
        for( auto& g : md->getGlobalList() ) { globals.push_back( &g ); }
        rg::sort( globals, sorter );
        for( auto const& g : globals ) {
            outs() << *g << '\n';
        }

        for( auto* const func : cenv.all_funcs ) {
            outs() << *func;
        }
    }
    outs().flush();

    // Code generation finished
    // Verify functions
    vector<string> all_funcs_name;
    for( auto* const func : cenv.all_funcs ) {
        all_funcs_name.push_back( func->getName() );
        if( verifyFunction( *func, &outs() ) ) {
            exit( 1 );
        }
    }

    if( g_opt.opt_level > 0 ) {
        legacy::PassManager pm;
        legacy::FunctionPassManager fpm( md.get() );
        PassManagerBuilder pmb;
        pmb.OptLevel = g_opt.opt_level;
        pmb.Inliner = createFunctionInliningPass( 
            max( g_opt.opt_level, g_opt.inline_level ), 
            0,
			false );
        pmb.populateModulePassManager( pm );
        pmb.populateFunctionPassManager( fpm );

        fpm.doInitialization();
        pm.run( *md );
        for( auto& func : md->functions() ) {
            fpm.run( func );
        }
        fpm.doFinalization();

        if( g_opt.print_after_opt ) {
            outs() << 
                "*******************************************\n"
                "**************** OPTIMIZED ****************\n"
                "*******************************************\n";
            for( auto const name : all_funcs_name ) {
                auto* func = md->getFunction( name );
                if( func ) {
                    outs() << *func;
                }
                else {
                    outs() << name << " optimized out.\n\n";
                }
            }
        }
    }
    outs().flush();

    if( !g_opt.llvm_output_path.empty() ) {
        error_code llvm_outs_ec;
        raw_fd_ostream llvm_outs{ 
            g_opt.llvm_output_path.string().c_str(),
            llvm_outs_ec, 
            sys::fs::F_None };
        if( llvm_outs.has_error() ) {
            cerr << bt::format( "Unable to open output file '%s'.\n" )
                % g_opt.llvm_output_path;
            exit( 1 );
        }
        llvm_outs << *md;
    }
    return move( md );
}

auto codegen() -> void
{
    llvm_shutdown_obj lshutdown;  // Call llvm_shutdown() on exit.
    auto const compile_start_time = chrono::high_resolution_clock::now();
#if defined(_WIN32)
    win_heap = HeapCreate( HEAP_NO_SERIALIZE, 64*(1<<20), 0 );
#endif
    unique_ptr<Module> md;
    
    auto const exec_mtime = fs::exists( g_opt.exec_path )
        ? fs::last_write_time( g_opt.exec_path )
        : time(nullptr);
    string cache_str = to_string( exec_mtime );
    for( auto const& filepath : g_opt.read_files ) {
        auto const& filepath_str = filepath.string();
        auto const filepath_mtime = fs::exists( filepath )
            ? fs::last_write_time( filepath )
            : time(nullptr);
        cache_str += "-" + filepath.filename().string()
            + "-" + to_string( filepath_mtime );
    }
    cache_str += "-O" + to_string( g_opt.opt_level ) + ".cache";
    fs::path cache_filepath = fs::temp_directory_path() / cache_str;
    auto const cache_filepath_str = cache_filepath.string();
    FILE* cache_file = fopen( cache_filepath_str.c_str(), "rb" );
    if( cache_file ) {
        VERBOSE_PRINT( 1 )
            cout << bt::format( "Using cache file '%s'.\n" )
                % cache_filepath
                << flush;
        auto const cache_fd = fileno( cache_file );
        uint64_t const cache_sz = lseek( cache_fd, 0, SEEK_END );
        lseek( cache_fd, 0, SEEK_SET );

        unique_ptr<MemoryBuffer> membuf = handle_error( 
            MemoryBuffer::getOpenFile( 
                cache_fd, cache_filepath_str.c_str(), cache_sz ) );
        fclose( cache_file );
        md = handle_error(
            parseBitcodeFile( membuf->getMemBufferRef(), llctxt ) );
        cenv.main_func = check( md->getFunction( "main" ) );
    }
    else {
        md = do_codegen();
        error_code bc_outs_ec;
        raw_fd_ostream bc_outs{ 
            cache_filepath.string().c_str(),
            bc_outs_ec, 
            sys::fs::F_None };
        if( bc_outs.has_error() ) {
            cerr << bt::format( "Unable to open cache file '%s'.\n" )
                % cache_filepath;
            exit( 1 );
        }
        WriteBitcodeToFile( md.get(), bc_outs );
    }

    bool const dump_bc = !g_opt.bc_output_path.empty();
    if( dump_bc ) {
        my_assert( !g_opt.bc_output_path.empty() );
        error_code bc_outs_ec;
        raw_fd_ostream bc_outs{ 
            g_opt.bc_output_path.string().c_str(),
            bc_outs_ec, 
            sys::fs::F_None };
        if( bc_outs.has_error() ) {
            cerr << bt::format( "Unable to open output file '%s'.\n" )
                % g_opt.bc_output_path;
            exit( 1 );
        }
        WriteBitcodeToFile( md.get(), bc_outs );
    }
    else {
        string error;
        EngineBuilder ebuilder( move( md ) );
        ebuilder.setErrorStr( &error );
        ebuilder.setEngineKind( EngineKind::JIT );
        ebuilder.setRelocationModel( Reloc::Static );
        ebuilder.setOptLevel( 
            g_opt.opt_level > 0 ? CodeGenOpt::Default : CodeGenOpt::None );
        LLVMInitializeNativeTarget();
        LLVMInitializeNativeAsmPrinter();
        auto* ee = ebuilder.create();
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fopen", (uint64_t)&fopen );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fclose", (uint64_t)&fclose );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fgets", (uint64_t)&fgets );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fputs", (uint64_t)&fputs );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_printf", (uint64_t)&printf );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fprintf", (uint64_t)&fprintf );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_vfprintf", (uint64_t)&vfprintf );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fwrite", (uint64_t)&fwrite );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_fflush", (uint64_t)&fflush );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_exit", (uint64_t)&my_exit );
        ee->addGlobalMapping( 
            SYMBOL_PREFIX "high_performance_reset_timer",
            (uint64_t)&high_performance_reset_timer );
        ee->addGlobalMapping( 
            SYMBOL_PREFIX "high_performance_print_timer",
            (uint64_t)&high_performance_print_timer );
        ee->addGlobalMapping( 
            SYMBOL_PREFIX "high_performance_log_timer",
            (uint64_t)&high_performance_log_timer );
        ee->addGlobalMapping( SYMBOL_PREFIX "find_file", (uint64_t)&find_file );
#if defined(_WIN32)
        ee->addGlobalMapping( SYMBOL_PREFIX "my_malloc", (uint64_t)&my_malloc );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_realloc", (uint64_t)&my_realloc );
        ee->addGlobalMapping( SYMBOL_PREFIX "my_free", (uint64_t)&my_free );
#endif
        if( !ee ) {
            cerr << error << endl;
            exit( 1 );
        }
        ee->finalizeObject();

        if( g_opt.verbose >= 1 ) {
            chrono::duration<double> const elapsed = 
                chrono::high_resolution_clock::now() - compile_start_time;

            size_t vm_usage, resident_set;
            tie( vm_usage, resident_set ) = process_mem_usage();
            cout << bt::format( "Code generation finished in %.6fs. %d MiB.\n" )
                % elapsed.count()
                % ( resident_set / 1024 / 1024 );
        }
        cout.flush();
        cerr.flush();

        auto* main_func_p = (void(*)(char const*[]))ee->getPointerToFunction( 
            cenv.main_func );
        vector<char const*> include_dirs;
        deque<string> include_dir_strings;
        for( auto const& dir : g_opt.include_directories ) {
            include_dir_strings.push_back( dir.string() );
            include_dirs.push_back( include_dir_strings.back().c_str() );
        }
        include_dirs.push_back( nullptr );
        (*main_func_p)( include_dirs.data() );
        fflush( stdout );
        fflush( stderr );

        if( g_opt.verbose >= 1 ) {
            size_t vm_usage, resident_set;
            tie( vm_usage, resident_set ) = process_mem_usage();
            printf( "%d MiB.\n", (unsigned)( resident_set / 1024 / 1024 ) );
            fflush( stdout );
        }
    }
}

}
