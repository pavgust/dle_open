#include "stdafx.h"
#include "utility.h"
#include "rilparser.h"
#include "factgen.h"

vector<rref<function_tc>> functions;
bt_unordered_map<rref<string_tc>,rref<def_tc>> globals;

struct value
{
    rref<def_tc> def;
    vector<uint32_t> offsets;
    bool lvalue;
};

struct file_location
{
    rref_str func;
    rref<location_tc> loc;
};

auto operator<<( ostream& o, file_location const& x ) -> ostream&
{
    return o << bt::format( "%s(%s:%s)" ) 
        % x.func 
        % x.loc->first_line
        % x.loc->first_column;
}

auto convert_to_rvalue( 
    value const& x,
    uint32_t const line_no,
    block& blk,
    function_context& fctxt )
    -> value
{
    if( !x.lvalue ) {
        return x;
    }
    return value{
        fctxt.construct_def<def_tc::load>(
            fctxt.func,
            blk.num,
            line_no,
            x.def ),
        {},
        false };
}

auto process_expression(
    rref<expression_tc> const expr,
    block& current_block,
    function_context& fctxt )
    -> value
{
    switch( expr.get_tag() ) {
    case expression_tc::IDENTIFIER:
    {
        auto const& x = expr.get<expression_tc::identifier>();
        rref<def_tc> def;
        auto iter = fctxt.allocs.find( x.name );
        if( iter != fctxt.allocs.end() ) {
            def = iter->second;
        }

        if( !def ) {
            iter = globals.find( x.name );
            if( iter != globals.end() ) {
                def = iter->second;
            }
        }

        if( !def ) {
            cerr << bt::format(
                "%s: Undeclared identifier '%s'.\n" )
                % file_location{ fctxt.func, x.loc }
                % x.name;
            exit( 1 );
        }
        return value{ 
            def,
            {},
            true };
    }
    case expression_tc::INTEGER:
    die();
    break;
    case expression_tc::ALLOCATION:
    {
        auto const& x = expr.get<expression_tc::allocation>();
        return value{
            fctxt.construct_def<def_tc::alloc>(
                fctxt.func,
                fresh_block( current_block, fctxt ),
                x.loc->first_line,
                rref_str( to_string( current_block.num ) ) ),
            {},
            false };
    }
    break;
    case expression_tc::DEREFERENCE:
    {
        auto const& x = expr.get<expression_tc::dereference>();
        value inner = process_expression( x.inner, current_block, fctxt );
        auto const innerval = convert_to_rvalue( 
            inner, 
            x.loc->first_line, 
            current_block, 
            fctxt );
        return value{
            innerval.def,
            {},
            true };
    }
    break;
    case expression_tc::ADDRESS_OF:
    {
        auto const& x = expr.get<expression_tc::address_of>();
        value inner = process_expression( x.inner, current_block, fctxt );
        if( !inner.lvalue ) {
            cerr << bt::format( "%s: Can't take the address of a rvalue.\n" )
                % file_location{ fctxt.func, x.loc };
            exit( 1 );
        }
        return value{
            inner.def,
            {},
            false };
    }
    break;
    case expression_tc::ASSIGNMENT:
    {
        auto const& x = expr.get<expression_tc::assignment>();
        value lhs = process_expression( x.lhs, current_block, fctxt );
        if( !lhs.lvalue ) {
            cerr << bt::format( "%s: Can't assign to a rvalue.\n" )
                % file_location{ fctxt.func, x.loc };
            exit( 1 );
        }
        value rhs = process_expression( x.rhs, current_block, fctxt );
        auto const rhsval = convert_to_rvalue( 
            rhs,
            x.loc->first_line,
            current_block,
            fctxt );
        fctxt.construct_store( 
            fctxt.func,
            fresh_block( current_block, fctxt ),
            x.loc->first_line,
            lhs.def,
            rhsval.def );
        return lhs;
    }
    break;
    case expression_tc::INVOKE:
    {
        auto const& x = expr.get<expression_tc::invoke>();
        value tgt = process_expression( x.target, current_block, fctxt );
        if( !tgt.lvalue ) {
            cerr << bt::format( 
                "%s: Function call target must be an "
                "lvalue of a function.\n" )
                % file_location{ fctxt.func, x.loc };
            exit( 1 );
        }

        auto const argvals = transform_to_list(
            x.args,
            [&]( rref<expression_tc> const arg )
            {
                return convert_to_rvalue(
                    process_expression( arg, current_block, fctxt ),
                    x.loc->first_line,
                    current_block,
                    fctxt ).def;
            } );
        return value{
            fctxt.construct_def<def_tc::invoke>(
                fctxt.func,
                fresh_block( current_block, fctxt ),
                x.loc->first_line,
                tgt.def,
                argvals ),
            {},
            false };
    }
    break;
    case expression_tc::POINTER_OFFSET:
    {
        auto const& x = expr.get<expression_tc::pointer_offset>();
        value ptr = process_expression( x.ptr, current_block, fctxt );
        if( !ptr.lvalue ) {
            cerr << bt::format( 
                "%s: Left-hand side of a member access must be a lvalue.\n" )
                % file_location{ fctxt.func, x.loc };
            exit( 1 );
        }
        ptr.offsets.push_back( x.offset );
        return ptr;
    }
    break;
    default:
    die();
    }
}

auto process_statement(
    rref<statement_tc> const stmt,
    block& current_block,
    function_context& fctxt )
    -> void
{
    switch( stmt.get_tag() ) {
    case statement_tc::VARIABLE_DECL:
    {
        auto const& x = stmt.get<statement_tc::variable_decl>();
        for( auto const name : x.names ) {
            fctxt.allocs[name] = fctxt.construct_def<def_tc::alloc_a>(
                fctxt.func,
                current_block.num,
                name );
        }
        fresh_block( current_block, fctxt );
    }
    break;
    case statement_tc::LABEL:
    die();
    break;
    case statement_tc::IF_:
    {
        auto const& x = stmt.get<statement_tc::if_>();
        auto true_b = fctxt.new_block();
        add_edge( fctxt, current_block, true_b );
        process_statement( x.true_branch, true_b, fctxt );
        auto const next_b = fctxt.new_block();
        add_edge( fctxt, true_b, next_b );

        if( !x.false_branch.is<statement_tc::empty>() ) {
            auto false_b = fctxt.new_block();
            add_edge( fctxt, current_block, false_b );
            process_statement( x.false_branch, false_b, fctxt );
            add_edge( fctxt, false_b, next_b );
        }
        else {
            add_edge( fctxt, current_block, next_b );
        }
        current_block = next_b;
    }
    break;
    case statement_tc::WHILE_:
    {
        auto const& x = stmt.get<statement_tc::while_>();
        auto header_b = fctxt.new_block();
        add_edge( fctxt, current_block, header_b );
        auto body_b = fctxt.new_block();
        auto const next_b = fctxt.new_block();
        add_edge( fctxt, header_b, body_b );
        add_edge( fctxt, header_b, next_b );
        process_statement( x.body, body_b, fctxt );
        add_edge( fctxt, body_b, header_b );
        current_block = next_b;
    }
    break;
    case statement_tc::DO_:
    {
        auto const& x = stmt.get<statement_tc::do_>();
        auto header_b = fctxt.new_block();
        auto body_b = header_b;
        add_edge( fctxt, current_block, body_b );
        process_statement( x.body, body_b, fctxt );

        auto const next_b = fctxt.new_block();
        add_edge( fctxt, body_b, header_b );
        add_edge( fctxt, body_b, next_b );
        current_block = next_b;
    }
    break;
    case statement_tc::EXPRESSION:
    {
        auto const& x = stmt.get<statement_tc::expression>();
        auto const val = process_expression( x.expr, current_block, fctxt );
    }
    break;
    case statement_tc::BLOCK:
    {
        auto const& x = stmt.get<statement_tc::block>();
        for( auto const stmt : x.stmts ) {
            process_statement( stmt, current_block, fctxt );
        }
    }
    break;
    case statement_tc::RETURN_:
    {
        auto const& x = stmt.get<statement_tc::return_>();
        auto const val = process_expression( x.expr, current_block, fctxt );
        auto const rval = convert_to_rvalue(
            val, 
            x.loc->first_line, 
            current_block, 
            fctxt );
        fctxt.returns.push_back( { rval.def, current_block.num } );
    }
    break;
    case statement_tc::EMPTY:
    break;
    default: abort();
    }
}

auto ril_read( fs::path const& file_path )
{
    {
        FILE* in = fopen( file_path.string().c_str(), "rb" );
        if( !in ) {
            cerr << bt::format( 
                "Error opening %1%.\n" )
                % file_path;
            exit( 1 );
        }
        yyscan_t scanner;
        rillex_init( &scanner );
        rilset_in( in, scanner );
        rilset_debug( g_opt.yy_debug, scanner );
        if( rilparse( scanner ) ) {
            cerr << "Syntax error." << endl;
            exit( -1 );
        }
        fclose( in );
        rillex_destroy ( scanner );
    }

    if( !fs::exists( g_opt.output_directory ) ) {
        try {
            fs::create_directory( g_opt.output_directory );
        }
        catch( fs::filesystem_error const& e ) {
            cerr << e.what() << endl;
            exit( 1 );
        }
    }

    for( rref<function_tc> const func : functions ) {
        g_ctxt.fctxts.emplace_back( func->name );
        function_context& fctxt = g_ctxt.fctxts.back();
        auto current_block = fctxt.new_block();

        for( auto const arg_def : func->args ) {
            auto const arg = arg_def.get<def_tc::argument>();
            rref<def_tc> arga = fctxt.construct_def<def_tc::alloc_a>(
                func->name,
                current_block.num,
                arg.name );
            fresh_block( current_block, fctxt );
            fctxt.construct_store( 
                func->name,
                current_block.num,
                func->loc->first_line,
                arga,
                arg_def );
            fctxt.allocs[arg.name] = arga;
            fresh_block( current_block, fctxt );
        }

        for( rref<statement_tc> const stmt : func->body ) {
            process_statement( stmt, current_block, fctxt );
        }
        fctxt.order_blocks();
    }

    write_tables();
}
