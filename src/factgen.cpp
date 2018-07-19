#include "stdafx.h"
#include "utility.h"
#include "rilparser.h"
#include "factgen.h"

options g_opt;
tables g_output;
global_context g_ctxt;

template< typename T >
using rts = root_term_store<T>;

auto construct_global( rref_str const name ) -> rref<def_tc>
{
    return g_ctxt.construct_global( name );
}

auto llvm_read( fs::path const& file_path ) -> void;
auto ril_read( fs::path const& file_path ) -> void;

auto main( int argc,  char* argv[] ) 
    -> int
{
    namespace po = boost::program_options;
    po::options_description optdesc(
        "Usage: rlog [option...] <input-file>" );
    optdesc.add_options()
        ( "help,h", "produce help message" )
        ( "input-file",
            po::value<string>(),
            "input file" )
        ( "output-dir", 
            po::value<string>()->default_value( "output/" ),
            "output directory")
        ( "verbose,v", 
            po::value<int>()->implicit_value(1)->default_value(0),
            "verbosity level" )
        ( "yydebug", "enable parser debug mode" )
        ;

    po::positional_options_description posoptdesc;
    posoptdesc.add( "input-file", -1 );

    fs::path input_file_path;

    try {
        po::variables_map vm;
        store( po::command_line_parser( argc, argv ).
            options( optdesc ).positional( posoptdesc ).run(), vm );

        notify( vm );
        if( vm.count( "help" ) ) {
            cout << optdesc;
            exit( 0 );
        }

        if( vm.count( "input-file" ) == 0 ) {
            cerr << "No input specified." << endl;
            exit( 1 );
        }

        g_opt.input_file_path = vm["input-file"].as<string>();
        g_opt.output_directory = fs::path{ vm["output-dir"].as<string>() };
        g_opt.verbose = vm["verbose"].as<int>();
        g_opt.yy_debug = vm.count( "yydebug" );
    }
    catch( po::unknown_option const& e ) {
        cerr << e.what() << endl;
        cout << optdesc;
        exit( -1 );
    }
    catch( po::error const& e ) {
        cerr << e.what() << endl;
        exit( -1 );
    }
    
    auto const& ext = g_opt.input_file_path.extension();
    if( ext == ".bc" ) {
        llvm_read( g_opt.input_file_path );
    }
    else {
        ril_read( g_opt.input_file_path );
    }
}

auto write_tables() -> void
{
    if( !g_opt.output_directory.empty() 
        && !fs::exists( g_opt.output_directory ) ) 
    {
        if( !fs::create_directory( g_opt.output_directory ) ) {
            cerr << bt::format( 
                "Unable to create direction %s.\n" )
                % g_opt.output_directory;
            exit( 1 );
        }
    }
    g_output.functions.open( 
        ( g_opt.output_directory / "functions.txt" ).string(), 
        ios::out|ios::binary );
    g_output.function_args.open( 
        ( g_opt.output_directory / "function-args.txt" ).string(), 
        ios::out|ios::binary );
    for( auto const& x : rts<function_tc>::container | ad::indexed() ) {
        auto const ref = make_ref_from_idx<function_tc>( x.index() );
        write_fields( 
            g_output.functions,
            ref,
            ref->addr,
            block{ 0 } );
        for( auto const arg : x.value().args | ad::indexed() ) {
            write_fields(
                g_output.function_args,
                ref,
                arg.index(),
                arg.value() );
        }
    }

    g_output.cfg_edges.open( 
        ( g_opt.output_directory / "cfg_edges.txt" ).string(), 
        ios::out|ios::binary );
    g_output.function_returns.open( 
        ( g_opt.output_directory / "function-returns.txt" ).string(), 
        ios::out|ios::binary );
    g_output.stores.open( 
        ( g_opt.output_directory / "stores.txt" ).string(), 
        ios::out|ios::binary );
    g_output.allocs.open( 
        ( g_opt.output_directory / "allocs.txt" ).string(), 
        ios::out|ios::binary );
    g_output.loads.open( 
        ( g_opt.output_directory / "loads.txt" ).string(), 
        ios::out|ios::binary );
    g_output.invokes.open( 
        ( g_opt.output_directory / "invokes.txt" ).string(), 
        ios::out|ios::binary );
    g_output.invoke_args.open( 
        ( g_opt.output_directory / "invoke-args.txt" ).string(), 
        ios::out|ios::binary );
    g_output.others.open( 
        ( g_opt.output_directory / "others.txt" ).string(), 
        ios::out|ios::binary );
    g_output.integers.open(
        ( g_opt.output_directory / "integers.txt" ).string(),
        ios::out|ios::binary );

    size_t num_pt_stores = 0;
    size_t num_pt_loads = 0;
    size_t num_pt_invokes = 0;
    for( auto const& fctxt : g_ctxt.fctxts ) {
        for( auto const edge : fctxt.edges ) {
            write_fields( 
                g_output.cfg_edges,
                fctxt.func,
                fctxt.ordering[edge->from].rpostorder,
                fctxt.ordering[edge->to].rpostorder );
        }
        for( auto const ret : fctxt.returns ) {
            write_fields( 
                g_output.function_returns,
                fctxt.func,
                fctxt.ordering[ret.second].rpostorder,
                ret.first );
        }
        for( auto const store : fctxt.stores ) {
            write_fields( 
                g_output.stores,
                store->func,
                fctxt.ordering[store->block].rpostorder,
                store->pointer,
                store->value );
        }

        for( auto const def : fctxt.defs ) {
            switch( def.get_tag() ) {
            case def_tc::ALLOC_A: {
                auto const& x = def.get<def_tc::alloc_a>();
                write_fields( 
                    g_output.allocs,
                    def,
                    x.func,
                    fctxt.ordering[x.block].rpostorder,
                    1 );
            }
            break;
            case def_tc::ALLOC: {
                auto const& x = def.get<def_tc::alloc>();
                write_fields( 
                    g_output.allocs,
                    def,
                    x.func,
                    fctxt.ordering[x.block].rpostorder,
                    0 );
            }
            break;
            case def_tc::LOAD: {
                auto const& x = def.get<def_tc::load>();
                write_fields( 
                    g_output.loads,
                    def,
                    x.func,
                    fctxt.ordering[x.block].rpostorder,
                    x.pointer );
            }
            break;
            case def_tc::GETELEMENTPTR: {
                die();
            }
            break;
            case def_tc::INVOKE: {
                auto const& x = def.get<def_tc::invoke>();
                write_fields(
                    g_output.invokes,
                    def,
                    x.func,
                    fctxt.ordering[x.block].rpostorder,
                    x.target );
                for( auto const arg : x.args | ad::indexed() ) {
                    write_fields(
                        g_output.invoke_args,
                        def,
                        arg.index(),
                        arg.value() );
                }
            }
            break;
            case def_tc::OTHER: {
                auto const& x = def.get<def_tc::other>();
                write_fields( 
                    g_output.others,
                    def,
                    x.func,
                    fctxt.ordering[x.block].rpostorder );
            }
            break;
            case def_tc::GLOBAL: {
                die();
            }
            break;
            case def_tc::ARGUMENT: {
                die();
            }
            case def_tc::INTEGER: {
                die();
            }
            break;
            case def_tc::NON_POINTER: {
                die();
            }
            break;
            default:
            die();
            }
        }
    }

    g_output.globals.open( 
        ( g_opt.output_directory / "globals.txt" ).string(), 
        ios::out|ios::binary );
    g_output.getelementptr.open( 
        ( g_opt.output_directory / "getelementptr.txt" ).string(), 
        ios::out|ios::binary );

    for( auto const def : g_ctxt.defs ) {
        BEGIN_TYPE_SWITCH_( def_tc, def )
        TYPE_CASE_X( global )
            write_fields( 
                g_output.globals,
                def );
        TYPE_CASE_X( integer )
            write_fields( 
                g_output.integers,
                def,
                x.value );
        TYPE_CASE_X( non_pointer )
        TYPE_CASE_X( getelementptr )
            write_fields( 
                g_output.getelementptr,
                def,
                x.base,
                x.offset );
        END_TYPE_SWITCH_
    }

    g_output.assigns.open(
        ( g_opt.output_directory / "assigns.txt" ).string(),
        ios::out|ios::binary );
    for( auto const x : g_ctxt.assigns ) {
        write_fields(
            g_output.assigns,
            x.first,
            x.second );
    }

    g_output.def_attributes.open(
        ( g_opt.output_directory / "def-attributes.txt" ).string(),
        ios::out|ios::binary );
    for( auto const& x : make_range<def_attributes_tc>() ) {
        write_fields(
            g_output.def_attributes,
            x.second.def,
            x.second.attr );
    }

    g_output.global_initializers.open(
        ( g_opt.output_directory / "global-initializers.txt" ).string(),
        ios::out|ios::binary );
    for( auto const& x : make_range<global_initializer_tc>() ) {
        write_fields(
            g_output.global_initializers,
            x.second.global,
            x.second.global_offset,
            x.second.value,
            x.second.value_offset );
    }


    auto fmt = bt::format( "%s: %s\n" );
    auto fmt2 = bt::format( "%s: %s (%s)\n" );

    if( g_opt.verbose >= 1 ) {
        cout << fmt % "function" % get_term_set_size<function_tc>();
        cout << fmt % "edge" % get_term_set_size<edge_tc>();
        cout << fmt2 % "store" % get_term_set_size<store_tc>() % num_pt_stores;
        cout << fmt % "assigns" % g_ctxt.assigns.size();
        cout << fmt % "alloca" % get_term_set_size<def_tc::alloc_a>();
        cout << fmt % "alloc" % get_term_set_size<def_tc::alloc>();
        cout << fmt2 % "load" % get_term_set_size<def_tc::load>() % num_pt_loads;
        cout << fmt % "getelementptr" % get_term_set_size<def_tc::getelementptr>();
        cout << fmt2 % "invoke" % get_term_set_size<def_tc::invoke>() % num_pt_invokes;
        cout << fmt % "argument" % get_term_set_size<def_tc::argument>();
        cout << fmt % "other" % get_term_set_size<def_tc::other>();
        cout << fmt % "global" % get_term_set_size<def_tc::global>();
        cout << fmt % "non-pointer" % get_term_set_size<def_tc::non_pointer>();
    }
}

template< typename T >
term_set<object_term_traits<T>> root_term_store<T>::container;

term_set<string_term_traits> root_term_store<string_tc>::container;

template< typename T >
auto insert_data(
    T const& x )
    -> object_t
{
    return root_term_store<T>::container.insert( x );
}

auto insert_data(
    string_ref const& x )
    -> object_t
{
    return root_term_store<string_tc>::container.insert( x );
}

template< typename T >
auto get_data( 
    object_t const idx )
    -> T const&
{
    return root_term_store<T>::container.get( idx );
}

auto get_string_data(
    object_t const idx )
    -> string_ref
{
    return root_term_store<string_tc>::container.get( idx );
}

template< typename T >
auto get_term_set_size() -> size_t
{
    return root_term_store<T>::container.next_idx;
}

#define RREF_INST( type ) void inst_rref( type const& ) \
    { \
        get_term_set_size<type>(); \
        for( auto const x : nil<type>() ) { \
            cons( x ); \
            insert_data<list_tc<type>::nil>( *(list_tc<type>::nil*)nullptr ); \
            get_data<list_tc<type>::nil>( 0 ); \
            type_switch( [](auto const& y){ insert_data( y ); }, x ); } }

RREF_INST( location_tc );
RREF_INST( statement_tc );
RREF_INST( expression_tc );
RREF_INST( function_tc );
RREF_INST( def_tc );
RREF_INST( store_tc );
RREF_INST( global_initializer_tc );
RREF_INST( edge_tc );
RREF_INST( def_attributes_tc );
RREF_INST( list_tc<string_tc> );
