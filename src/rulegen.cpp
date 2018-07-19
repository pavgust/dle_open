#include "stdafx.h"
#include "utility.h"

unsigned heap;
unsigned method;
enum class mode_type { CALL, PLAIN, FULL, SELECTIVE };
mode_type mode = mode_type::FULL;
bool type_sensitive = false;
bool flat_indices;
bool only_wildcard;
bool non_canon;

struct transformer
{
    unsigned exits;
    unsigned entries;
    bool wildcard;
};

vector<transformer> c_hm, c_hh, c_mm, c_h, c_m;

auto name( transformer const& t )
    -> string
{
    return "c_" 
        + string( t.exits, 'x' ) 
        + ( t.wildcard ? "w" : "" )
        + string( t.entries, 'e' );
}

auto all_transformers( 
    unsigned const exits, unsigned const entries,
    bool const method_context = false )
{
    vector<transformer> o;
    for( auto w = 0; w < 2; ++w ) {
        for( auto x = 0u; x <= exits; ++x ) {
            for( auto e = 0u; e <= entries; ++e ) {
                if( ( !w || !method_context )
                    && ( x < exits && e < entries || w || non_canon || method_context )
                    && ( !only_wildcard || !w || ( x == 0 && e == 0 ) ) )
                {
                    o.push_back( { x, e, (bool)w } );
                }
            }
        }
    }
    return o;
}

auto canonicalize( 
    transformer& o, 
    vector<string>& x, vector<string>& e,
    unsigned const max_x, unsigned const max_e,
    bool const method_context = false )
{
    if( only_wildcard && o.wildcard ) {
        o.entries = 0;
        o.exits = 0;
        x.clear();
        e.clear();
    }
    if( !non_canon 
        && !method_context
        && !( o.exits < max_x && o.entries < max_e ) ) 
    {
        o.wildcard = true;
    }
    return o;
} 

auto gen_compose( 
    string const& func_suffix,
    transformer const& l,
    transformer r,
    bool const right_reversed,
    unsigned const max_x, unsigned const max_e )
    -> void
{
    if( right_reversed ) {
        swap( r.entries, r.exits );
    }

    vector<string> lx, le, rx, re, ox, oe;
    transformer o{ 0, 0, false };
    auto midx = 0;

    for( ; midx < l.entries && midx < r.exits; ++midx ) {
        auto const& v = "M" + to_string( midx+1 );
        le.push_back( v );
        rx.push_back( v );
    }

    if( midx == l.entries && l.wildcard 
        || midx == r.exits && r.wildcard )
    {
        // Remainders consumed
        for( auto i = 0; i < l.exits; ++i ) {
            auto const& v = "X" + to_string( i+1 );
            lx.push_back( v );
            ox.push_back( v );
        }
        for( auto i = 0; i < r.entries; ++i ) {
            auto const& v = "E" + to_string( i+1 );
            re.push_back( v );
            oe.push_back( v );
        }
        for( ; midx < l.entries;  ++midx ) {
            le.push_back( "_" );
        }
        for( ; midx < r.exits; ++midx ) {
            rx.push_back( "_" );
        }
        o.wildcard = true;
    }
    else if( midx < l.entries ) {
        // LHS Exit-Entry remaining, RHS Entry remaining
        for( auto i = 0; i < l.exits; ++i ) {
            auto const& v = "X" + to_string( i+1 );
            lx.push_back( v );
            ox.push_back( v );
        }
        auto i = 0;
        for( ; i < r.entries; ++i ) {
            auto const& v = "E" + to_string( i+1 );
            re.push_back( v );
            oe.push_back( v );
        }
        for( ; i < max_e && midx < l.entries; ++i, ++midx ) {
            auto const& v = "E" + to_string( i+1 );
            le.push_back( v );
            oe.push_back( v );
        }
        for( ; midx < l.entries; ++midx ) {
            le.push_back( "_" );
            o.wildcard = true;
        }
        o.wildcard = o.wildcard || l.wildcard || r.wildcard;
    }
    else {
        // RHS Exit-Entry remaining, LHS Exit remaining
        for( auto i = 0; i < r.entries; ++i ) {
            auto const& v = "E" + to_string( i+1 );
            re.push_back( v );
            oe.push_back( v );
        }
        auto i = 0;
        for( ; i < l.exits; ++i ) {
            auto const& v = "X" + to_string( i+1 );
            lx.push_back( v );
            ox.push_back( v );
        }
        for( ; i < max_x && midx < r.exits; ++i, ++midx ) {
            auto const& v = "X" + to_string( i+1 );
            rx.push_back( v );
            ox.push_back( v );
        }
        for( ; midx < r.exits; ++midx ) {
            rx.push_back( "_" );
            o.wildcard = true;
        }
        o.wildcard = o.wildcard || l.wildcard || r.wildcard;
    }

    if( right_reversed ) {
        swap( re, rx );
        swap( r.entries, r.exits );
    }

    o.exits = ox.size();
    o.entries = oe.size();

    canonicalize( o, ox, oe, max_x, max_e );

    cout << bt::format( 
        "compose%s#bbf(%s(%s),%s(%s),%s(%s)).\n" )
        % func_suffix
        % name( l ) % delimited( rg::join( lx, le ) )
        % name( r ) % delimited( rg::join( rx, re ) )
        % name( o ) % delimited( rg::join( ox, oe ) );
}

auto gen_compose( 
    string const& func_suffix,
    vector<transformer> const& ls,
    vector<transformer> const& rs,
    bool const right_reversed,
    unsigned const max_x, unsigned const max_e )
    -> void
{
    for( auto const& l : ls ) {
        for( auto const& r : rs ) {
            gen_compose( 
                func_suffix,
                l, r,
                right_reversed,
                max_x, max_e );
        }
    }
}

auto gen_compose_w()
{
    for( auto const& l : c_h ) {
        vector<string> h, oe /*empty*/, ox;
        transformer o{ 0, 0, false };
        for( auto i = 0; i < l.exits; ++i ) {
            auto const& v = "H" + to_string( i+1 );
            h.push_back( v );
            ox.push_back( v );
        }

        o.exits = ox.size();
        o.wildcard = true;
        canonicalize( o, ox, oe, heap, method );

        cout << bt::format( 
            "compose_w#bbf(P,%s(%s),%s(%s)) :-: reachable_ci(P).\n" )
            % name( l ) % delimited( h )
            % name( o ) % delimited( ox );
    }
}


auto gen_constuctor(
    ostream& o, transformer const& t,
    string const& exit_type, string const& entry_type ) 
    -> decltype(auto)
{
    o << name( t );
    for( auto i = 0; i < t.exits; ++i ) {
        o << ' ' << exit_type;
    }
    for( auto i = 0; i < t.entries; ++i ) {
        o << ' ' << entry_type;
    }
    return o;
}

auto gen_data(
    string const& suffix,
    string const& exit_type, string const& entry_type,
    vector<transformer> const& ts )
{
    cout << bt::format( "data context%s = %s.\n" )
        % suffix
        % delimited( 
            ts, 
            [&]( auto& o, auto const& t ) -> decltype(auto)
            { return gen_constuctor( o, t, exit_type, entry_type ); },
            " | " );
}

auto gen_record_from()
{
    for( auto const& l : c_hm ) {
        vector<string> h, ox, oe;
        transformer o{ 0, 0, false };
        for( auto i = 0; i < l.exits; ++i ) {
            h.push_back( "_" );
        }
        for( auto i = 0; i < l.entries; ++i ) {
            auto const& v = "M" + to_string( i+1 );
            h.push_back( v );
            if( ox.size() < heap ) {
                ox.push_back( v );
            }
            else {
                o.wildcard = true;
            }
            oe.push_back( v );
        }

        o.exits = ox.size();
        o.entries = oe.size();
        canonicalize( o, ox, oe, heap, method );

        cout << bt::format( 
            "record_from#bf(%s(%s),%s(%s)).\n" )
            % name( l ) % delimited( h )
            % name( o ) % delimited( rg::join( ox, oe ) );
    }
}

auto gen_record_from_ci()
{
    for( auto const& l : c_hm ) {
        vector<string> h, ox, oe;
        transformer o{ 0, 0, false };
        ox.assign( heap, "M" );
        for( auto i = 0; i < l.exits; ++i ) {
            h.push_back( "_" );
        }
        for( auto i = 0; i < l.entries; ++i ) {
            auto const& v = "M" + to_string( i+1 );
            h.push_back( v );
            oe.push_back( v );
        }

        o.entries = oe.size();
        o.exits = ox.size();
        o.wildcard = true;
        canonicalize( o, ox, oe, heap, method );

        cout << bt::format( 
            "record_from_ci#bf(%s(%s),%s(%s)) :-: %s(M).\n" )
            % name( l ) % delimited( h )
            % name( o ) % delimited( rg::join( ox, oe ) )
            % ( mode == mode_type::CALL ? "intrinsic_invoke" 
                : type_sensitive ? "intrinsic_type"
                : "intrinsic_heap" );
    }
}

auto gen_merge()
{
    for( auto const& l : c_hm ) {
        vector<string> h, ox, oe;
        transformer o{ 0, 0, false };
        if( method > 0 ) {
            oe.push_back( 
                mode == mode_type::CALL ? "I" 
                : type_sensitive ? "get_declaring_type(H)"
                : "H" );
        }

        for( auto i = 0; i < l.exits; ++i ) {
            auto const& v = "H" + to_string( i+1 );
            h.push_back( v );
            if( mode == mode_type::FULL ) {
                if( oe.size() < method ) {
                    oe.push_back( v );
                }
                else {
                    o.wildcard = true;
                }
            }
        }
        for( auto i = 0; i < l.entries; ++i ) {
            auto const& v = "M" + to_string( i+1 );
            h.push_back( v );
            ox.push_back( v );
            if( mode != mode_type::FULL ) {
                if( oe.size() < method ) {
                    oe.push_back( v );
                }
                else {
                    o.wildcard = true;
                }
            }

        }

        o.exits = ox.size();
        o.entries = oe.size();
        if( mode != mode_type::CALL ) {
            o.wildcard = o.wildcard || l.wildcard;
        }
        canonicalize( o, ox, oe, method, method );

        cout << bt::format( 
            "merge#bbbf(H,I,%s(%s),%s(%s)).\n" )
            % name( l ) % delimited( h )
            % name( o ) % delimited( rg::join( ox, oe ) );
    }
}

auto gen_merge_s()
{
    if( mode == mode_type::CALL ) {
        vector<string> ox, oe;
        transformer o{ 0, 0, false };

        if( method > 0 ) {
            oe.push_back( "I" );
        }
        o.exits = ox.size();
        o.entries = oe.size();
        canonicalize( o, ox, oe, method, method );

        cout << bt::format( 
            "merge_s#bbf(P,I,%s(%s)) :-: reachable_ci(P).\n" )
            % name( o ) % delimited( oe );
        return;
    }

    for( auto const& l : c_m ) {
        vector<string> m, ox, oe;
        transformer o{ 0, 0, false };

        for( auto i = 0; i < l.entries; ++i ) {
            auto const& v = "M" + to_string( i+1 );
            m.push_back( v );
            ox.push_back( v );
            oe.push_back( v );
        }

        o.exits = ox.size();
        o.entries = oe.size();
        o.wildcard = false;
        canonicalize( o, ox, oe, method, method );

        cout << bt::format( 
            "merge_s#bbf(P,I,%s(%s)) :-: reachable(P,%s(%s)).\n" )
            % name( o ) % delimited( rg::join( ox, oe ) )
            % name( l ) % delimited( m );
    }
}

auto gen_drop_r()
{
    for( auto const& l : c_mm ) {
        vector<string> m, ox, oe;
        transformer o{ 0, 0, false };
        for( auto i = 0; i < l.exits; ++i ) {
            m.push_back( "_" );
        }
        for( auto i = 0; i < l.entries; ++i ) {
            auto const& v = "E" + to_string( i+1 );
            m.push_back( v );
            oe.push_back( v );
        }

        o.entries = oe.size();
        canonicalize( o, ox, oe, 0, method, true );

        cout << bt::format( 
            "drop_r#bf(%s(%s),%s(%s)).\n" )
            % name( l ) % delimited( m )
            % name( o ) % delimited( oe );
    }
}

auto gen_drop_m()
{
    for( auto const& l : c_hm ) {
        vector<string> m, ox;
        transformer o{ 0, 0, false };
        for( auto i = 0; i < l.exits; ++i ) {
            auto const& v = "H" + to_string( i+1 );
            m.push_back( v );
            ox.push_back( v );
        }
        for( auto i = 0; i < l.entries; ++i ) {
            m.push_back( "_" );
        }

        o.exits = ox.size();
        canonicalize( o, ox, ox, heap, 0 );
        o.wildcard = true;

        cout << bt::format( 
            "drop_m#bf(%s(%s),%s(%s)).\n" )
            % name( l ) % delimited( m )
            % name( o ) % delimited( ox );
    }
}

auto gen_indices(
    ostream& o,
    unsigned const start, 
    unsigned const num,
    bool first = true )
    -> ostream&
{
    if( first ) {
        o << bt::format( "hash_map([%d],10000," ) % start;
    }
    else {
        o << bt::format( "hash_map([%d],1," ) % start;
    }
    if( start < num ) {
        gen_indices( o, start+1, num, false );
    }
    else {
        o << "hash_set(100)";
    }
    return o << ")";
}

auto gen_vars(
    char const* const prefix,
    unsigned const num )
{
    vector<string> r;
    for( auto i = 0; i < num; ++i ) {
        r.push_back( prefix + to_string( i+1 ) );
    }
    return r;
}

struct indices { unsigned start, num; };
auto operator<<( ostream& o, indices const& i ) -> ostream&
{ return gen_indices( o, i.start, i.num ); }

auto gen_indices( 
    string const& name, indices const& idx, bool const secondary = false )
{
    if( !flat_indices ) {
        cout << bt::format( "index %s %s%s.\n" )
            % name
            % idx
            % ( secondary ? " secondary" : "" );
    }
    else {
        cout << bt::format( 
            "index %s array_map(%d,10000,%s(%d))%s.\n" )
            % name
            % idx.start
            % ( secondary ? "value_list" : "hash_set" )
            % ( 100 + idx.num - idx.start )
            % ( secondary ? " secondary" : "" );
        vector<unsigned> parms;
        for( auto i = 0u; i < idx.num-idx.start; ++i ) {
            parms.push_back( idx.start + i + 1 );
            cout << bt::format( 
                "index %s array_map(%d,10000,"
                    "hash_map([%s],2,value_list(%d))) secondary.\n" )
                % name
                % idx.start
                % delimited( parms )
                % ( 100 + idx.num - idx.start - i - 1 );
        }
    }
}

auto main( int argc,  char* argv[] ) 
    -> int
{
    namespace po = boost::program_options;
    po::options_description optdesc(
        "Usage: rulegen [option...] <output-file>" );
    optdesc.add_options()
        ( "help,h", "produce help message" )
        ( "output-file,o",
            po::value<string>(),
            "output file" )
        ( "method", po::value<unsigned>()->default_value( 1 ), 
            "method contexts" )
        ( "heap", po::value<unsigned>()->default_value( 0 ), 
            "heap contexts" )
        ( "flat-indices", "flat indices" )
        ( "mode,t", 
            po::value<string>()->default_value( "full" ), 
            "sensitivity mode" )
        ( "non-canon", "do not canonicalize" )
        ( "only-wildcard", "only-wildcard" )
        ;

    po::positional_options_description posoptdesc;
    posoptdesc.add( "output-file", 1 );

    fs::path output_file_path;
    try {
        po::variables_map vm;
        store( po::command_line_parser( argc, argv ).
            options( optdesc ).positional( posoptdesc ).run(), vm );

        notify( vm );
        if( vm.count( "help" ) ) {
            cout << optdesc;
            exit( 0 );
        }

        if( vm.count( "output-file" ) > 0 ) {
            output_file_path = vm["output-file"].as<string>();
        }

        auto const& mstr = vm["mode"].as<string>();
        if( mstr.empty() ) {
            cerr << "Invalid mode string: " << mstr << endl;
            exit( 1 );
        }
        mode = mstr[0] == 'f' || mstr[0] == 't' ? mode_type::FULL 
            : mstr[0] == 'p' ? mode_type::PLAIN
            : mstr[0] == 's' ? mode_type::SELECTIVE
            : mode_type::CALL;
        type_sensitive = mstr[0] == 't';
        method = vm["method"].as<unsigned>();
        heap = vm["heap"].as<unsigned>();
        flat_indices = vm.count( "flat-indices" );
        only_wildcard = vm.count( "only-wildcard" );
        non_canon = vm.count( "non-canon" );
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
    
    c_hm = all_transformers( heap, method );
    c_hh = all_transformers( heap, heap );
    c_mm = all_transformers( method, method );
    c_h = all_transformers( heap, 0 );
    c_m = all_transformers( 0, method, true );

    string const& heap_type = mode 
        == mode_type::CALL ? "invoke" 
        : type_sensitive ? "type"
        : "heap";
    string const& method_type = mode == mode_type::SELECTIVE
        ? "invoke_or_heap" : heap_type;

    auto const* const epsilon = ( !heap || !method ? "c_w" : "c_" );

    gen_data( "_hm", heap_type, method_type, c_hm );
    gen_data( "_hh", heap_type, method_type, c_hh );
    gen_data( "_mm", method_type, method_type, c_mm );
    gen_data( "_h", heap_type, method_type, c_h );
    gen_data( "_m", heap_type, method_type, c_m );
    cout << "include(\"java.dl\").\n";
    cout << bt::format( "record#bf(P,%s()) :-: reachable_ci(P).\n" )
        % epsilon;

    transformer recordcit{ heap, 0, true };
    vector<string> recordci( heap, "M" );
    cout << bt::format( "record_ci#bf(P,%s(%s)) :-: reachable_ci(P), %s(M).\n" )
        % name( recordcit ) % delimited( recordci )
        % ( mode == mode_type::CALL ? "intrinsic_invoke" 
            : type_sensitive ? "intrinsic_type"
            : "intrinsic_heap" );

    gen_record_from();
    gen_record_from_ci();
    
    gen_compose( "", c_hh, c_hm, false, heap, method );
    gen_compose( "_r", c_hm, c_hm, true, heap, heap );
    gen_compose( "_m", c_hm, c_mm, false, heap, method );
    gen_compose( "_mr", c_hm, c_mm, true, heap, method );
    gen_compose_w();

    gen_merge();
    gen_merge_s();
    gen_drop_r();
    gen_drop_m();

    transformer blankt{ 0, method, false };
    vector<string> blank( method, "M" );
    cout << bt::format( "blank(%s(%s)) :-: %s(M).\n" )
        % name( blankt ) % delimited( blank )
        % ( mode == mode_type::CALL ? "entry_invoke" 
            : type_sensitive ? "entry_type"
            : "entry_heap" );

    // pts
    for( auto const& l : c_hm ) {
        vector<string> heap_types( l.exits, heap_type );
        vector<string> method_types( l.entries, method_type );
        auto const h = gen_vars( "X", l.exits );
        auto const m = gen_vars( "Y", l.entries );

        cout << bt::format( "predicate pts_%s(variable%s,heap%s).\n" )
            % name( l )
            % delimited( method_types, ",", "," )
            % delimited( heap_types, ",", "," );
        gen_indices( "pts_" + name( l ), indices{ 0, l.entries } );
        cout << bt::format( "pts(Y,X,%s(%s)) :-: pts_%s(Y%s,X%s).\n" )
            % name( l ) % delimited( rg::join( h, m ) )
            % name( l )
            % delimited( m, ",", "," )
            % delimited( h, ",", "," );
    }

    // throw pts
    for( auto const& l : c_hm ) {
        vector<string> heap_types( l.exits, heap_type );
        vector<string> method_types( l.entries, method_type );
        auto const h = gen_vars( "X", l.exits );
        auto const m = gen_vars( "Y", l.entries );

        cout << bt::format( "predicate throw_pts_%s(method%s,heap%s).\n" )
            % name( l )
            % delimited( method_types, ",", "," )
            % delimited( heap_types, ",", "," );
        gen_indices( "throw_pts_" + name( l ), indices{ 0, l.entries } );
        cout << bt::format( "throw_pts(Y,X,%s(%s)) :-: throw_pts_%s(Y%s,X%s).\n" )
            % name( l ) % delimited( rg::join( h, m ) )
            % name( l )
            % delimited( m, ",", "," )
            % delimited( h, ",", "," );
    }

    // hpts
    for( auto const& l : c_hh ) {
        vector<string> xt( l.exits, heap_type );
        vector<string> et( l.entries, heap_type );
        auto const h = gen_vars( "X", l.exits );
        auto const m = gen_vars( "Y", l.entries );

        cout << bt::format( "predicate hpts_%s(heap,field_sig%s,heap%s).\n" )
            % name( l )
            % delimited( et, ",", "," )
            % delimited( xt, ",", "," );
        gen_indices( "hpts_" + name( l ), indices{ 0, l.entries+1 } );
        cout << bt::format( "hpts(Y,X,F,%s(%s)) :-: hpts_%s(Y,F%s,X%s).\n" )
            % name( l ) % delimited( rg::join( h, m ) )
            % name( l )
            % delimited( m, ",", "," )
            % delimited( h, ",", "," );
    }

    // hptsa
    for( auto const& l : c_hh ) {
        vector<string> xt( l.exits, heap_type );
        vector<string> et( l.entries, heap_type );
        auto const h = gen_vars( "X", l.exits );
        auto const m = gen_vars( "Y", l.entries );

        cout << bt::format( "predicate hptsa_%s(heap%s,heap%s).\n" )
            % name( l )
            % delimited( et, ",", "," )
            % delimited( xt, ",", "," );
        gen_indices( "hptsa_" + name( l ), indices{ 0, l.entries } );
        cout << bt::format( "hptsa(Y,X,%s(%s)) :-: hptsa_%s(Y%s,X%s).\n" )
            % name( l ) % delimited( rg::join( h, m ) )
            % name( l )
            % delimited( m, ",", "," )
            % delimited( h, ",", "," );
    }

    // hptss
    for( auto const& l : c_h ) {
        vector<string> xt( l.exits, heap_type );
        auto const h = gen_vars( "X", l.exits );

        cout << bt::format( "predicate hptss_%s(field_sig,heap%s).\n" )
            % name( l )
            % delimited( xt, ",", "," );
        cout << bt::format( "index hptss_%s array_map(0,10000,hash_set(10)).\n" )
            % name( l );
        cout << bt::format( "hptss(X,F,%s(%s)) :-: hptss_%s(F,X%s).\n" )
            % name( l ) % delimited( h )
            % name( l )
            % delimited( h, ",", "," );
    }

    // hload
    for( auto const& l : c_hm ) {
        vector<string> heap_types( l.exits, heap_type );
        vector<string> method_types( l.entries, method_type );
        auto const h = gen_vars( "X", l.exits );
        auto const m = gen_vars( "Y", l.entries );

        cout << bt::format( "predicate hload_%s(heap,field_sig%s,variable%s).\n" )
            % name( l )
            % delimited( heap_types, ",", "," )
            % delimited( method_types, ",", "," );
        gen_indices( "hload_" + name( l ), indices{ 0, l.exits+1 } );
        cout << bt::format( "hload(X,Y,F,%s(%s)) :-: hload_%s(X,F%s,Y%s).\n" )
            % name( l ) % delimited( rg::join( h, m ) )
            % name( l )
            % delimited( h, ",", "," )
            % delimited( m, ",", "," );
    }

    // hloada
    for( auto const& l : c_hm ) {
        vector<string> heap_types( l.exits, heap_type );
        vector<string> method_types( l.entries, method_type );
        auto const h = gen_vars( "X", l.exits );
        auto const m = gen_vars( "Y", l.entries );

        cout << bt::format( "predicate hloada_%s(heap%s,variable%s).\n" )
            % name( l )
            % delimited( heap_types, ",", "," )
            % delimited( method_types, ",", "," );
        gen_indices( "hloada_" + name( l ), indices{ 0, l.exits } );
        cout << bt::format( "hloada(X,Y,%s(%s)) :-: hloada_%s(X%s,Y%s).\n" )
            % name( l ) % delimited( rg::join( h, m ) )
            % name( l )
            % delimited( h, ",", "," )
            % delimited( m, ",", "," );
    }

    // call
    for( auto const& l : c_mm ) {
        vector<string> xt( l.exits, method_type );
        vector<string> et( l.entries, method_type );
        auto const x = gen_vars( "R", l.exits );
        auto const e = gen_vars( "E", l.entries );

        cout << bt::format( "predicate call_%s(invoke%s,method%s).\n" )
            % name( l )
            % delimited( xt, ",", "," )
            % delimited( et, ",", "," );
        gen_indices( "call_" + name( l ), 
            indices{ 0, l.exits } );
        gen_indices( "call_" + name( l ), 
            indices{ l.exits+1, l.exits+1+l.entries }, true );
        cout << bt::format( "call(I,Q,%s(%s)) :-: call_%s(I%s,Q%s).\n" )
            % name( l ) % delimited( rg::join( x, e ) )
            % name( l )
            % delimited( x, ",", "," )
            % delimited( e, ",", "," );
    }

    // reachable
    for( auto const& l : c_m ) {
        vector<string> method_types( l.entries, method_type );
        auto const m = gen_vars( "R", l.entries );

        cout << bt::format( "predicate reachable_%s(method%s).\n" )
            % name( l )
            % delimited( method_types, ",", "," );
        cout << bt::format( "index reachable_%s array_map(0,10000,hash_set(10)).\n" )
            % name( l );
        cout << bt::format( "reachable(Q,%s(%s)) :-: reachable_%s(Q%s).\n" )
            % name( l ) % delimited( m )
            % name( l )
            % delimited( m, ",", "," );
    }
}
