#include "stdafx.h"
#include "utility.h"

using namespace std;
using boost::format;
using boost::filesystem::path;

template< typename T >
struct move_ptr
{
    T* const p;
    move_ptr( T* p0 ) : p( p0 ) {}
    auto operator*() -> T { return move( *p ); }
    ~move_ptr() { delete p; }
};

template< typename T >
auto make( T* const p ) -> move_ptr<T> { return p; }

struct atype
{
    string name;
    vector<atype> args;
    mutable bool is_variable;
};

struct field
{
    string name;
    atype type;
};

struct dconstr
{
    string name;
    vector<field> fields;
};

struct tconstr
{
    string name;
    vector<string> args;
    vector<dconstr> dconstrs;
};

auto to_fields( vector<atype> const& atypes )
{
    vector<field> ret;
    size_t i = 0;
    for( auto& x : atypes ) {
        ret.push_back( { "arg" + to_string( i ), move( x ) } );
        ++i;
    }
    return ret;
}

vector<tconstr> type_constructors;

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#define YY_TYPEDEF_YY_SIZE_T
typedef int yy_size_t;

#include "gen/inspectparser.hh"

#define YY_DECL int yylex( \
    YYSTYPE* yylval, \
    YYLTYPE* yylloc, \
    yyscan_t yyscanner )

YY_DECL;

inline void yyerror(
    YYLTYPE* yylloc,
    yyscan_t yyscanner,
    const char* s )
{
    cerr << yylloc->first_line
        << ':' << yylloc->first_column
        << '-'
        << yylloc->last_line
        << ':' << yylloc->last_column << " "
        << s << endl;
}

auto inline operator<<(
    ostream& o,
    YYLTYPE const& x )
    -> ostream&
{
    return o << x.first_line << ':' << x.first_column;
}

char *yyget_text( yyscan_t );
yy_size_t yyget_leng( yyscan_t );

FILE *yyget_in( yyscan_t );
FILE *yyget_out( yyscan_t );
int yyget_lineno( yyscan_t );
void yyset_lineno( int, yyscan_t );
int  yyget_debug( yyscan_t );

void yyset_debug( int, yyscan_t );
void yyset_in( FILE*, yyscan_t );
void yyset_out( FILE*, yyscan_t );
void yyset_lineno( int, yyscan_t );

int yylex_init( yyscan_t* );
int yylex_destroy( yyscan_t );
int yyparse( yyscan_t );

auto inline loc(
    YYLTYPE const& begin,
    YYLTYPE const& end )
    -> YYLTYPE
{
    YYLTYPE ret;
    ret.first_column = begin.first_column;
    ret.first_line = begin.first_line;
    ret.last_column = end.last_column;
    ret.last_line = end.last_line;
    return ret;
}

#ifdef _MSC_VER
#   pragma warning( push )
#   pragma warning( disable : 4065 4018 4702 4701 )
#else
#   pragma GCC diagnostic ignored "-Wpragmas"
#   pragma GCC diagnostic ignored "-Wunused-variable"
#   pragma GCC diagnostic ignored "-Wunused-function"
#   pragma GCC diagnostic ignored "-Wsign-compare"
#   pragma GCC diagnostic ignored "-Wnull-conversion"
#endif
#include "gen/inspectparser.cc"
#include "gen/inspectscanner.cc"
#ifdef _MSC_VER
#   pragma warning( pop )
#endif

#define INDENT1  "    "
#define INDENT2  "        "
#define INDENT3  "            "

struct indent_t
{};

indent_t indent;
unsigned level = 0u;

template< typename T >
struct indent_guard
{
    T const& inner;
    indent_guard( T const& inner0 ) : inner( inner0 ) {}
};

auto operator<<( ostream& o, indent_t const& ) -> ostream&
{
    for( size_t i = 0; i < level*4; ++i ) {
        o.put( ' ' );
    }
    return o;
}

template< typename T >
auto operator<<( ostream& o, indent_guard<T> const& x ) -> ostream&
{
    ++level;
    o << x.inner;
    --level;
    return o;
}

template< typename T >
auto iguard( T const& x ) -> indent_guard<T> { return x; }

auto operator<<( ostream& o, atype const& x ) -> ostream&
{
    if( x.is_variable ) {
        return o << format(
            "T%s" )
            % x.name;
    }
    else {
        return o << format(
            "t_%s<Ref%s>" )
            % x.name
            % delimited( x.args, ",", "," );
    }
}

auto operator<<( ostream& o, field const& x ) -> ostream&
{
    return o << indent << format(
        "Ref<%s,void> %s;" ) // TODO: remove void
        % x.type
        % x.name;
}

auto bind_atype( atype const& x, vector<string> const& vars ) -> void
{
    x.is_variable = seq_contains( vars, x.name );
    if( x.is_variable && x.args.size() > 0 ) {
        cerr << "Higher order kinds not allowed.\n";
        exit( 1 );
    }
    for( auto const& y : x.args ) {
        bind_atype( y, vars );
    }
}

auto main( 
    int argc, 
    char* argv[] ) 
    -> int
{
    yyscan_t scanner;
    yylex_init( &scanner );

    FILE* in = nullptr;
    if( argc >= 2 ) {
        in = fopen( argv[1], "rb" );
        if( !in ) {
            cerr << format( 
                "Error opening %1%.\n" )
                % argv[1];
            exit( 1 );
        }
        yyset_in( in, scanner );
    }
    
    ofstream fout;
    if( argc >= 3 ) {
        string header_name = string( argv[2] );
        fout.open( header_name, ios::binary );
        if( !fout.is_open() ) {
            cerr << format( 
                "Error opening %1%.\n" )
                % header_name;
            exit( 1 );
        }
    }
    ostream& hout = fout.is_open() ? fout : cout;
    if( yyparse( scanner ) ) {
        cerr << "Syntax error." << endl;
        exit( -1 );
    }

    if( in ) {
        fclose( in );
    }
    yylex_destroy ( scanner );

    hout << "#pragma once\n";
    hout << "#include \"basictypes.h\"\n";
    for( auto const& tc : type_constructors ) {
        for( auto const& dc : tc.dconstrs ) {
            for( auto const& f : dc.fields ) {
                bind_atype( f.type, tc.args );
            }
        }

        auto const tc_args_decl = delimited( 
            tc.args, 
            []( ostream& o, string const& x ) -> ostream& 
            { return o << "typename T" << x; },
            ", ",
            ", " );
        hout << format( 
            "template< template< typename... > class Ref%s >\n"
            "struct t_%s;\n" )
            % tc_args_decl
            % tc.name;
    }
    hout << "\n";

    for( auto const& tc : type_constructors ) {
        auto const tc_args_decl = delimited( 
            tc.args, 
            []( ostream& o, string const& x ) -> ostream& 
            { return o << "typename T" << x; },
            ", ",
            ", " );
        auto const tc_args_ref = delimited(
            tc.args,
            []( ostream& o, string const& x ) -> ostream&
            {
                return o << 'T' << x;
            },
            ",",
            "," );

        hout << format( 
            "template< template< typename... > class Ref%s >\n"
            "struct t_%s\n"
            "{\n" )
            % tc_args_decl
            % tc.name;
        ++level;
        bool const single_constructor = tc.dconstrs.size() == 1 
            && tc.dconstrs.front().name == tc.name;
        if( !single_constructor && !tc.dconstrs.empty() ) {
            hout << format(
                INDENT1 "enum {%s};\n" )
                % delimited( 
                    tc.dconstrs,
                    []( ostream& o, dconstr const& dc ) -> ostream&
                    { 
                        return o << ag::to_upper_copy( dc.name );
                    } );
        }

        if( single_constructor ) {
            auto const& dc = tc.dconstrs.front();
            for( auto const& f : dc.fields ) {
                hout << f << "\n";
            }
            hout << format(
                INDENT1 "auto static constexpr d_name() -> char const* "
                "{ return \"%1%\"; }\n" )
                % dc.name;
            hout << format( 
                INDENT1 "using members_type = members_pack<%s>;\n" )
                % delimited(
                    dc.fields,
                    [&]( ostream& o, field const& x ) -> ostream&
                    {
                        return o << format( "member<Ref,t_%1%,%2%,&t_%1%::%3%>" )
                            % tc.name
                            % x.type
                            % x.name;
                    },
                    ",\n" INDENT2,
                    "\n" INDENT2 );
        }
        else{
            for( auto const& dc : tc.dconstrs ) {
                hout << format(
                    INDENT1 "struct %s\n"
                    INDENT1 "{\n"  )
                    % dc.name;
                ++level;
                for( auto const& f : dc.fields ) {
                    hout << f << "\n";
                }
                hout << format(
                    INDENT2 "auto static constexpr d_name() -> char const* "
                    "{ return \"%1%\"; }\n" )
                    % dc.name;
                hout << format( 
                    INDENT2 "static unsigned const d_tag = %s;\n" )
                    % ag::to_upper_copy( dc.name );
                hout << format(
                    INDENT2 "using parent_type = t_%s;\n" )
                    % tc.name;
                hout << format( 
                    INDENT2 "using members_type = members_pack<%s>;\n" )
                    % delimited(
                        dc.fields,
                        [&]( ostream& o, field const& x ) -> ostream&
                        {
                            return o << format( "member<Ref,%1%,%2%,&%1%::%3%>" )
                                % dc.name
                                % x.type
                                % x.name;
                        },
                        ",\n" INDENT3,
                        "\n" INDENT3 );
                --level;
                hout << INDENT1 "};\n";
            }
        }
        hout << format(
            INDENT1 "static bool const is_single = %s;\n" )
            % ( single_constructor || tc.dconstrs.empty() ? "true" : "false" );
        hout << format(
            INDENT1 "auto static constexpr t_name() -> char const* "
            "{ return \"%1%\"; }\n" )
            % tc.name;
        hout << format( INDENT1 "using arguments_type = argument_pack<%s>;\n" )
            % delimited( 
                tc.args,
                []( ostream& o, string const& x ) -> ostream&
                {
                    return o << 'T' << x;
                } );
        if( !single_constructor && !tc.dconstrs.empty() ) {
            hout << format(
                INDENT1 "using dconstrs_type = dconstrs_pack<%s>;\n" )
                % delimited( 
                    tc.dconstrs,
                    []( ostream& o, dconstr const& dc ) -> ostream&
                    { 
                        return o << dc.name;
                    } );
        }
        else {
            hout << format(
                INDENT1 "using dconstrs_type = dconstrs_pack<t_%s>;\n" )
                % tc.name;
        }

        --level;
        hout << "};\n\n";

        hout << format(
            "template< typename Func, template< typename... > class Ref%2%"
            ", typename... RefArgs >\n"
            "auto inline type_switch( Func&& f, Ref<t_%1%<Ref%3%>,RefArgs...> const& x ) -> decltype(auto)\n"
            "{\n" )
            % tc.name
            % tc_args_decl
            % tc_args_ref;
        ++level;

        if( single_constructor ) {
            hout << INDENT1 "return forward<Func>( f )( x.get() );\n";
        }
        else if( tc.dconstrs.empty() ) {
        }
        else {
            hout << INDENT1 "switch( x.get_tag() ) {\n";

            for( auto const& dc : tc.dconstrs ) {
                hout << format(
                    INDENT1 "case t_%1%<Ref%4%>::%2%:\n" 
                    INDENT2"return forward<Func>( f )( x.template get<typename t_%1%<Ref%4%>::%3%>() ); \n" )
                    % tc.name
                    % ag::to_upper_copy( dc.name )
                    % dc.name
                    % delimited(
                        tc.args,
                        []( ostream& o, string const& x ) -> ostream&
                        {
                            return o << 'T' << x;
                        },
                        ",",
                        "," );
            }
            hout << INDENT1 "default: abort();\n"
                INDENT1 "}\n";
        }

        --level;
        hout << "}\n";
    }
}

template< template< typename... > class Ref >
struct t_string
{
};

template< template< typename... > class Ref >
struct t_i32
{
};

template< typename... Args >
struct argument_pack
{};

template< typename... Args >
struct members_pack
{};

template< typename... Args >
struct dconstrs_pack
{};
