#pragma once
#include "stdafx.h"
#include "utility.h"
#include "table.h"
#include "main.h"

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#define YY_TYPEDEF_YY_SIZE_T
typedef int yy_size_t;

#include "gen/dleparser.hh"
#define YY_DECL int dlelex( \
    YYSTYPE* yylval, \
    YYLTYPE* yylloc, \
    yyscan_t yyscanner )

YY_DECL;

inline void dleerror(
    YYLTYPE* yylloc,
    yyscan_t yyscanner,
    const char* s )
{
    cerr << ienv.paths.back().filename().string() << ':'
		<< yylloc->first_line
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

auto find_macro( string_ref const& x ) -> string_ref;

auto inline construct_escaped( string_ref const& x ) -> rref_str
{
    string str;
    char const* p = x.begin();
    while( p != x.end() ) {
        auto c = *p++;
        if( c == '\\' && p != x.end() ) {
            switch( *p++ ) {
            case '\\': c = '\\'; break;
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case 't': c = '\t'; break;
            case 'n': c = '\n'; break;
            case 'v': c = '\v'; break;
            case 'f': c = '\f'; break;
            case 'r': c = '\r'; break;
            default: continue;
            }
        }
        str += c;
    }
    return construct( string_ref( str ) );
}

char *dleget_text( yyscan_t );
yy_size_t dleget_leng( yyscan_t );

FILE *dleget_in( yyscan_t );
FILE *dleget_out( yyscan_t );
int dleget_lineno( yyscan_t );
void dleset_lineno( int, yyscan_t );
int  dleget_debug( yyscan_t );

void dleset_debug( int, yyscan_t );
void dleset_in( FILE* , yyscan_t );
void dleset_out( FILE* , yyscan_t );
void dleset_lineno( int , yyscan_t );

int dlelex_init( yyscan_t* );
int dlelex_destroy( yyscan_t );
int dleparse( yyscan_t );

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

auto construct( YYLTYPE const& loc ) -> rref<location_tc>;
