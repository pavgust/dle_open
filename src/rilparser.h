#pragma once
#include "stdafx.h"
#include "utility.h"
#include "ds.h"
#include "rootref.h"
#include "gen/riltypes.hh"

template< typename T >
struct root_term_store
{
    static term_set<object_term_traits<T>> container;
};

template<>
struct root_term_store< t_string<::root_reference> >
{
    static term_set<string_term_traits> container;
};

#define RREF_TYPE( arg ) using arg ## _tc = t_ ## arg<root_reference>;
RREF_TYPE(location)
RREF_TYPE(statement)
RREF_TYPE(expression)
RREF_TYPE(function)
RREF_TYPE(def)

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#define YY_TYPEDEF_YY_SIZE_T
typedef int yy_size_t;

#include "gen/rilparser.hh"
#define YY_DECL int rillex( \
    YYSTYPE* yylval, \
    YYLTYPE* yylloc, \
    yyscan_t yyscanner )

YY_DECL;

inline void rilerror(
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

char *rilget_text( yyscan_t );
yy_size_t rilget_leng( yyscan_t );

FILE *rilget_in( yyscan_t );
FILE *rilget_out( yyscan_t );
int rilget_lineno( yyscan_t );
void rilset_lineno( int, yyscan_t );
int  rilget_debug( yyscan_t );

void rilset_debug( int, yyscan_t );
void rilset_in( FILE* , yyscan_t );
void rilset_out( FILE* , yyscan_t );
void rilset_lineno( int , yyscan_t );

int rillex_init( yyscan_t* );
int rillex_destroy( yyscan_t );
int rilparse( yyscan_t );

auto inline construct( YYLTYPE const& loc )
{
    return construct<location_tc>(
        (uint32_t)loc.first_line, 
        (uint32_t)loc.first_column, 
        (uint32_t)loc.last_line, 
        (uint32_t)loc.last_column );
}

auto construct_global( rref_str const name ) -> rref<def_tc>;

auto inline to_formal_list( 
    rref_str const name, 
    rref<list_tc<string_tc>> const args )
    -> rref<list_tc<def_tc>>
{
    return transform_to_list( 
        args,
        [&]( rref_str const arg_name ) 
        {
            return construct<def_tc::argument>( name, arg_name );
        } );
}

extern vector<rref<function_tc>> functions;
