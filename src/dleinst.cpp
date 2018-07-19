#include "stdafx.h"

#include "rootref.h"
#include "ds.h"

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

#define RREF_INST( type ) auto inst_rref( type const& ) -> void \
    { \
        get_term_set_size<type>(); \
        for( auto const x : nil<type>() ) { \
            cons( x ); \
            insert_data<list_tc<type>::nil>( *(list_tc<type>::nil*)nullptr ); \
            get_data<list_tc<type>::nil>( 0 ); \
            type_switch( [](auto const& y){ insert_data( y ); }, x ); } }
#include "dleinst.h"

RREF_INST( list_tc< i32_tc > );
RREF_INST( list_tc< string_tc > );
RREF_INST( list_tc< binding_tc > );
RREF_INST( list_tc< list_tc< term_tc > > );
RREF_INST( list_tc< list_tc< typed_term_tc > > );
