#pragma once
#include "utility.h"
#include "basictypes.h"

#define BEGIN_TYPE_SWITCH( type, x ) { \
    using __type = type; \
    auto __switch_var = ( x ); \
    switch( __switch_var.get_tag() ) {
#define END_TYPE_SWITCH } }
#define BEGIN_TYPE_CASE( type, var ) case __type::type::d_tag: {\
    auto const& var = __switch_var.get<__type::type>();
#define BEGIN_TYPE_CASE_X( type ) case __type::type::d_tag: {\
    auto const& x = __switch_var.get<__type::type>();
#define END_TYPE_CASE } break;


#define BEGIN_TYPE_SWITCH_( type, x ) \
    BEGIN_TYPE_SWITCH( type, x ) default: { die();
#define END_TYPE_SWITCH_ END_TYPE_CASE END_TYPE_SWITCH
#define TYPE_CASE_DEFAULT default: {
#define TYPE_CASE_X( type ) END_TYPE_CASE BEGIN_TYPE_CASE_X( type )
#define TYPE_CASE_DIE( type ) END_TYPE_CASE case __type::type::d_tag: { die();
#define TYPE_CASE_THROUGH( type ) } case __type::type::d_tag: {

template< typename T, typename Enable = void >
struct root_reference;

template< typename T, typename Enable = void >
using rref = root_reference<T,Enable>;

template< typename DT >
auto inline make_ref_from_idx( object_t const idx )
    -> rref<typename DT::parent_type>
{
    return rref<typename DT::parent_type>::from_id( 
        (uint32_t)DT::d_tag, idx );
}

template< 
    typename T,
    typename Enable = typename enable_if<T::is_single>::type >
auto inline make_ref_from_idx( object_t const idx )
    -> rref<T>
{
    return rref<T>::from_id( idx );
}

template< typename T >
ATTRIBUTE_USED
auto insert_data( 
    T const& x )
    -> object_t;

auto insert_data(
    string_ref const& x )
    -> object_t;

template< typename T >
ATTRIBUTE_USED
auto get_data( 
    object_t const idx )
    -> T const&;

auto get_string_data( 
    object_t const idx )
    -> string_ref;

template< typename T >
ATTRIBUTE_USED
auto get_term_set_size() -> size_t;

template< 
    typename T, 
    typename... Args >
auto inline construct( Args&&... args ) 
{
    static_assert( 
        sizeof...(Args) == size_of_pack( typename T::members_type() ),
        "Initializer mismatch." );
    T tmp{ ::std::forward<Args>( args )... };
    auto const idx = insert_data( tmp );
    return make_ref_from_idx<T>( idx );
}

template< typename T, typename Enable >
struct root_reference
{
    static bool const is_tagged = true;
    using inner_type = T;
    using this_type = root_reference<inner_type>;
    dconstr_tag_t tag;
    object_t idx;

    root_reference() 
        : tag( 0 ), 
        idx( (object_t)-1 ) 
    {}

    auto get_tag() const { return tag; }

    template< typename DT >
    auto is() const -> bool
    {
        static_assert( is_same<T,typename DT::parent_type>::value, 
            "type mismatch" );
        my_assert( *this );
        return tag == (uint32_t)DT::d_tag;
    }

    template< typename DT >
    auto get() const -> DT const&
    {
        static_assert( is_same<T,typename DT::parent_type>::value, 
            "type mismatch" );
        my_assert( is<DT>() );
        return get_data<DT>( idx );
    }

    auto operator==( root_reference const& other ) const 
    { return tag == other.tag && idx == other.idx; };
    explicit operator bool() const { return idx != (object_t)-1; }
    auto hash_value() const -> size_t { return ( tag<<24 ) + idx; }
    static auto from_id( 
        dconstr_tag_t const tag0, 
        object_t const idx0 ) 
    { return root_reference<T>( tag0, idx0 ); }
private:
    explicit root_reference( 
        dconstr_tag_t const tag0, 
        object_t const idx0 ) 
        : tag( tag0 ), 
        idx( idx0 ) 
    {}
};


template< typename T  >
struct root_reference<T, typename enable_if<T::is_single>::type > 
{
    static bool const is_tagged = false;
    using inner_type = T;
    using this_type = root_reference<inner_type>;
    object_t id;
#ifndef NDEBUG
    T const* debug_value = nullptr;
#endif

    root_reference() 
        : id( (object_t)-1 ) 
    {}

    auto get() const -> T const&
    {
        my_assert( *this );
        return get_data<T>( id );
    }

    auto operator->() const -> T const*
    {
        return &get();
    }

    auto operator*() const -> T const&
    {
        return get();
    }

    auto operator==( root_reference const& other ) const 
    { return id == other.id; };
    explicit operator bool() const { return id != (object_t)-1; }
    auto hash_value() const -> size_t { return id; }
    static auto from_id( object_t const id ) 
    { return root_reference<T>( id ); }
private:
    explicit root_reference( object_t const id0 )
        : id( id0 ) 
    {
#ifndef NDEBUG
        if( *this ) {
            debug_value = &get();
        }
#endif    
    }
};

template<>
struct root_reference< t_string<::root_reference> > 
{
    static bool const is_tagged = false;
    using inner_type = t_string<::root_reference>;
    using this_type = root_reference<inner_type>;

    object_t id;
#ifndef NDEBUG
    string_ref debug_value;
#endif
    root_reference() 
        : id( (object_t)-1 ) 
    {}
    root_reference( string_ref const& x )
    {
        id = insert_data( x );
#ifndef NDEBUG
        debug_value = str();
#endif
    }
    root_reference( ::std::string const& x )
        : root_reference( (string_ref)x )
    {}
    root_reference( char const* const p )
        : root_reference( string_ref( p ) )
    {}

    auto str() const -> string_ref
    {
        return get_string_data( id );
    }

    auto to_string() const -> string
    {
        return string( str() );
    }

    auto get() const -> string_ref
    {
        return str();
    }

    auto operator*() const -> string_ref
    {
        return str();
    }

    auto operator==( root_reference const& other ) const 
    { return id == other.id; };
    explicit operator bool() const { return id != (object_t)-1; }
    auto hash_value() const -> size_t { return id; }
    static auto from_id( object_t const id ) 
    { return root_reference<t_string<::root_reference>>( id ); }
private:
    explicit root_reference( object_t const id0 )
        : id( id0 ) 
    {
#ifndef NDEBUG
        if( *this ) {
            debug_value = str();
        }
#endif
    }
};

using string_tc = t_string< root_reference >;
using rref_str = root_reference< string_tc >;

auto inline construct( string_ref const& x ) -> rref<string_tc>
{
    return x;
}

template<>
struct root_reference< t_i32< ::root_reference > >
{
    static bool const is_tagged = false;
    using inner_type = t_i32< ::root_reference >;
    using this_type = root_reference<inner_type>;
    uint32_t id;

    root_reference() 
        : id( (uint32_t)-1 ) {}

    root_reference( uint32_t const id0 )
        : id( id0 ) {}

    operator int32_t() const
    {
        return (int32_t)id;
    }

    auto get() const -> int32_t
    {
        return (int32_t)*this;
    }

    auto operator*() const -> int32_t
    {
        return (int32_t)*this;
    }

    auto operator==( root_reference const& other ) const
    { return id == other.id; }
    explicit operator bool() const { return id != 0; }
    auto hash_value() const -> size_t { return id; }
    static auto from_id( uint32_t const id ) 
    { return this_type( id ); }
};

using i32_tc = t_i32< root_reference >;

// Unsafe root reference for parser
template< typename T, typename Enable >
struct unsafe_root_reference
{
    dconstr_tag_t tag;
    object_t idx;
    unsafe_root_reference() = default;
    unsafe_root_reference( root_reference<T> const x )
        : tag( x.tag ), idx( x.idx )
    {}
    auto ref() const { return rref<T>::from_id( tag, idx ); };
    operator root_reference<T,Enable>() const { return ref(); }
};

template< typename T >
struct unsafe_root_reference<
    T,
    typename enable_if<!rref<T>::is_tagged>::type >
{
    object_t id;
    unsafe_root_reference() = default;
    unsafe_root_reference( root_reference<T> const x )
        : id( x.id )
    {}
    auto ref() const { return rref<T>::from_id( id ); };
    operator root_reference<T,void>() const
    { return ref(); }
};

template< typename T, typename Enable = void >
using urref = unsafe_root_reference<T, Enable>;

// List
template< typename T >
struct root_reference< t_list< ::root_reference, T > >
{
    static bool const is_tagged = true;
    using inner_type = t_list< ::root_reference, T >;
    using this_type = root_reference<inner_type>;
    using value_type = ::root_reference<T>;
	using reference = ::root_reference<T>&;
	using const_reference = ::root_reference<T> const&;
    dconstr_tag_t tag;
    object_t idx;
#ifndef NDEBUG
    typename inner_type::cons const* debug_value = nullptr;
#endif

    struct iterator_type : bt::iterator_facade<
        iterator_type,
        ::root_reference< T >,
        ::std::forward_iterator_tag,
        ::root_reference< T > >
    {
        this_type ref;
        iterator_type( this_type const& ref0 )
            : ref{ ref0 }
        {}
        auto dereference() const -> ::root_reference< T >
        {
            return ref.template get<typename t_list< ::root_reference, T >::cons>().head;
        }
        auto equal( iterator_type const& other ) const
        {
            return ref == other.ref;
        }
        auto increment()
        {
            ref = ref.template get<typename t_list< ::root_reference, T >::cons>().rest;
        }
    };
    using iterator = iterator_type;
    using const_iterator = iterator_type;
	using difference_type = size_t;
	using size_type = size_t;

    root_reference() 
        : tag( 0 ), 
        idx( (tagged_object_t)-1 )
    {}

    root_reference( std::initializer_list<rref<T>> init )
    {
        *this = construct<typename inner_type::nil>();
        for( auto const x : bt::make_iterator_range( 
            ::std::rbegin( init ), ::std::rend( init ) ) )
        {
            this->push_front( x );
        }
    }

    auto get_tag() const { return tag; }

    template< typename DT >
    auto is() const -> bool
    {
        static_assert( 
            is_same<inner_type,typename DT::parent_type>::value, 
            "type mismatch" );
        my_assert( *this );
        return tag == DT::d_tag;
    }

    template< typename DT >
    auto get() const -> DT const&
    {
        static_assert( 
            is_same<inner_type,typename DT::parent_type>::value, 
            "type mismatch" );
        my_assert( is<DT>() );
        return get_data<DT>( idx );
    }

    auto head() const -> rref<T>
    {
        return get<typename inner_type::cons>().head;
    }

    auto rest() const -> this_type
    {
        return get<typename inner_type::cons>().rest;
    }

    auto last() const -> rref<T>
    {
        my_assert( !empty() );
        auto i = *this;
        while( !i.rest().empty() ) {
            i = i.rest();
        }
        return i.head();
    }

    auto empty() const -> bool
    {
        return is<typename inner_type::nil>();
    }

    auto length() const -> size_t
    {
        if( empty() ) {
            return 0;
        }
        else {
            return 1 + rest().length();
        }
    }

    auto size() const -> size_t
    {
        return length();
    }

    auto begin() const -> iterator_type
    {
        return iterator_type{ *this };
    }

    auto end() const -> iterator_type
    {
        return iterator_type{
            construct<typename inner_type::nil>() };
    }

    auto push_front( rref<T> const x ) -> decltype( auto )
    {
        my_assert( *this );
        return *this = construct<typename inner_type::cons>( x, *this );
    }

    auto pop_front() -> void
    {
        my_assert( *this );
        *this = rest();
    }

    auto operator[]( size_t const i ) const -> rref<T>
    {
        return i == 0 ? head() : rest()[i-1];
    }

    auto operator==( root_reference const& other ) const 
    { return tag == other.tag && idx == other.idx; };
    explicit operator bool() const { return idx != (object_t)-1; }
    auto hash_value() const -> size_t { return (tag<<24)+idx; }
    static auto from_id( 
        dconstr_tag_t const tag0, 
        object_t const idx0 ) 
    { return this_type( tag0, idx0 ); }
private:
    explicit root_reference( 
        dconstr_tag_t const tag0, 
        object_t const idx0 ) 
        : tag( tag0 ), 
        idx( idx0 ) 
    {
#ifndef NDEBUG
        if( *this && !empty() ) {
            debug_value = &get<typename inner_type::cons>();
        }
#endif
    }
};

template< typename T >
using list_tc = t_list< root_reference, T >;

template< typename T >
using rref_list = rref<list_tc<T>>;

template< typename T >
auto inline nil() -> rref_list<T>
{
    return construct<typename list_tc<T>::nil>();
}

template< typename T >
auto cons( rref<T> const x, rref_list<T> rest = nil<T>() )
    -> rref_list<T>
{
    return rest.push_front( x );
}

template<
    typename T >
auto inline construct_n( size_t const n, rref<T> const x )
{
    auto list = nil<T>();
    for( size_t i = 0; i < n; ++i ) {
        list = cons( x, list );
    }
    return list;
}

template< typename T >
auto inline reverse_acc( 
    rref_list<T> const list,
    rref_list<T> const acc ) 
    -> rref_list<T>
{
    my_assert( list );
    if( list.empty() ) {
        return acc;
    }
    else {
        return reverse_acc( 
            list.rest(), 
            cons( list.head(), acc ) );
    }
}

template< typename T >
auto inline reverse( rref_list<T> const list ) 
    -> rref_list<T>
{
    my_assert( list );
    return reverse_acc( list, nil<T>() );
}

template< typename T, typename F, typename Initial >
auto inline foldr( 
    rref_list<T> const list, 
    F&& f,
    Initial&& initial )
    -> decltype( f( list.head(), ::std::forward<Initial>( initial ) ) )
{
    my_assert( list );
    if( list.empty() ) {
        return ::std::forward<Initial>( initial );
    }
    else {
        return f( 
            list.head(),
            foldr( list.rest(), ::std::forward<F>( f ), initial ) );
    }
}

template< typename T, typename F, typename Initial >
auto inline foldr( 
    T const& list, 
    F&& f,
    Initial&& initial )
{
    if( bt::empty( list ) ) {
        return ::std::forward<Initial>( initial );
    }
    else {
        return f( 
            *bt::begin( list ),
            foldr( 
                bt::make_iterator_range( 
                    bt::next( begin( list ) ), end( list ) ), 
                ::std::forward<F>( f ), initial ) );
    }
}

template< typename T, typename F >
auto inline transform_to_list( 
    rref_list<T> const list, 
    F const& f )
{
    my_assert( list );
    if( list.empty() ) {
        return nil< typename decltype(f(rref<T>()))::inner_type>();
    }
    else {
        auto const res = f( list.head() );
        return cons(
            res,
            transform_to_list( list.rest(), f ) );
    }
}

template< typename T, typename F >
auto inline transform_to_list( 
    T const& range, 
    F const& f )
{
    if( begin( range ) == end( range ) ) {
        return nil< typename decltype( f( *begin( range ) ) )::inner_type>();
    }
    else {
        auto const res = f( *begin( range ) );
        return cons(
            res,
            transform_to_list( 
                bt::make_iterator_range( 
                    bt::next( begin( range ) ), end( range ) ), f ) );
    }
}

template< typename T >
auto inline concat( 
    T const& range,
    rref_list<typename T::value_type::inner_type> const initial 
        = nil< typename T::value_type::inner_type >() )
{
    if( bt::empty( range ) ) {
        return initial;
    }
    else {
        return cons( 
            *begin( range ),
            concat( 
                bt::make_iterator_range( 
                    bt::next( begin( range ) ), end( range ) ), initial ) );
    }
}

template< typename T, typename IdxT >
auto inline slice(
    rref_list<T> const list,
    rref_list<IdxT> const idxs )
{
    return transform_to_list(
        idxs,
        [&]( size_t const idx )
        { return list[idx]; } );
}

auto inline operator<<( ostream& o, rref<string_tc> const x ) -> ostream&
{
    return o << x.get();
}

auto inline operator<<( ostream& o, rref<i32_tc> const x ) -> ostream&
{
    return o << x.get();
}

template< typename T >
auto inline hash_value( rref<T> const x ) -> size_t
{
    return x.hash_value();
}

template< typename T >
auto inline operator!=( rref<T> const l, rref<T> const r )
{
    return !( l == r );
}

template< typename T >
using rref_vector = vector<rref<T>>;

template< typename Key, typename Value >
using rref_map = unordered_map<rref<Key>,Value,bt::hash<rref<Key>>>;

template< typename T >
using rref_set = unordered_set<rref<T>,bt::hash<rref<T>>>;

template< typename T >
struct rref_range_iterator : bt::iterator_facade<
    rref_range_iterator<T>,
    pair<decltype(make_ref_from_idx<T>(0)),T const&> const,
    bt::forward_traversal_tag,
    pair<decltype(make_ref_from_idx<T>(0)),T const&> const >
{
    using this_type = rref_range_iterator<T>;
    size_t idx;

    rref_range_iterator( size_t const idx0 ) : idx( idx0 ) {}

    auto dereference() const
    {
        return make_pair( ref(), std::cref( get_data<T>( idx ) ) );
    }
    auto equal( this_type const& other ) const
    {
        return idx == other.idx;
    }
    auto increment()
    {
        ++idx;
    }
    auto ref() const
    {
        return make_ref_from_idx<T>( idx );
    }
};

template< typename T >
auto inline make_range() -> decltype(auto)
{
    return bt::make_iterator_range( 
        rref_range_iterator<T>{0},
        rref_range_iterator<T>{get_term_set_size<T>()} );
}
