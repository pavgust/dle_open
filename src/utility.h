#pragma once
#include "stdafx.h"

#define ENABLE_IF( b ) ::std::enable_if_t<(b)>* = nullptr

unsigned const cache_line_sz = 64;
unsigned const pool_chunk_size = 1024*1024*1;

auto inline aligned_malloc( size_t size, size_t align ) -> void* {
#ifdef _MSC_VER 
    return _aligned_malloc( size, align );
#else 
    void* res;
    if( posix_memalign( &res, align, size ) ) res = 0;
    return res;
#endif
}

auto inline aligned_free( void* p ) -> void {
#ifdef _MSC_VER 
    _aligned_free( p );
#else 
    free( p );
#endif
}

using string_ref = ::boost::string_ref;

template<typename T> 
using refw = ::boost::reference_wrapper<T>;

pair<size_t,size_t> process_mem_usage();

namespace boost {
    auto inline hash_value( string_ref const& x ) -> size_t
    {
        return ::boost::hash_range( x.begin(), x.end() );
    }

    template< typename T >
    auto inline hash_value( reference_wrapper<T> const& x ) -> size_t
    {
        return hash_value( x.get() );
    }

    template< typename T >
    auto inline hash_equality(
        reference_wrapper<T> const& l,
        reference_wrapper<T> const& r )
        -> bool
    {
        return hash_equality( l.get(), r.get() );
    }

    template< typename T >
    auto inline operator<(
        reference_wrapper<T> const& l,
        reference_wrapper<T> const& r )
        -> bool
    {
        return l.get() < r.get();
    }

    template< typename T >
    auto inline operator==(
        reference_wrapper<T> const& l,
        reference_wrapper<T> const& r )
        -> bool
    {
        return l.get_pointer() == r.get_pointer();
    }

    template< typename T >
    auto inline operator!=(
        reference_wrapper<T> const& l,
        reference_wrapper<T> const& r )
        -> bool
    {
        return l.get_pointer() != r.get_pointer();
    }
}

auto inline to_string( string_ref const& x ) -> string
{
    return string( x.begin(), x.end() );
}

auto inline stoi(
    string_ref const& str, 
    size_t * const pos = 0, 
    int const base = 10 )
    -> int
{
    return ::std::stoi( to_string( str ), pos, base ); 
}

struct identity_function
{
    template< typename T >
    auto operator()( T&& t ) const -> decltype( ::std::forward<T>( t ) )
    { return ::std::forward<T>( t ); }
};

struct equal_function
{
    template< typename T1, typename T2 >
    auto operator()( T1 const& t1, T2 const& t2 ) const -> bool
    { return t1 == t2; }
};

struct less_function
{
    template< typename T1, typename T2 >
    auto operator()( T1 const& t1, T2 const& t2 ) const -> bool
    { return t1 < t2; }
};

struct get_function
{
    template< typename T >
    auto operator()( T& x ) const -> decltype( ( x.get() ) )
    {
        return x.get();
    }
};

template<size_t N>
struct apply_helper {
    template<typename F, typename T, typename... A>
    static inline auto apply(F && f, T && t, A &&... a) -> decltype(auto)
    {
        return apply_helper<N-1>::apply(
            ::std::forward<F>(f), 
            ::std::forward<T>(t),
            ::std::get<N-1>(::std::forward<T>(t)), 
            ::std::forward<A>(a)... );
    }
};

template<>
struct apply_helper<0> {
    template<typename F, typename T, typename... A>
    static inline auto apply(F && f, T &&, A &&... a) -> decltype(auto)
    {
        return ::std::forward<F>(f)( ::std::forward<A>(a)... );
    }
};

template<typename F, typename T>
inline auto apply(F && f, T && t) -> decltype(auto)
{
    return apply_helper< ::std::tuple_size<
        typename ::std::decay<T>::type
    >::value>::apply( 
        ::std::forward<F>(f), ::std::forward<T>(t) );
}

template< typename T >
auto inline make_unique_ptr( T* const p )
    -> unique_ptr<T>
{
    return unique_ptr<T>( p );
}

template< typename T >
auto inline make_unique_arr_ptr( T p[] )
    -> unique_ptr<T[]>
{
    return unique_ptr<T[]>( p );
}

template< typename T, typename D >
auto inline make_unique_ptr( T* const p, D&& d )
    -> unique_ptr<T,D>
{
    return unique_ptr<T,D>( p, ::std::forward<D>( d ) );
}

template< typename T, typename D >
auto inline make_unique_arr_ptr( T p[], D&& d )
    -> unique_ptr<T[],D>
{
    return unique_ptr<T[],D>( p, ::std::forward<D>( d ) );
}

template< typename T >
auto inline map_contains( 
    T const& cont, 
    typename T::key_type const& val ) 
    -> bool
{
    return cont.find( val ) != cont.end();
}

template< typename T >
auto inline set_contains( 
    T const& cont, 
    typename T::value_type const& val ) 
    -> bool
{
    return cont.find( val ) != cont.end();
}

template< typename T >
auto inline seq_contains( 
    T const& cont, 
    typename T::value_type const& val ) 
    -> bool
{
    return find( cont.begin(), cont.end(), val ) != cont.end();
}

template< typename T, typename F >
auto inline seq_find_default(
    T const& cont,
    F&& func )
    -> typename T::value_type
{
    auto const iter = find_if( 
        begin( cont ), 
        end( cont ), 
        ::std::forward<F>( func ) );
    if( iter != end( cont ) ) {
        return *iter;
    }
    else {
        return typename T::value_type();
    }
}

template< typename T >
auto inline must_find( 
    T& cont, 
    typename T::value_type const& val,
    enable_if_t<is_same<typename T::value_type,typename T::key_type>::value>*
        = nullptr )
    -> decltype( auto )
{
    auto iter = cont.find( val );
    my_assert( iter != cont.end() );
    return ( *iter );
}

template< typename T >
auto inline must_find( 
    T& cont, 
    typename T::key_type const& val,
    enable_if_t<!is_same<typename T::value_type,typename T::key_type>::value>*
        = nullptr )
    -> decltype( auto )
{
    auto iter = cont.find( val );
    my_assert( iter != cont.end() );
    return ( iter->second );
}

template< typename T >
auto inline find_default( 
    T const& cont, 
    typename T::key_type const& key, 
    typename T::mapped_type const& val = typename T::mapped_type() )
    -> typename T::mapped_type 
{
    auto iter = cont.find( key );
    if( iter != cont.end() ) {
        return iter->second;
    }
    else {
        return val;
    }
}

template< typename T >
auto inline insert_default( 
    T& cont, 
    typename T::key_type const& key, 
    typename T::value_type const& val )
    -> decltype(auto)
{
    return cont.emplace( 
        key, 
        val ).first->second;
}

struct increment_tuple
{
    auto operator()() const
    {
    }
    template< typename Arg, typename... Args >
    auto operator()( Arg& arg, Args&... args ) const
    {
        ++arg;
        operator()( args... );
    }
};

template< typename... TupleArgs >
struct dereference_tuple
{
    template< typename... Args >
    auto operator()( Args&... args ) const -> decltype( auto )
    {
        return tuple<TupleArgs...>( *args... );
    }
};

template< typename... Iter >
struct tuple_iterator
    : ::boost::iterator_facade<
        tuple_iterator<Iter...>,
        ::std::tuple< typename ::std::iterator_traits<Iter>::value_type... >,
        ::boost::forward_traversal_tag,
        ::std::tuple< typename ::std::iterator_traits<Iter>::reference... > >
{
    ::std::tuple< Iter... > iters;

    tuple_iterator( Iter const&... iters0 )
        : iters( iters0... )
    {}
    auto dereference() const
    {
        return apply( dereference_tuple<
            typename ::std::iterator_traits<Iter>::reference...>(), 
            iters );
    }
    auto equal( tuple_iterator const& other ) const
    {
        return iters == other.iters;
    }
    auto increment()
    {
        apply( increment_tuple(), iters );
    }
};

template< typename... Args >
auto inline zip( Args&&... args ) 
    -> ::boost::iterator_range<
        tuple_iterator< typename ::boost::range_iterator<Args>::type... >
    >
{
    return ::boost::make_iterator_range(
        tuple_iterator< typename ::boost::range_iterator<Args>::type... >{ 
            begin( ::std::forward<Args>( args ) )... },
        tuple_iterator< typename ::boost::range_iterator<Args>::type... >{ 
            end( ::std::forward<Args>( args ) )... }
    );
}

template< typename T, typename F >
auto inline transform_to_vector(
    T const& range, 
    F const& f )
    -> vector<decltype( f( *begin( range ) ) )>
{
    using value_type = decltype( f( *begin( range ) ) );
    vector<value_type> ret;
    ret.reserve( size( range ) );
    for( auto const& x : range ) {
        ret.emplace_back( f( x ) );
    }
    return ret;
}

template< typename T >
struct scoped_exit
{
    T f;
    explicit scoped_exit( T const& f0 ) 
        : f( f0 )
    {}

    ~scoped_exit()
    {
        f();
    }
};

template< typename T >
auto inline make_scoped_exit( T const& f )
    -> scoped_exit<T>
{
    return scoped_exit<T>( f );
}

struct make_pair_function
{
    template< typename T1, typename T2 >
    auto operator()( T1&& t1, T2&& t2 ) const -> decltype(auto)
    { return make_pair( ::std::forward<T1>( t1 ), ::std::forward<T2>( t2 ) ); }
};

template< typename T >
auto operator<<(
    ::std::ostream& o,
    refw<T> const& x )
    -> ::std::ostream&
{
    return o << x.get();
}


template< typename T, typename Func >
struct delimited_output
{
    T const& cont;
    Func func;
    char const* const delimiter;
    char const* const prefix;

    delimited_output(
        T const& cont0,
        Func const& func0,
        char const* const delimiter0,
        char const* const prefix0 = "" )
        : cont( cont0 ),
        func( func0 ),
        delimiter( delimiter0 ),
        prefix( prefix0 )
    {}
};

template< typename T, typename F >
auto operator<<( 
    ::std::ostream& o,
    delimited_output<T,F> const& x )
    -> ::std::ostream&
{
    bool first = true;
    for( auto const& y : x.cont ) {
        if( !first ) {
            o << x.delimiter;
        }
        else {
            o << x.prefix;
        }
        x.func( o, y );
        first = false;
    }
    return o;
}

template< typename T >
auto delimited(
    T const& cont,
    char const* const delimiter = ",",
    char const* const prefix = "" )
{
    auto func = []( ::std::ostream& o, auto const& x ) -> ::std::ostream&
    { return o << x; };
    return delimited_output<T,decltype(func)>( 
        cont, 
        func,
        delimiter,
        prefix );
}

template< typename T, typename F >
auto delimited(
    T const& cont,
    F const& func,
    char const* const delimiter = ",",
    char const* const prefix = "" )
    -> delimited_output<T,F>
{
    return delimited_output<T,F>( cont, func, delimiter, prefix );
}

struct readable_unsigned
{
    uint64_t const num;
    bool const scientific;
    readable_unsigned(
        uint64_t const num0,
        bool const scientific0 = false )
        : num( num0 ),
        scientific( scientific0 )
    {}
};

auto inline operator<<(
    ::std::ostream& out,
    readable_unsigned const& x )
    -> ::std::ostream&
{
    using namespace std;
    auto const f = out.flags();
    auto const p = out.precision();
    if( x.scientific ) {
        unsigned exp = (unsigned)log10( x.num );
        out << fixed;
        out.precision( 2 );
        out << (x.num/pow(10,exp)) << 'E' << exp;
    }
    else {
        out << fixed;
        if( x.num >= 10*giga::num ) {
            out.precision( x.num >= 999950*mega::num ? 0 
                            : x.num >= 99995*mega::num ? 1
                            : 2 );
            out << x.num / double(giga::num) << 'G';
        }
        else if( x.num >= 10*mega::num ) {
            out.precision( x.num >= 999950*kilo::num ? 0 
                            : x.num >= 99995*kilo::num ? 1
                            : 2 );
            out << x.num / double(mega::num) << 'M';
        }
        else if( x.num >= 10*kilo::num ) {
            out.precision( x.num >= 999950 ? 0 
                            : x.num >= 99995 ? 1
                            : 2 );
            out << x.num / double(kilo::num) << 'k';
        }
        else {
            out << x.num;
        }
        out.precision( p );
        out.flags( f );
    }
    return out;
}

struct indenter
{
    unsigned const num;
};

auto inline operator<<( 
    ::std::ostream& o, indenter const& x ) 
    -> ::std::ostream&
{
    my_assert( x.num < 1000 );
    for( unsigned i = 0; i < x.num; ++i ) {
        o.write( "    ", 4 );
    }
    return o;
}

auto inline mybe32toh( 
    char const x[4] )
    -> uint32_t
{
    return ((unsigned char)x[0]<<24)
        + ((unsigned char)x[1]<<16)
        + ((unsigned char)x[2]<<8)
        + ((unsigned char)x[3]<<0);
}

struct escaped_string
{
    string_ref const s;
};

auto inline operator<<( 
    ::std::ostream& o, escaped_string const& x ) 
    -> ::std::ostream&
{
    for( char const y : x.s ) {
        if( ::boost::is_print()( y ) ) {
            o.put( y );
        }
        else {
            switch( y ) {
            case '\a': o << "\\a"; break;
            case '\b': o << "\\b"; break;
            case '\f': o << "\\f"; break;
            case '\n': o << "\\n"; break;
            case '\r': o << "\\r"; break;
            case '\t': o << "\\t"; break;
            case '\v': o << "\\v"; break;
            case '\'': o << "\\\'"; break;
            case '\"': o << "\\\""; break;
            default: o << ::boost::format("\\x%02x") % static_cast<int>( y ); 
            }
        }
    }
    return o;
}

struct surround_if
{
    ::std::ostream& o;
    bool const cond;
    char const* const end;
    surround_if( 
        ::std::ostream& o0, 
        bool const cond0, 
        char const* begin0 = "(",
        char const* end0 = ")" ) 
        : o( o0 ), 
        cond( cond0 ),
        end( end0 )
    {
        if( cond ) o << begin0;
    }
    ~surround_if()
    {
        if( cond ) o << end;
    }
};

template< typename T >
auto inline operator<<( 
    surround_if const& s, 
    T const& x ) -> surround_if const&
{
    s.o << x;
    return s;
}

inline auto upper_power_of_two(
    uint32_t v )
    -> uint32_t
{
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;
    return v;
}

inline auto upper_power_of_two(
    uint64_t v )
    -> uint64_t
{
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    v++;
    return v;
}

// ############################################################################
// Simple pool
// ############################################################################
struct free_delete
{
    void operator()( void* const x ) const { ::free( x ); }
};

struct aligned_free_delete
{
    void operator()( void* const x ) const { ::aligned_free( x ); }
};

template< typename T, typename... Args >
auto inline aligned_new( 
    size_t const align,
    Args&&... args )
    -> T*
{
    void* p = aligned_malloc( sizeof( T ), align );
    new( p ) T( ::std::forward<Args>( args )... );
    return reinterpret_cast<T*>( p );
}

template<typename T>
using unique_ptr_aligned = unique_ptr<T,aligned_free_delete>;

// User must round up to alignment requirement
struct simple_pool
{
    static const size_t chunk_sz = pool_chunk_size;
    typedef array<char,chunk_sz> chunk_type;
    size_t element_sz;

    simple_pool( simple_pool const& ) = delete;

private:
    void* next_element = nullptr;
    vector<unique_ptr<chunk_type,free_delete>> chunks;

public:
    explicit simple_pool(
        size_t const element_sz0 )
        : element_sz( max( sizeof( void* ), element_sz0 ) )
    {
        auto const rem = element_sz%alignment_of<void*>::value;
        if( rem > 0 ) {
            element_sz += alignment_of<void*>::value - rem;
        }
    }

    simple_pool(
        simple_pool&& other )
        : element_sz( other.element_sz ),
        next_element( other.next_element ),
        chunks( ::std::move( other.chunks ) )
    {
        other.next_element = nullptr;
    }

    auto swap( simple_pool& other )
        -> void
    {
        ::std::swap( element_sz, other.element_sz );
        ::std::swap( next_element, other.next_element );
        chunks.swap( other.chunks );
    }

    auto init_chunk( char* const chunk ) -> void
    {
        void** next = &next_element;
        for( char* p = chunk; 
            p + element_sz < chunk + chunk_sz; 
            p += element_sz ) 
        {
            *next = reinterpret_cast<void*>( p );
            next = reinterpret_cast<void**>( p );
        }
        *next = nullptr;
    }

    auto new_chunk()
        -> void
    {
        char* const chunk = (char*)::malloc( sizeof( chunk_type ) );
        chunks.emplace_back( reinterpret_cast<chunk_type*>( chunk ) );
        init_chunk( chunk );
    }

    auto malloc()
        -> void*
    {
        if( !next_element ) {
            new_chunk();
        }
        auto* const ret = next_element;
        next_element = *reinterpret_cast<void**>( next_element );
        return ret;
    }

    auto free( void* const p )
        -> void
    {
        if( p ) {
            *reinterpret_cast<void**>( p ) = next_element;
            next_element = p;
        }
    }

    auto clear()
        -> void
    {
        for( size_t i = 0; i < chunks.size(); ++i ) {
            init_chunk( reinterpret_cast<char*>( chunks[i].get() ) );
        }
    }

    auto purge_memory()
        -> void
    {
        chunks.clear();
        next_element = nullptr;
    }
};
