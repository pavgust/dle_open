#pragma once
#include "stdafx.h"
#include "utility.h"
#include "basictypes.h"

auto bd_allocate( size_t const sz_pow ) -> void*;
auto bd_free( void* const p, size_t const sz_pow ) -> void;

struct stable_vector
{
    static unsigned const low_bits = 12;
    static size_t const chunk_size = (1<<low_bits);
    static size_t const low_mask = chunk_size-1;
    
    stable_vector() = default;
    stable_vector( stable_vector&& ) = default;
    stable_vector( stable_vector const& ) = delete;
    auto alloc( size_t const size ) -> void*
    {
        my_assert( size <= chunk_size );
        if( chunk_offset + size > chunk_size ) {
            bufs.emplace_back( 
                (char*)::malloc( chunk_size ), 
                free_delete() );
            chunk_offset = 0;
        }
        auto* p = &bufs.back()[chunk_offset];
        chunk_offset += size;
        offset += size;
        return p;
    }

    auto operator[]( size_t const i ) const -> void*
    {
        size_t const low_idx = i&low_mask;
        size_t const high_idx = i>>low_bits;
        my_assert( high_idx < bufs.size() );
        return &bufs[high_idx][low_idx];
    }

    template< typename T >
    auto at( size_t const i ) const -> T*
    {
        return reinterpret_cast<T*>( operator[]( i ) );
    }

private:
    vector<unique_ptr<char[],free_delete>> bufs;
    size_t chunk_offset{ chunk_size };
    size_t offset{ 0 };
};

struct fixed_stable_vector
{
    static unsigned const low_bits = 10;
    static size_t const elements_per_chunk = (1<<low_bits);
    static size_t const low_mask = elements_per_chunk-1;

    vector<unique_ptr<char[],free_delete>> bufs;
    size_t const element_size;
    size_t size{ 0 };

    fixed_stable_vector( size_t const element_size0 ) 
        : element_size( element_size0 )
    {}
    fixed_stable_vector( fixed_stable_vector&& ) = default;
    fixed_stable_vector( fixed_stable_vector const& ) = delete;

    auto alloc() -> void*
    {
        size_t const low_idx = (size&low_mask) * element_size;
        size_t const high_idx = size>>low_bits;
        if( high_idx >= bufs.size() ) {
            bufs.emplace_back( 
                (char*)::malloc( elements_per_chunk * element_size ), 
                free_delete() );
        }
        auto* p = &bufs.back()[low_idx];
        size += 1;
        return p;
    }

    auto operator[]( size_t const i ) const -> void*
    {
        size_t const low_idx = (i&low_mask) * element_size;
        size_t const high_idx = i>>low_bits;
        return &bufs[high_idx][low_idx];
    }
};

struct dynamic_vector
{
    unique_ptr<char[],free_delete> buf;
    size_t offset{ 0 };
    size_t capacity{ 0 };

    dynamic_vector() = default;
    dynamic_vector( dynamic_vector&& ) = default;
    dynamic_vector( dynamic_vector const& ) = delete;

    auto resize( size_t const expected )
    {
        while( expected > capacity ) {
            size_t const newcapacity = max<size_t>( 1024, capacity*2 );
            auto* p = ::malloc( newcapacity );
            memcpy( p, buf.get(), capacity );
            buf.reset( (char*)p );
            capacity = newcapacity;
        }
    }

    template< typename T = void>
    auto inline alloc( size_t const size ) -> T*
    {
        if( offset+size > capacity ) {
            resize( offset+size );
        }
        auto* ret = &buf[offset];
        offset += size;
        return reinterpret_cast<T*>( ret );
    }

    template< typename T, typename... Args >
    auto typed_alloc( Args&&... args )
    {
        auto* p = alloc( sizeof( T ) );
        new( p ) T( ::std::forward<Args>( args )... );
        return reinterpret_cast<T*>( p );
    }

    template< typename T >
    auto at( size_t const offset ) const -> T*
    {
        return reinterpret_cast<T*>( &buf[offset] );
    }

    auto data() const -> char*
    {
        return buf.get();
    }

    auto operator[]( size_t const offset ) const -> char*
    {
        return &buf[offset];
    }

    auto swap( dynamic_vector& other ) 
    {
        ::std::swap( buf, other.buf );
        ::std::swap( offset, other.offset );
        ::std::swap( capacity, other.capacity );
    }

    auto clear()
    {
        offset = 0;
    }

    auto size() const { return offset; }
    template< typename T >
    auto begin() const { return at<T>( 0 ); }
    template< typename T >
    auto end() const { return at<T>( size() ); }
};

template< 
    typename TraitsType
>
struct term_set
{
    typedef TraitsType traits_type;
    typedef typename traits_type::node_type node_type;
    typedef typename traits_type::mapped_type mapped_type;
    typedef typename traits_type::value_type value_type;
    size_t buckets_sz{0};
    unique_ptr<mapped_type[],free_delete> buckets;
    mapped_type next_idx{0};
    TraitsType traits;
    fixed_stable_vector vec;

    auto resize()
    {
        size_t new_sz = ::std::max<size_t>( 2*buckets_sz, 1 );
        unique_ptr<mapped_type[],free_delete> new_buckets( 
            (mapped_type*)malloc( new_sz*sizeof( mapped_type ) ) );
        memset( new_buckets.get(), -1, new_sz*sizeof( mapped_type ) );

        for( size_t i = 0; i < buckets_sz; ++i ) {
            if( buckets[i] != traits_type::invalid_idx ) {
                mapped_type const head = buckets[i];
                mapped_type idx = head;
                while( idx != traits_type::invalid_idx ) {
                    auto* elem = (node_type*)vec[idx];

                    size_t const h = traits.hash_value( *elem )&(new_sz-1);
                    mapped_type& new_head = new_buckets[h];

                    auto const tmp = traits.get_next( *elem );
                    traits.set_next( *elem, new_head );
                    new_head = idx;
                    idx = tmp;
                }
            }
        }
        ::std::swap( buckets, new_buckets );
        buckets_sz = new_sz;
    }

    auto get( 
        typename traits_type::mapped_type const idx ) const
        -> decltype( auto )
    {
        auto* p = (node_type*)vec[idx];
        return traits.get_values( *p );
    }

    auto insert( 
        typename traits_type::insert_type const& x )
        -> typename traits_type::mapped_type
    {
        if( next_idx*3 >= buckets_sz*2 ) {
            resize();
        }

        size_t const h = traits.hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const* elem = (node_type*)vec[idx];
            if( traits.equals( *elem, x ) ) {
                return idx;
            }
            idx = traits.get_next( *elem );
        }

        mapped_type const newidx = next_idx++;
        auto* p = (node_type*)vec.alloc();
        traits.construct( p, x );
        traits.set_next( *p, head );
        head = newidx;
        return newidx;
    }

    auto find( 
        typename traits_type::insert_type const& x )
        -> typename traits_type::mapped_type
    {
        if( buckets_sz == 0 ) {
            return traits_type::invalid_idx;
        }

        size_t const h = traits.hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const* elem = (node_type*)vec[idx];
            if( traits.equals( *elem, x ) ) {
                return idx;
            }
            idx = traits.get_next( *elem );
        }
        return idx;
    }

    struct iterator_type : bt::iterator_facade<
        iterator_type,
        value_type const&,
        bt::forward_traversal_tag,
        value_type const& >
    {
        term_set const& c;
        mapped_type idx;
        iterator_type( term_set const& c0, mapped_type const idx0 )
            : c( c0 ), idx{ idx0 }
        {}
        auto dereference() const -> typename traits_type::value_type const&
        {
            return c.get( idx );
        }
        auto equal( iterator_type const& other ) const
        {
            return idx == other.idx;
        }
        auto increment()
        {
            idx += 1;
        }
    };
    using iterator = iterator_type;
    using const_iterator = iterator_type;

    auto begin() const -> iterator_type
    {
        return iterator_type{ *this, 0 };
    }

    auto end() const -> iterator_type
    {
        return iterator_type{ *this, next_idx };
    }

    term_set() 
        : vec( traits.size() )
    {}
    explicit term_set( TraitsType const& traits0 )
        : traits( traits0 ),
        vec( traits.size() )
    {}

    term_set( term_set&& ) = default;
    term_set( term_set const& ) = delete;
};

template< typename Cont >
auto inline copy_components( 
    Cont& c,
    typename Cont::traits_type::mapped_type const idx,
    typename Cont::traits_type::insert_type const& dest )
{
    typedef typename Cont::traits_type::node_type node_type;
    auto* p = (node_type*)c.vec[idx];
    c.traits.copy( p, dest );
}

template< typename Cont >
auto inline get_node(
    Cont& c,
    typename Cont::traits_type::mapped_type const idx )
    -> typename Cont::traits_type::node_type*
{
    typedef typename Cont::traits_type::node_type node_type;
    return (node_type*)c.vec[idx];
}

struct string_term_traits
{
    typedef string_ref insert_type;
    typedef string_ref value_type;
    typedef object_t mapped_type;

    stable_vector pool;
    struct node_type 
    {
        mapped_type next;
        string_ref value;
        node_type( node_type const& ) = delete;
    };
    static mapped_type const invalid_idx = (mapped_type)-1;

    auto size() const -> size_t
    {
        return sizeof( node_type );
    }
    auto set_next( node_type& p, mapped_type const next ) const -> void 
    {
        p.next = next;
    }
    auto get_next( node_type const& p ) const -> mapped_type 
    {
        return p.next;
    }
    auto get_values( node_type const& p ) const -> string_ref
    {
        return p.value;
    }
    auto construct( node_type* const p, value_type const& x ) -> void
    {
        auto* buf = reinterpret_cast<char*>( pool.alloc( x.length() + 1 ) );
        memcpy( buf, x.data(), x.length() );
        buf[x.length()] = 0;
        p->value = string_ref( buf, x.length() );
    }
    auto hash_value( node_type const& p ) const -> size_t 
    { 
        return hash_value( get_values( p ) );
    }
    auto hash_value( value_type const& p ) const -> size_t 
    { 
        return bt::hash<string_ref>()( p );
    }

    auto equals( node_type const& l, value_type const& r ) const -> bool
    {
        return l.value == r;
    }
};

template< typename T >
auto inline hash_members( 
    T const& x, 
    members_pack<> const&,
    size_t& seed )
    -> void
{
}

template< typename T, typename Member, typename... Rest >
auto inline hash_members( 
    T const& x, 
    members_pack<Member,Rest...> const&,
    size_t& seed )
    -> void
{
    seed ^= hash_value( x.*(Member::pointer()) ) + 0x9e3779b9 + (seed<<6) + (seed>>2);
    hash_members( x, members_pack<Rest...>(), seed );
}

template< typename T >
auto inline equals_members( 
    T const& l,
    T const& r,
    members_pack<> const& )
    -> bool
{
    return true;
}

template< typename T, typename Member, typename... Rest >
auto inline equals_members( 
    T const& l,
    T const& r,
    members_pack<Member,Rest...> const& )
    -> bool
{
    return l.*(Member::pointer()) == r.*(Member::pointer()) 
        && equals_members( l, r, members_pack<Rest...>() );
}

template< typename T >
struct object_term_traits
{
    struct compound_hook
    {
        object_t next;
        T const value;
        compound_hook( compound_hook const& ) = delete;
    };

    typedef T value_type;
    typedef T insert_type;
    typedef compound_hook node_type;
    typedef object_t mapped_type;
    static mapped_type const invalid_idx = (mapped_type)-1;

    auto size() const -> size_t
    {
        return sizeof( node_type );
    }
    auto set_next( node_type& p, mapped_type const next ) const -> void 
    {
        p.next = next;
    }
    auto get_next( node_type const& p ) const -> mapped_type 
    {
        return p.next;
    }

    auto get_values( node_type const& p ) const -> value_type const&
    {
        return p.value;
    }

    auto construct( 
        node_type* const p, 
        value_type const& x ) const -> void
    {
        new( (void*)&p->value ) T( x );
    }

    auto hash_value( 
        node_type const& x ) const
        -> size_t 
    {
        return hash_value( x.value );
    }

    auto hash_value( value_type const& x ) const -> size_t
    { 
        size_t seed = 0;
        hash_members( x, typename T::members_type(), seed );
        return seed;
    }

    auto equals( node_type const& ll, value_type const& r ) const -> bool
    {
        return equals( get_values( ll ), r );
    }

    auto equals( value_type const& l, value_type const& r ) const -> bool
    {
        return equals_members( l, r, typename T::members_type() );
    }
};

struct indexed_term_traits
{
    struct compound_hook
    {
        object_t next;
        compound_hook( compound_hook const& ) = delete;
    };

    typedef pair<tagged_object_t*,uint32_t const*> insert_type;
    typedef tagged_object_t const* value_type;
    typedef compound_hook node_type;
    typedef object_t mapped_type;
    static mapped_type const invalid_idx = (mapped_type)-1;

    uint32_t const num_elements;

    auto size() const -> size_t
    {
        return sizeof( node_type ) + num_elements*sizeof( tagged_object_t );
    }
    auto set_next( node_type& p, mapped_type const next ) const -> void 
    {
        p.next = next;
    }
    auto get_next( node_type const& p ) const -> mapped_type 
    {
        return p.next;
    }

    auto get_values( node_type const& p ) const -> value_type
    {
        return reinterpret_cast<value_type>( &p+1 );
    }

    auto construct( node_type* const p, insert_type const& x ) const -> void
    {
        tagged_object_t* dest = const_cast<tagged_object_t*>( get_values( *p ) );
        for( size_t i = 0; i < num_elements; ++i ) {
            dest[i] = x.first[x.second[i]];
        }
    }

    auto copy( node_type* const p, insert_type const& x ) const -> void
    {
        tagged_object_t const* src = get_values( *p );
        for( size_t i = 0; i < num_elements; ++i ) {
            x.first[x.second[i]] = src[i];
        }
    }

    auto hash_value( node_type const& x ) const -> size_t 
    {
        size_t seed = 0;
        auto const* p = get_values( x );
        for( size_t i = 0; i < num_elements; ++i ) {
            bt::hash_combine( seed, p[i] );
        }
        return seed;
    }

    auto hash_value( insert_type const& x ) const -> size_t 
    { 
        size_t seed = 0;
        for( size_t i = 0; i < num_elements; ++i ) {
            bt::hash_combine( seed, x.first[x.second[i]] );
        }
        return seed;
    }

    auto equals( node_type const& ll, insert_type const& rr ) const -> bool
    {
        return equals( get_values( ll ), rr );
    }

    auto equals( tagged_object_t const* l, insert_type const& r ) const -> bool
    {
        for( size_t i = 0; i < num_elements; ++i ) {
            if( l[i] != r.first[r.second[i]] ) return false;
        }
        return true;
    }
};

struct indexed_map_traits
{
    struct compound_hook
    {
        object_t next;
        compound_hook( compound_hook const& ) = delete;
    };

    typedef pair<tagged_object_t*,uint32_t const*> insert_type;
    typedef tagged_object_t const* value_type;
    typedef compound_hook node_type;
    typedef object_t mapped_type;
    static mapped_type const invalid_idx = (mapped_type)-1;

    uint32_t const num_elements;
    uint32_t const mapped_sz;

    auto size() const -> size_t
    {
        return sizeof( node_type ) 
            + num_elements*sizeof( tagged_object_t )
            + mapped_sz;
    }
    auto set_next( node_type& p, mapped_type const next ) const -> void 
    {
        p.next = next;
    }
    auto get_next( node_type const& p ) const -> mapped_type 
    {
        return p.next;
    }

    auto get_values( node_type const& p ) const -> value_type
    {
        return reinterpret_cast<value_type>( &p+1 );
    }

    auto get_mapped( node_type const& p ) const -> char*
    {
        return reinterpret_cast<char*>( 
            const_cast<tagged_object_t*>( 
                get_values( p ) + num_elements ) );
    }

    auto construct( node_type* const p, insert_type const& x ) const -> void
    {
        tagged_object_t* dest = const_cast<tagged_object_t*>( get_values( *p ) );
        for( size_t i = 0; i < num_elements; ++i ) {
            dest[i] = x.first[x.second[i]];
        }
    }

    auto copy( node_type* const p, insert_type const& x ) const -> void
    {
        tagged_object_t const* src = get_values( *p );
        for( size_t i = 0; i < num_elements; ++i ) {
            x.first[x.second[i]] = src[i];
        }
    }

    auto hash_value( node_type const& x ) const -> size_t 
    {
        size_t seed = 0;
        auto const* p = get_values( x );
        for( size_t i = 0; i < num_elements; ++i ) {
            bt::hash_combine( seed, p[i] );
        }
        return seed;
    }

    auto hash_value( insert_type const& x ) const -> size_t 
    { 
        size_t seed = 0;
        for( size_t i = 0; i < num_elements; ++i ) {
            bt::hash_combine( seed, x.first[x.second[i]] );
        }
        return seed;
    }

    auto equals( node_type const& ll, insert_type const& rr ) const -> bool
    {
        return equals( get_values( ll ), rr );
    }

    auto equals( tagged_object_t const* l, insert_type const& r ) const -> bool
    {
        for( size_t i = 0; i < num_elements; ++i ) {
            if( l[i] != r.first[r.second[i]] ) return false;
        }
        return true;
    }
};

using indexed_term_set = term_set<indexed_term_traits>;
using indexed_hash_map = term_set<indexed_map_traits>;
using array_map = fixed_stable_vector;
