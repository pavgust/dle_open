#define _CRT_SECURE_NO_WARNINGS
#define BOOST_EXCEPTION_DISABLE
#define BOOST_NO_EXCEPTIONS
#include <memory>
#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <cstdarg>
#include <cinttypes>
#include <ctime>
#include <string>
#include <functional>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/enum_trailing_params.hpp>
#include <boost/utility/string_ref.hpp>
#include <boost/functional/hash.hpp>
#include <boost/functional/hash/extensions.hpp>

#define MAX_ARITY 15

#define PARM3(z, n, x) x

//#define TRACE(...) my_fprintf( stderr, __VA_ARGS__ ); fflush( stderr );
//#define DEBUG_ALLOC
#ifndef TRACE
#   define TRACE(...)
#endif

#ifdef NATIVE_CALL
#undef assert
#define assert(...)
#endif

#define INLINE_WRAPPER
//#define INLINE_WRAPPER BOOST_FORCEINLINE
#define INLINE_VEC_OP BOOST_FORCEINLINE
#define INLINE_HASH_OP BOOST_FORCEINLINE
#define INLINE_VALUE_OP BOOST_FORCEINLINE

#define LOAD_FACTOR_NUMER 2
#define LOAD_FACTOR_DENOM 3

#define OPEN_ADDRESSING
#define QUADRATIC_PROBING

#ifndef NATIVE_CALL
#define USE_HIGHPERFORMANCE_TIMER
#endif

#define USE_NATIVE_HEAP

#ifndef OPEN_ADDRESSING
#   define hash_set ch_hash_set
#   define hash_set_traits uint_vec_traits
#else
#   define hash_set oa_hash_set
#   define hash_set_traits oa_uint_vec_traits
#endif

#if defined(__i386)
#   define SIZE_T_CTZ __builtin_ctz
#elif defined(__x86_64__)
#   define SIZE_T_CTZ __builtin_ctzll
#elif !defined( __INTELLISENSE__ )
#   error( "Unknown processor" )
#endif

#if defined(USE_NATIVE_HEAP) && defined(_WIN32)
#include <Windows.h>
#   define POOL_MALLOC( sz ) my_malloc( sz )
#   define POOL_REALLOC( ptr, sz, old_sz ) my_realloc( ptr, sz, old_sz )
#   define POOL_FREE( ptr, sz ) my_free( ptr, sz )
#   define _MALLOC( sz ) ::malloc( sz )
#   define _REALLOC( ptr, sz ) ::realloc( ptr, sz )
#   define _FREE( ptr ) ::free( ptr )
#else
#   define POOL_MALLOC( sz ) ::malloc( sz )
#   define POOL_REALLOC( ptr, sz, old_sz ) ::realloc( ptr, sz )
#   define POOL_FREE( ptr, sz ) ::free( ptr )
#   define _MALLOC( sz ) ::malloc( sz )
#   define _REALLOC( ptr, sz ) ::realloc( ptr, sz )
#   define _FREE( ptr ) ::free( ptr )
#endif

const uint32_t offset_basis = 2166136261U;
const uint32_t prime = 2654435761U;

extern "C"
{
    extern FILE* my_fopen( char const*, char const* );
    extern int my_fclose( FILE* );
    extern char* my_fgets( char const*, int, FILE* );
    extern int my_fputs( const char*, FILE* );
    extern int my_printf( char const*, ... );
    extern int my_fprintf( FILE*, char const*, ... );
    extern int my_vfprintf( FILE*, char const*, va_list );
    extern int my_fwrite( void const*, size_t, size_t, FILE* );
    extern int my_fflush( FILE* );
    extern void my_exit( int );
    extern void* my_malloc( size_t );
    extern void* my_realloc( void*, size_t, size_t );
    extern void my_free( void*, size_t );
    extern void high_performance_reset_timer();
    extern void high_performance_print_timer();
    extern void high_performance_log_timer();
    extern char const* const find_file( char const* const );
}

#ifdef NATIVE_CALL
#define my_fopen fopen
#define my_fclose fclose
#define my_fgets fgets
#define my_fputs fputs
#define my_printf printf
#define my_fprintf fprintf
#define my_vfprintf vfprintf
#define my_fwrite fwrite
#define my_fflush fflush
#define my_exit exit
#endif

using namespace std;

typedef uint32_t object_t;
typedef uint32_t tagged_object_t;
typedef size_t table_idx_t;
typedef uint32_t dconstr_tag_t;
using string_ref = ::boost::string_ref;

namespace boost {
    auto inline hash_value(
        string_ref const& x )
        -> size_t
    {
        return ::boost::hash_range( x.begin(), x.end() );
    }
}

auto inline get_tagged_id(
    dconstr_tag_t const constr,
    object_t const idx )
    -> tagged_object_t
{
    return (constr << 27) + idx;
}

auto inline get_dconstr_tag(
    tagged_object_t const id )
    -> dconstr_tag_t
{
    return id >> 27;
}

auto inline get_object_idx(
    tagged_object_t const id )
    -> object_t
{
    return (~(0b11111 << 27))&id;
}

static size_t total_fixed_vector_alloc_sz = 0;
struct fixed_vector
{
    char* buf{ nullptr };
    size_t sz{ 0 };
    size_t capacity{ 0 };

    fixed_vector() = default;
    fixed_vector( fixed_vector&& ) = default;
    fixed_vector( fixed_vector const& ) = delete;

    ~fixed_vector()
    {
        POOL_FREE( buf, capacity );
    }

    BOOST_NOINLINE
    auto resize( size_t const expected )
    {
        auto const old_capacity = capacity;
        while( expected > capacity ) {
            capacity = max<size_t>( 16, capacity*2 );
        }
        buf = (char*)POOL_REALLOC( buf, capacity, old_capacity );
#ifdef DEBUG_ALLOC
        if( !buf ) {
            my_fprintf( stderr, "REALLOC FAILED\n" );
            my_exit( 1 );
        }
        my_fprintf( stderr, "REALLOC: %p\n", buf );
        total_fixed_vector_alloc_sz += capacity - old_capacity;
        my_fprintf( stderr, "DEBUG: %p fixed_vector resize %d (%d)\n", 
            (void*)this, capacity, total_fixed_vector_alloc_sz );
#endif
    }

    template< typename T >
    BOOST_FORCEINLINE
    auto alloc() -> T*
    {
        size_t const offset = sz*sizeof(T);
        if( offset+sizeof(T) > capacity ) {
            resize( offset+sizeof(T) );
        }
        sz += 1;
#ifdef DEBUG_ALLOC
        my_fprintf( stderr, "DEBUG: %p fixed_vector alloc %d %d %d\n", 
            (void*)this, offset, capacity, sz );
#endif
        return reinterpret_cast<T*>( &buf[offset] );
    }

    template< typename T >
    BOOST_FORCEINLINE
    auto alloc( size_t const element_sz ) -> T*
    {
        size_t const offset = sz*element_sz;
        if( offset+element_sz > capacity ) {
            resize( offset+element_sz );
        }
        sz += 1;
#ifdef DEBUG_ALLOC
        my_fprintf( stderr, "DEBUG: %p fixed_vector alloc %d %d %d\n", 
            (void*)this, offset, capacity, sz );
#endif
        return reinterpret_cast<T*>( &buf[offset] );
    }

    auto data() const -> char*
    {
        return buf;
    }

    template< typename T >
    BOOST_FORCEINLINE
    auto at( size_t const idx ) const -> T&
    {
        return *reinterpret_cast<T*>( &buf[idx*sizeof(T)] );
    }

    template< typename T >
    BOOST_FORCEINLINE
    auto at( size_t const idx, size_t const element_sz ) const -> T&
    {
        return *reinterpret_cast<T*>( &buf[idx*element_sz] );
    }

    template< typename T >
    BOOST_FORCEINLINE
    auto back() const -> T&
    {
        return at<T>( sz - 1 );
    }

    BOOST_FORCEINLINE
    auto swap( fixed_vector& other ) 
    {
        ::std::swap( buf, other.buf );
        ::std::swap( sz, other.sz );
        ::std::swap( capacity, other.capacity );
    }

    BOOST_FORCEINLINE
    auto clear()
    {
        sz = 0;
    }

    BOOST_FORCEINLINE
    auto size() const { return sz; }
};

template< size_t low_bits = 12 >
struct stable_vector
{
    static size_t const chunk_size = (1<<low_bits);
    static size_t const low_mask = chunk_size-1;
    
    stable_vector() = default;
    stable_vector( stable_vector&& ) = default;
    stable_vector( stable_vector const& ) = delete;

    auto alloc( size_t const size ) -> void*
    {
        if( chunk_offset + size > chunk_size ) {
            *bufs.alloc<char*>() = (char*)POOL_MALLOC( chunk_size );
#ifdef DEBUG_ALLOC
            if( !bufs.back<char*>() ) {
                my_fprintf( stderr, "MALLOC FAILED\n" );
                my_exit( 1 );
            }
            //my_fprintf( stderr, "MALLOC: %p\n", bufs.back<char*>() );
#endif
            chunk_offset = 0;
        }
        auto* p = &bufs.back<char*>()[chunk_offset];
        chunk_offset += size;
        return p;
    }

    ~stable_vector()
    {
        for( size_t i = 0; i < bufs.size(); ++i ) {
            POOL_FREE( bufs.at<char*>( i ), chunk_size );
        }
    }

private:
    fixed_vector bufs;
    size_t chunk_offset{ chunk_size };
};

template< size_t low_bits = 12 >
struct fixed_stable_vector
{
    static size_t const elements_per_chunk = (1<<low_bits);
    static size_t const low_mask = elements_per_chunk-1;

    fixed_vector bufs;
    size_t sz{ 0 };

    fixed_stable_vector() {}
    fixed_stable_vector( fixed_stable_vector&& ) = default;
    fixed_stable_vector( fixed_stable_vector const& ) = delete;

    template< typename T = void >
    auto alloc( size_t const element_size ) -> T*
    {
        size_t const low_idx = (sz&low_mask) * element_size;
        size_t const high_idx = sz>>low_bits;
        if( high_idx >= bufs.size() ) {
            *bufs.alloc<char*>() = (char*)_MALLOC( 
                elements_per_chunk * element_size );
#ifdef DEBUG_ALLOC
            if( !bufs.back<char*>() ) {
                my_fprintf( stderr, "MALLOC FAILED\n" );
                my_exit( 1 );
            }
            //my_fprintf( stderr, "MALLOC: %p\n", bufs.back<char*>() );
#endif
        }
        auto* p = &bufs.back<char*>()[low_idx];
        sz += 1;
        return reinterpret_cast<T*>( p );
    }

    BOOST_FORCEINLINE
    auto get( size_t const i, size_t const element_size ) const -> void*
    {
        size_t const low_idx = (i&low_mask) * element_size;
        size_t const high_idx = i>>low_bits;
        return &bufs.at<char*>( high_idx )[low_idx];
    }

    template< typename T = void >
    BOOST_FORCEINLINE
    auto at( size_t const i, size_t const element_size ) const -> T&
    {
        return *reinterpret_cast<T*>( get( i, element_size ) );
    }

    ~fixed_stable_vector()
    {
        for( size_t i = 0; i < bufs.size(); ++i ) {
            _FREE( bufs.at<char*>( i ) );
        }
    }

    BOOST_FORCEINLINE
    auto size() const { return sz; }
};

static size_t total_term_set_alloc_sz = 0;
struct term_set
{
    typedef object_t mapped_type;

    mapped_type* buckets{nullptr};
    size_t buckets_sz{0};
    fixed_vector vec;

    template< typename TraitsType >
    BOOST_NOINLINE
    auto resize( size_t const elem_sz )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        size_t new_sz = ::std::max<size_t>( 2*buckets_sz, 16 );
        auto* new_buckets = (mapped_type*)POOL_MALLOC( 
            new_sz*sizeof( mapped_type ) );
#ifdef DEBUG_ALLOC
        if( !new_buckets ) {
            my_fprintf( stderr, "MALLOC FAILED\n" );
            my_exit( 1 );
        }
        //my_fprintf( stderr, "MALLOC: %p\n", new_buckets );
#endif
        fill( 
            (int*)( new_buckets ),
            (int*)( ((char*)new_buckets)+new_sz*sizeof( mapped_type ) ),
            traits_type::invalid_idx );

        for( size_t i = 0; i < buckets_sz; ++i ) {
            if( buckets[i] != traits_type::invalid_idx ) {
                mapped_type const head = buckets[i];
                mapped_type idx = head;
                while( idx != traits_type::invalid_idx ) {
                    auto& elem = vec.at<node_type>( idx, elem_sz );

                    size_t const h = traits_type::hash_value( elem )&(new_sz-1);
                    mapped_type& new_head = new_buckets[h];

                    auto const tmp = traits_type::get_next( elem );
                    traits_type::set_next( elem, new_head );
                    new_head = idx;
                    idx = tmp;
                }
            }
        }

#ifdef DEBUG_ALLOC
        total_term_set_alloc_sz += new_sz - buckets_sz;
        my_fprintf( stderr, "DEBUG: term_set resize %d (%d)\n", 
            new_sz, total_term_set_alloc_sz );
#endif

        POOL_FREE( buckets, buckets_sz );
        buckets = new_buckets;
        buckets_sz = new_sz;        
    }

    ~term_set()
    {
        POOL_FREE( buckets, buckets_sz );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto get_key( 
        mapped_type const idx,
        TraitsType& traits ) const
        -> decltype( auto )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        auto& p = vec.at<node_type>( idx, traits.size() );
        return traits_type::get_key( p );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto get_mapped( 
        mapped_type const idx,
        TraitsType& traits ) const
        -> decltype( auto )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        auto& p = vec.at<node_type>( idx, traits.size() );
        return traits_type::get_mapped( p );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto next( 
        size_t& idx,
        TraitsType& traits ) const
        -> bool
    {
        return idx < vec.size();
    }

    BOOST_FORCEINLINE
    auto size() const { return vec.size(); }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto insert( 
        typename TraitsType::insert_type const& x,
        TraitsType& traits )
        -> mapped_type
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        if( (int)vec.size()*LOAD_FACTOR_DENOM >= (int)buckets_sz*LOAD_FACTOR_NUMER ) {
            resize<traits_type>( traits.size() );
        }

        size_t const h = traits_type::hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const& elem = vec.at<node_type>( idx, traits.size() );
            if( traits_type::equals( elem, x ) ) {
                return idx;
            }
            idx = traits_type::get_next( elem );
        }

        mapped_type const newidx = vec.size();
        auto* p = vec.alloc<node_type>( traits.size() );
        traits.construct( p, x );
        traits_type::set_next( *p, head );
        head = newidx;
        return newidx;
    }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto find( 
        typename TraitsType::insert_type const& x,
        TraitsType& traits )
        -> mapped_type
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        if( buckets_sz == 0 ) {
            return traits_type::invalid_idx;
        }

        size_t const h = traits_type::hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const& elem = vec.at<node_type>( idx, traits.size() );
            if( traits_type::equals( elem, x ) ) {
                return idx;
            }
            idx = traits_type::get_next( elem );
        }
        return idx;
    }

    term_set() {}
    term_set( term_set&& ) = default;
    term_set( term_set const& ) = delete;
};

struct term_set_stable
{
    typedef object_t mapped_type;

    mapped_type* buckets{nullptr};
    size_t buckets_sz{0};
    fixed_stable_vector<2> vec;

    template< typename TraitsType >
    BOOST_NOINLINE
    auto resize( size_t const elem_sz )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        size_t new_sz = ::std::max<size_t>( 2*buckets_sz, 16 );
        auto* new_buckets = (mapped_type*)POOL_MALLOC( 
            new_sz*sizeof( mapped_type ) );
#ifdef DEBUG_ALLOC
            if( !new_buckets ) {
                my_fprintf( stderr, "MALLOC FAILED\n" );
                my_exit( 1 );
            }
            //my_fprintf( stderr, "MALLOC: %p\n", new_buckets );
#endif
        fill( 
            (int*)( new_buckets ),
            (int*)( ((char*)new_buckets)+new_sz*sizeof( mapped_type ) ),
            traits_type::invalid_idx );

        for( size_t i = 0; i < buckets_sz; ++i ) {
            if( buckets[i] != traits_type::invalid_idx ) {
                mapped_type const head = buckets[i];
                mapped_type idx = head;
                while( idx != traits_type::invalid_idx ) {
                    auto& elem = vec.at<node_type>( idx, elem_sz );

                    size_t const h = traits_type::hash_value( elem )&(new_sz-1);
                    mapped_type& new_head = new_buckets[h];

                    auto const tmp = traits_type::get_next( elem );
                    traits_type::set_next( elem, new_head );
                    new_head = idx;
                    idx = tmp;
                }
            }
        }

#ifdef DEBUG_ALLOC
        total_term_set_alloc_sz += new_sz - buckets_sz;
        my_fprintf( stderr, "DEBUG: term_set resize %d (%d)\n", 
            new_sz, total_term_set_alloc_sz );
#endif

        POOL_FREE( buckets, buckets_sz );
        buckets = new_buckets;
        buckets_sz = new_sz;        
    }

    ~term_set_stable()
    {
        POOL_FREE( buckets, buckets_sz );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto get_key( 
        mapped_type const idx,
        TraitsType& traits ) const
        -> decltype( auto )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        auto& p = vec.at<node_type>( idx, traits.size() );
        return traits_type::get_key( p );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto get_mapped( 
        mapped_type const idx,
        TraitsType& traits ) const
        -> decltype( auto )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        auto& p = vec.at<node_type>( idx, traits.size() );
        return traits_type::get_mapped( p );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto next( 
        size_t& idx,
        TraitsType& traits ) const
        -> bool
    {
        return idx < vec.size();
    }

    BOOST_FORCEINLINE
    auto size() const { return vec.size(); }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto insert( 
        typename TraitsType::insert_type const& x,
        TraitsType& traits )
        -> mapped_type
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        if( (int)vec.size()*LOAD_FACTOR_DENOM >= (int)buckets_sz*LOAD_FACTOR_NUMER ) {
            resize<traits_type>( traits.size() );
        }

        size_t const h = traits_type::hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const& elem = vec.at<node_type>( idx, traits.size() );
            if( traits_type::equals( elem, x ) ) {
                return idx;
            }
            idx = traits_type::get_next( elem );
        }

        mapped_type const newidx = vec.size();
        auto* p = vec.alloc<node_type>( traits.size() );
        traits.construct( p, x );
        traits_type::set_next( *p, head );
        head = newidx;
        return newidx;
    }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto find( 
        typename TraitsType::insert_type const& x,
        TraitsType& traits )
        -> mapped_type
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;

        if( buckets_sz == 0 ) {
            return traits_type::invalid_idx;
        }

        size_t const h = traits_type::hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const& elem = vec.at<node_type>( idx, traits.size() );
            if( traits_type::equals( elem, x ) ) {
                return idx;
            }
            idx = traits_type::get_next( elem );
        }
        return idx;
    }

    term_set_stable() {}
    term_set_stable( term_set_stable&& ) = default;
    term_set_stable( term_set_stable const& ) = delete;
};

stable_vector<> bitmap_blocks;

auto constexpr log2_c( size_t const n, size_t const p = 0 ) -> size_t
{
    return (n <= 1) ? p : log2_c(n / 2, p + 1);
}

template< size_t Bits >
struct bitmap_block
{
    using word_type = size_t;
    static size_t constexpr bits_per_word = sizeof(word_type)*CHAR_BIT;
    static size_t constexpr num_words = Bits == 0 
        ? 1 : ( Bits - 1 )/bits_per_word + 1;
    static word_type constexpr word_one = 1;
    static word_type constexpr word_one_mask = ~(word_type)0;
    word_type buf[num_words] = {};

    INLINE_VEC_OP
    auto operator[]( size_t const idx ) const -> bool
    {
        return ( buf[idx/bits_per_word]&(word_one << idx%bits_per_word) ) != 0;
    }

    INLINE_VEC_OP
    auto set( size_t const idx, bool const val ) -> bool
    {
        bool old_val = operator[]( idx );
        if( val )
            buf[idx/bits_per_word] |= word_one << idx%bits_per_word;
        else
            buf[idx/bits_per_word] &= ~(word_one << idx%bits_per_word);
        return old_val;
    }

    INLINE_VEC_OP
    auto next( size_t& idx ) const -> bool
    {
        if( idx >= num_words*bits_per_word ) {
            return false;
        }

        // search first word
        auto i = idx/bits_per_word;
        auto w = buf[i];

        // mask off bits below bound
        w &= word_one_mask << idx%bits_per_word;

        if( w ) {
            idx = i*bits_per_word + SIZE_T_CTZ( w );
            return true;
        }

        // check subsequent words
        ++i;
        for( ; i < num_words; ++i ) {
            auto const w = buf[i];
            if( w ) {
                idx = i*bits_per_word + SIZE_T_CTZ( w );
                return true;
            }
        }
        return false;
    }
};

static size_t total_bitmap_internal_alloc_sz = 0;
static size_t total_bitmap_block_alloc_sz = 0;
template< typename Block >
struct bitmap_internal
{
    static size_t constexpr low_bits = log2_c( sizeof( Block )*CHAR_BIT );
    static size_t constexpr low_mask = (1<<low_bits)-1;
    char* buf{nullptr};
    size_t capacity{0};

    BOOST_NOINLINE
    auto resize( size_t const expected )
    {
        auto const old_capacity = capacity;
        while( expected > capacity ) {
            capacity = max<size_t>( 32, capacity*2 );
        }
        buf = (char*)POOL_REALLOC( buf, capacity, old_capacity );
#ifdef DEBUG_ALLOC
        if( !buf ) {
            my_fprintf( stderr, "REALLOC FAILED\n" );
            my_exit( 1 );
        }
        //my_fprintf( stderr, "REALLOC: %p\n", buf );
        total_bitmap_internal_alloc_sz += capacity - old_capacity;
        my_fprintf( stderr, "DEBUG: bitmap resize %d (%d)\n", 
            capacity, total_bitmap_internal_alloc_sz );
#endif
        memset( buf+old_capacity, 0, capacity-old_capacity );
    }

    INLINE_VEC_OP
    auto operator[]( size_t const idx ) const -> bool
    {
        auto const offset = (idx>>low_bits)*sizeof(Block*);
        if( offset >= capacity ) {
            return false;
        }
        auto const* const block = *reinterpret_cast<Block const**>( 
            buf+offset );
        if( !block ) {
            return false;
        }
        return (*block)[idx&low_mask];
    }

    INLINE_VEC_OP
    auto set( size_t const idx, bool const val ) -> bool
    {
        auto const offset = (idx>>low_bits)*sizeof(Block*);
        if( offset >= capacity ) {
            resize( offset+sizeof(Block*) );
        }
        auto*& block = *reinterpret_cast<Block**>( buf+offset );
        if( !block ) {
            block = (Block*)bitmap_blocks.alloc( sizeof( Block ) );
            new (block) Block;

#ifdef DEBUG_ALLOC
            total_bitmap_block_alloc_sz += sizeof( Block );
            my_fprintf( stderr, "DEBUG: bitmap block alloc (%d)\n", 
                total_bitmap_block_alloc_sz );
#endif
        }
        return block->set( idx&low_mask, val );
    }

    INLINE_VEC_OP
    auto next( size_t& idx ) const -> bool
    {
        auto curr_idx = idx;
        for( auto high_idx = curr_idx>>low_bits; 
            high_idx < capacity/sizeof(Block*);
            ++high_idx )
        {
            auto const* const block = *reinterpret_cast<Block const**>( 
                buf+high_idx*sizeof(Block*) );
            if( block ) {
                size_t low_idx = curr_idx&low_mask;
                if( block->next( low_idx ) ) {
                    idx = (high_idx<<low_bits)|low_idx;
                    return true;
                }
            }
            curr_idx = 0;
        }
        return false;
    }
};

using bitmap = bitmap_internal<bitmap_block<512>>;

struct ch_hash_set
{
    typedef object_t mapped_type;

    mapped_type* buckets{nullptr};
    size_t buckets_sz{0};
    fixed_vector vec;

    ~ch_hash_set()
    {
        POOL_FREE( buckets, buckets_sz );
    }

    template< typename TraitsType >
    BOOST_NOINLINE
    auto resize()
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        size_t new_sz = ::std::max<size_t>( 2*buckets_sz, 16 );
        mapped_type* new_buckets =
            (mapped_type*)POOL_MALLOC( new_sz*sizeof( mapped_type ) );
#ifdef DEBUG_ALLOC
            if( !new_buckets.get() ) {
                my_fprintf( stderr, "MALLOC FAILED\n" );
                my_exit( 1 );
            }
            //my_fprintf( stderr, "MALLOC: %p\n", new_buckets.get() );
#endif
        fill( 
            (int*)( new_buckets ),
            (int*)( ((char*)new_buckets)+new_sz*sizeof( mapped_type ) ),
            traits_type::invalid_idx );

        for( size_t i = 0; i < buckets_sz; ++i ) {
            if( buckets[i] != traits_type::invalid_idx ) {
                mapped_type const head = buckets[i];
                mapped_type idx = head;
                while( idx != traits_type::invalid_idx ) {
                    auto& elem = vec.at<node_type>( idx );

                    size_t const h = traits_type::hash_value( elem )&(new_sz-1);
                    mapped_type& new_head = new_buckets[h];

                    auto const tmp = traits_type::get_next( elem );
                    traits_type::set_next( elem, new_head );
                    new_head = idx;
                    idx = tmp;
                }
            }
        }
        POOL_FREE( buckets, buckets_sz );
        buckets = new_buckets;
        buckets_sz = new_sz;
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto get( 
        size_t const idx ) const
        -> decltype( auto )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        auto& p = vec.at<node_type>( idx );
        return traits_type::get_key( p );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto next( 
        size_t& idx ) const
        -> bool
    {
        return idx < vec.size();
    }

    BOOST_FORCEINLINE
    auto size() const { return vec.size(); }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto insert( 
        typename TraitsType::insert_type const& x )
        -> bool
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        if( (int)vec.size()*LOAD_FACTOR_DENOM >= (int)buckets_sz*LOAD_FACTOR_NUMER ) {
            resize<traits_type>();
        }

        size_t const h = traits_type::hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const& elem = vec.at<node_type>( idx );
            if( traits_type::equals( elem, x ) ) {
                return false;
            }
            idx = traits_type::get_next( elem );
        }

        mapped_type const newidx = vec.size();
        auto* p = vec.alloc<node_type>();
        traits_type::construct( p, x );
        traits_type::set_next( *p, head );
        head = newidx;
        return true;
    }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto find( 
        typename TraitsType::insert_type const& x )
        -> bool
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        if( buckets_sz == 0 ) {
            return false;
        }

        size_t const h = traits_type::hash_value( x )&(buckets_sz-1);
        mapped_type& head = buckets[h];
        mapped_type idx = head;
        while( idx != traits_type::invalid_idx ) {
            auto const& elem = vec.at<node_type>( idx );
            if( traits_type::equals( elem, x ) ) {
                return true;
            }
            idx = traits_type::get_next( elem );
        }
        return false;
    }

    ch_hash_set() {}
    ch_hash_set( ch_hash_set&& ) = default;
    ch_hash_set( ch_hash_set const& ) = delete;
};

struct oa_hash_set
{
    typedef object_t mapped_type;

    char* buckets{nullptr};
    size_t buckets_sz{0};
    mapped_type sz{0};

    ~oa_hash_set()
    {
        POOL_FREE( buckets, buckets_sz );
    }

    template< typename TraitsType >
    BOOST_NOINLINE
    auto resize()
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::value_type value_type;

        size_t new_sz = ::std::max<size_t>( 2*buckets_sz, 16 );
        char* new_buckets = (char*)POOL_MALLOC( new_sz*sizeof( value_type ) );
#ifdef DEBUG_ALLOC
            if( !new_buckets ) {
                my_fprintf( stderr, "MALLOC FAILED\n" );
                my_exit( 1 );
            }
            //my_fprintf( stderr, "MALLOC: %p\n", new_buckets );
#endif
        fill( 
            (int*)( new_buckets ),
            (int*)( new_buckets+new_sz*sizeof( value_type ) ),
            traits_type::invalid_idx );

        auto* buckets_p = reinterpret_cast<value_type*>( buckets );
        auto* new_buckets_p = reinterpret_cast<value_type*>( new_buckets );
        for( size_t i = 0; i < buckets_sz; ++i ) {
            if( buckets_p[i].front() != traits_type::invalid_idx ) {
                size_t idx = traits_type::hash_value( buckets_p[i] )&(new_sz-1);
#if defined(QUADRATIC_PROBING)
                for( size_t j = 0;; j+=1, idx = (idx+j*j)&(new_sz-1) ) {
#else
                for( ;; idx = (idx+1)&(new_sz-1) ) {
#endif
                    auto* p = new_buckets_p + idx;
                    if( p->front() == traits_type::invalid_idx ) {
                        traits_type::construct( p, buckets_p[i] );
                        break;
                    }
                }

            }
        }
        POOL_FREE( buckets, buckets_sz );
        buckets = new_buckets;
        buckets_sz = new_sz;
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto get( 
        size_t& idx ) const
        -> decltype( auto )
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        return *( reinterpret_cast<node_type*>( buckets ) + idx );
    }

    template< typename TraitsType >
    BOOST_FORCEINLINE
    auto next( 
        size_t& idx ) const
        -> bool
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;
        for( ; idx < buckets_sz; idx += 1 ) {
            auto* p = reinterpret_cast<node_type*>( buckets ) + idx;
            if( p->front() != traits_type::invalid_idx ) {
                return true;
            }
        }
        return false;
    }

    BOOST_FORCEINLINE
    auto size() const { return sz; }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto insert( 
        typename TraitsType::insert_type const& x )
        -> bool
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        if( (int)sz*LOAD_FACTOR_DENOM >= (int)buckets_sz*LOAD_FACTOR_NUMER ) {
            resize<traits_type>();
        }
        //size_t scanned = 0;
        size_t ihash;
        size_t idx = ihash = traits_type::hash_value( x )&(buckets_sz-1);
#if defined(QUADRATIC_PROBING)
        for( size_t j = 0;; j+=1, idx = (idx+j*j)&(buckets_sz-1) ) {
#else
        for( ;; idx = (idx+1)&(buckets_sz-1) ) {
#endif
            //scanned += 1;
            auto* p = reinterpret_cast<node_type*>( buckets ) + idx;
            if( p->front() == traits_type::invalid_idx ) {
                traits_type::construct( p, x );
                sz += 1;
                //TRACE( "insert succeeded hash: %zu idx: %zu scanned: %zu sz: %zu buckets: %zu\n", 
                //    ihash, idx, scanned, sz, buckets_sz );
                return true;
            }
            if( traits_type::equals( *p, x ) ) {
                return false;
            }
        }
    }

    template< typename TraitsType >
    INLINE_HASH_OP
    auto find( 
        typename TraitsType::insert_type const& x )
        -> bool
    {
        typedef TraitsType traits_type;
        typedef typename traits_type::node_type node_type;
        typedef typename traits_type::value_type value_type;

        if( buckets_sz == 0 ) {
            return false;
        }

        size_t idx = traits_type::hash_value( x )&(buckets_sz-1);
#if defined(QUADRATIC_PROBING)
        for( size_t j = 0;; j+=1, idx = (idx+j*j)&(buckets_sz-1) ) {
#else
        for( ;; idx = (idx+1)&(buckets_sz-1) ) {
#endif
            auto* p = reinterpret_cast<node_type*>( buckets ) + idx;
            if( p->front() == traits_type::invalid_idx ) {
                return false;
            }
            if( traits_type::equals( *p, x ) ) {
                return true;
            }
        }
    }

    oa_hash_set() {}
    oa_hash_set( oa_hash_set&& ) = default;
    oa_hash_set( oa_hash_set const& ) = delete;
};

struct string_term_traits
{
    typedef string_ref insert_type;
    typedef string_ref value_type;
    typedef object_t mapped_type;

    stable_vector<18> pool;
    struct node_type 
    {
        mapped_type next;
        string_ref value;
        node_type( node_type const& ) = delete;
    };
    static mapped_type const invalid_idx;

    auto static size() -> size_t
    {
        return sizeof( node_type );
    }
    auto static set_next( node_type& p, mapped_type const next ) -> void 
    {
        p.next = next;
    }
    auto static get_next( node_type const& p ) -> mapped_type 
    {
        return p.next;
    }
    auto static get_key( node_type const& p ) -> string_ref
    {
        return p.value;
    }
    auto construct( node_type* const p, value_type const& x ) -> void
    {
        if( x.length() > pool.chunk_size ) {
            my_fprintf( stderr, "String too big. Got: %zu. Max length: %zu.\n",
                x.length(), pool.chunk_size );
            exit( 1 );
        }
        auto* buf = reinterpret_cast<char*>( pool.alloc( x.length() + 1 ) );
        memcpy( buf, x.data(), x.length() );
        buf[x.length()] = 0;
        p->value = string_ref( buf, x.length() );
    }
    auto static hash_value( node_type const& p ) -> size_t 
    { 
        return hash_value( p.value );
    }
    auto static hash_value( value_type const& p ) -> size_t 
    { 
        return ::boost::hash<string_ref>()( p );
    }

    auto static equals( node_type const& l, value_type const& r ) -> bool
    {
        return l.value == r;
    }
};

string_term_traits::mapped_type const string_term_traits::invalid_idx
    = (string_term_traits::mapped_type)INT_MAX;

template< size_t I >
struct uint_vec_traits
{
    typedef array<uint32_t,I> value_type;
    typedef array<uint32_t,I> insert_type;
    struct compound_hook
    {
        object_t next;
        value_type const value;
        compound_hook( compound_hook const& ) = delete;
    };

    typedef compound_hook node_type;
    static object_t const invalid_idx;

    auto static size() -> size_t
    {
        return sizeof( node_type );
    }
    auto static set_next( node_type& p, object_t const next ) -> void 
    {
        p.next = next;
    }
    auto static get_next( node_type const& p ) -> object_t 
    {
        return p.next;
    }

    auto static get_key( node_type const& p ) -> value_type const&
    {
        return p.value;
    }

    auto static construct( 
        node_type* const p, 
        value_type const& x ) -> void
    {
        new( (void*)&p->value ) value_type( x );
    }
    auto static hash_value( 
        node_type const& x )
        -> size_t 
    {
        return hash_value( x.value );
    }
    auto static hash_value( uint32_t const x ) -> size_t
    {
        return ::boost::hash<uint32_t>()( x );
    }
    auto static hash_value( value_type const& x ) -> size_t
    {
        uint32_t val = offset_basis;
        if( I > 0 ) {
//            val = hash_value( x[0] );
            for(size_t i = 0; i < I; ++i)
            {	// fold in another byte
                val ^= (uint32_t)x[i];
                val *= prime;
            }
        }
        return val;
        //size_t seed = hash_value( x[0] );
        //for( size_t i = 1; i < I; ++i ) {
        //    seed ^= hash_value( x[i] ) + 0x9e3779b9 + (seed<<6) + (seed>>2);
        //}
        //return seed;
    }

    auto static equals( node_type const& l, value_type const& r ) -> bool
    {
        return equals( l.value, r );
    }

    auto static equals( value_type const& l, value_type const& r ) -> bool
    {
        return l == r;
    }
};

template< size_t I >
object_t const uint_vec_traits<I>::invalid_idx = (object_t)INT_MAX;

template< size_t I >
struct oa_uint_vec_traits
{
    typedef array<uint32_t,I> value_type;
    typedef array<uint32_t,I> insert_type;
    typedef array<uint32_t,I> node_type;
    static uint32_t const invalid_idx;

    auto static size() -> size_t
    {
        return sizeof( node_type );
    }
    auto static construct( 
        node_type* const p, 
        value_type const& x ) -> void
    {
        new( p ) value_type( x );
    }
    auto static hash_value( uint32_t x ) -> size_t
    {
        return ::boost::hash<uint32_t>()( x );
    }
    auto static hash_value( value_type const& x ) -> size_t
    { 
        uint32_t val = offset_basis;
        if( I > 0 ) {
            //val = hash_value( x[0] );
            for(size_t i = 0; i < I; ++i)
            {	// fold in another byte
                val ^= (uint32_t)x[i];
                val *= prime;
            }
        }
        return val;
        //size_t seed = hash_value( x[0] );
        //for( size_t i = 1; i < I; ++i ) {
        //    seed ^= hash_value( x[i] ) + 0x9e3779b9 + (seed<<6) + (seed>>2);
        //}
        //return seed;
    }
    auto static equals( value_type const& l, value_type const& r ) -> bool
    {
        return l == r;
    }
};

template< size_t I >
uint32_t const oa_uint_vec_traits<I>::invalid_idx = (uint32_t)INT_MAX;

template< size_t I >
struct uint_vec_mapped_traits
{
    size_t const mapped_sz;

    typedef array<uint32_t,I> insert_type;
    struct node_type
    {
        object_t next;
        insert_type key;
        node_type( node_type const& ) = delete;
    };
    static object_t const invalid_idx;

    auto size() const -> size_t
    {
        return sizeof( node_type ) + mapped_sz;
    }
    auto static set_next( node_type& p, object_t const next ) -> void 
    {
        p.next = next;
    }
    auto static get_next( node_type const& p ) -> object_t 
    {
        return p.next;
    }
    auto static get_key( node_type& p ) -> insert_type const&
    {
        return p.key;
    }
    auto static get_mapped( node_type& p ) -> void*
    {
        return &p+1;
    }
    auto static construct( 
        node_type* const p, 
        insert_type const& x ) -> void
    {
        new( &p->key ) insert_type{ x };
    }

    auto static hash_value( 
        node_type const& x )
        -> size_t 
    {
        return hash_value( x.key );
    }

    auto static hash_value( uint32_t const x ) -> size_t
    {
        return ::boost::hash<uint32_t>()( x );
    }

    auto static hash_value( insert_type const& x ) -> size_t
    { 
        uint32_t val = offset_basis;
        if( I > 0 ) {
//            val = hash_value( x[0] );
            for(size_t i = 0; i < I; ++i)
            {	// fold in another byte
                val ^= (uint32_t)x[i];
                val *= prime;
            }
        }
        return val;
        //size_t seed = hash_value( x[0] );
        //for( size_t i = 1; i < I; ++i ) {
        //    seed ^= hash_value( x[i] ) + 0x9e3779b9 + (seed<<6) + (seed>>2);
        //}
        //return seed;
    }

    auto static equals( node_type const& l, insert_type const& r ) -> bool
    {
        return equals( l.key, r );
    }

    auto static equals( insert_type const& l, insert_type const& r ) -> bool
    {
        return l == r;
    }
};

template< size_t I >
object_t const uint_vec_mapped_traits<I>::invalid_idx = (object_t)INT_MAX;

template< size_t I, typename... Args, ::std::size_t... Is  >
auto copy_from_helper( 
    ::std::index_sequence<Is...>,
    array<uint32_t,I> const& arr,
    Args*... args ) 
    -> void
{
    using dummy = uint32_t[];
    (void)dummy{ 0, ( *args = arr[Is] )... };
}

template< size_t I, typename... Args >
auto copy_from(
    array<uint32_t, I> const& arr,
    Args*... args )
    -> void
{
    copy_from_helper(
        ::std::index_sequence_for<Args...>{},
        arr,
        args... );
}

struct worklist
{
    fixed_vector curr;
    fixed_vector next;
};

//
// Data constructors
//
template< size_t I, typename... Args >
INLINE_WRAPPER
auto pack(
    term_set* const p,
    dconstr_tag_t const tag,
    Args const... args )
    -> tagged_object_t
{
    uint_vec_traits<I> traits;
    array<uint32_t,I> elem{ args... };
    return get_tagged_id( tag, p->insert( elem, traits ) );
}

#define pack_n( z, n, unused )              \
extern "C" tagged_object_t pack_ ## n(      \
    term_set* p,                            \
    dconstr_tag_t tag                      \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t i) )   \
{                                           \
    return pack<n>( p, tag                \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, i) );       \
}

BOOST_PP_REPEAT(MAX_ARITY,pack_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
auto unpack(
    term_set* const p,
    tagged_object_t const id,
    dconstr_tag_t const tag,
    Args*... args )
    -> bool
{
    if( get_dconstr_tag( id ) != tag ) {
        return false;
    }

    uint_vec_traits<I> traits;
    copy_from( 
        p->get_key( get_object_idx( id ), traits ),
        args... );
    return true;
}

#define unpack_n( z, n, unused )            \
extern "C" bool unpack_ ## n(               \
    term_set* const p,                      \
    tagged_object_t const id,               \
    dconstr_tag_t const tag                \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t* arg) )              \
{                                           \
    return unpack<n>( p, id, tag           \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) );  \
}

BOOST_PP_REPEAT(MAX_ARITY,unpack_n,~)

//
// List
//
template< size_t I, typename... Args >
INLINE_WRAPPER
auto insert_list( 
    fixed_vector* const p, 
    Args const... args ) 
    -> void
{
    *p->alloc<array<uint32_t,I>>() = array<uint32_t,I>{ args... };
}

#define insert_list_n( z, n, unused )       \
extern "C" void insert_list_ ## n(          \
    fixed_vector* p                        \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t arg) )   \
{                                           \
    TRACE( "il" BOOST_PP_REPEAT(n,PARM3," %d") "\n" \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) );     \
    insert_list<n>(                  \
        p BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) );    \
    TRACE( "exit il\n" ); \
}

BOOST_PP_REPEAT(MAX_ARITY,insert_list_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
auto unique_insert_list( 
    fixed_vector* const p, 
    Args const... args ) 
    -> bool
{
    auto const vec = array<uint32_t,I>{ args... };
    auto const sz = p->size();
    for( size_t i = 0; i < sz; ++i ) {
        if( vec == p->at<array<uint32_t, I>>( i ) ) {
            return false;
        }
    }
    *p->alloc<array<uint32_t,I>>() = vec;
    return true;
}

#define unique_insert_list_n( z, n, unused )       \
extern "C" bool unique_insert_list_ ## n(          \
    fixed_vector* p                        \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t arg) )   \
{                                           \
    bool const res = unique_insert_list<n>(                  \
        p BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) );    \
    TRACE( "uil" BOOST_PP_REPEAT(n,PARM3," %d") " %s\n" \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg), res?"true":"false" );     \
    return res; \
}

BOOST_PP_REPEAT(MAX_ARITY,unique_insert_list_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
auto next_list(
    fixed_vector* const p,
    size_t const i,
    Args*... args )
    -> bool
{
    if( i >= p->size() ) {
        return false;
    }
    copy_from( 
        p->at<array<uint32_t,I>>( i ),
        args... );
    return true;
}

#define next_list_n( z, n, unused )         \
extern "C" bool next_list_ ## n(            \
    fixed_vector* p,                        \
    size_t* const i                               \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t* arg) )  \
{                                           \
    bool const res = next_list<n>( \
        p, *i BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
    *i += 1; \
    if( res ) TRACE( "nl" BOOST_PP_REPEAT(n,PARM3," %d") "\n" \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, *arg) ); \
    return res; \
}

BOOST_PP_REPEAT(MAX_ARITY,next_list_n,~)

//
// Hash set
//
template< size_t I, typename... Args >
INLINE_WRAPPER
auto unique_insert_hash_set( 
    hash_set* const p, 
    Args const... args ) 
    -> bool
{
    array<uint32_t,I> elem{ args... };
    return p->insert<hash_set_traits<I>>( elem );
}

#define unique_insert_hash_set_n( z, n, unused )       \
extern "C" bool unique_insert_hash_set_ ## n(          \
    hash_set* p                        \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t arg) )   \
{                                           \
    bool const res = unique_insert_hash_set<n>(                  \
        p BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) );    \
    TRACE( "uih" BOOST_PP_REPEAT(n,PARM3," %d") " %s\n" \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg), res?"true":"false" );     \
    return res; \
}

BOOST_PP_REPEAT(MAX_ARITY,unique_insert_hash_set_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
auto lookup_hash_set( 
    hash_set* const p, 
    Args const... args ) 
    -> bool
{
    array<uint32_t,I> elem{ args... };
    return p->find<hash_set_traits<I>>( elem );
}

#define lookup_hash_set_n( z, n, unused )       \
extern "C" bool lookup_hash_set_ ## n(          \
    hash_set* p                        \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t arg) )   \
{                                           \
    bool const res = lookup_hash_set<n>(                  \
        p BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) );    \
    TRACE( "lh" BOOST_PP_REPEAT(n,PARM3," %d") " %s\n" \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg), res?"true":"false" );     \
    return res; \
}

BOOST_PP_REPEAT(MAX_ARITY,lookup_hash_set_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
auto next_hash_set(
    hash_set* const p,
    size_t& i,
    Args*... args )
    -> bool
{
    if( !p->next<hash_set_traits<I>>( i ) ) {
        return false;
    }

    copy_from( 
        p->get<hash_set_traits<I>>( i ),
        args... );
    return true;
}

#define next_hash_set_n( z, n, unused )         \
extern "C" bool next_hash_set_ ## n(            \
    hash_set* p,                        \
    size_t* const i                               \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t* arg) )  \
{                                           \
    bool const res = next_hash_set<n>( \
        p, *i BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
    *i += 1; \
    if( res ) TRACE( "nh" BOOST_PP_REPEAT(n,PARM3," %d") "\n" \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, *arg) ); \
    return res; \
}

BOOST_PP_REPEAT(MAX_ARITY,next_hash_set_n,~)

//
// Bitmap
//
extern "C" bool unique_insert_bitmap_1( 
    bitmap* const p, 
    uint32_t const arg ) 
{
    return !p->set( arg, true );
}

extern "C" bool lookup_bitmap_1( 
    bitmap* const p, 
    uint32_t const arg )
{
    return (*p)[arg];
}

extern "C" bool next_bitmap_1(
    bitmap* const p, 
    size_t& i,
    uint32_t* arg )
{
    if( !p->next( i ) ) {
        return false;
    }

    *arg = i;
    i += 1;
    return true;
}

//
// Array map
//
extern "C" void* lookup_insert_array_1(
    fixed_stable_vector<>* p,
    size_t const element_size,
    void (*init_func)( void* ),
    uint32_t const arg )
{
    TRACE( "lia %d %d\n", element_size, arg );
    while( !( arg < p->size() ) ) {
        (*init_func)( p->alloc( element_size ) );
    }
    auto* ret = p->get( arg, element_size );
    TRACE( "exit lia %p\n", ret );
    return ret;
}

extern "C" void* lookup_array_1(
    fixed_stable_vector<>* p,
    size_t const element_size,
    uint32_t const arg )
{
    TRACE( "la %d %d\n", element_size, arg );
    if( arg < p->size() ) {
        return p->get( arg, element_size );
    }
    return nullptr;
}

extern "C" void* next_array_1(
    fixed_stable_vector<>* p,
    size_t* iter,
    size_t const element_size,
    uint32_t* const arg )
{
    if( *iter < p->size() ) {
        *arg = (uint32_t)*iter;
        TRACE( "na %d %d\n", element_size, *arg );
        return p->get( (*iter)++, element_size );
    }
    return nullptr;
}

//
// Hash map
//
template< size_t I, typename... Args >
INLINE_WRAPPER
void* lookup_insert_hash(
    term_set_stable* p,
    size_t const element_size,
    void (*init_func)( void* ),
    Args const... args )
{
    uint_vec_mapped_traits<I> traits{ element_size };
    array<uint32_t,I> elem{ args... };

    auto const next_obj_id = p->size();
    auto const obj_id = p->insert( elem, traits );
    auto* index = p->get_mapped( obj_id, traits );
    if( obj_id == next_obj_id ) {
        (*init_func)( index );
    }
    return index;
}

#define lookup_insert_hash_n( z, n, unused )         \
extern "C" void* lookup_insert_hash_ ## n( \
    term_set_stable* p, \
    size_t const element_size, \
    void( *init_func )(void*) \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t arg) ) \
{                                       \
    TRACE( "lihm %d" BOOST_PP_REPEAT(n,PARM3," %d") "\n", element_size \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
    return lookup_insert_hash<n>( p, element_size, init_func \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
}

BOOST_PP_REPEAT(MAX_ARITY,lookup_insert_hash_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
void* lookup_hash(
    term_set_stable* p,
    size_t const element_size,
    Args const... args )
{
    uint_vec_mapped_traits<I> traits{ element_size };
    array<uint32_t,I> elem{ args... };

    auto const obj_id = p->find( elem, traits );
    if( obj_id != traits.invalid_idx ) {
        return p->get_mapped( obj_id, traits );
    }
    return nullptr;
}

#define lookup_hash_n( z, n, unused )         \
extern "C" void* lookup_hash_ ## n( \
    term_set_stable* p,                    \
    size_t const element_size   \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t arg) ) \
{                               \
    TRACE( "lhm %d" BOOST_PP_REPEAT(n,PARM3," %d") "\n", element_size \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
    return lookup_hash<n>( p, element_size \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
}

BOOST_PP_REPEAT(MAX_ARITY,lookup_hash_n,~)

template< size_t I, typename... Args >
INLINE_WRAPPER
void* next_hash(
    term_set_stable* p,
    size_t* iter,
    size_t const element_size,
    Args* const... args )
{
    uint_vec_mapped_traits<I> traits{ element_size };
    if( !p->next( *iter, traits ) ) {
        return nullptr;
    }

    copy_from( 
        p->get_key( *iter, traits ),
        args... );
    auto* ret = p->get_mapped( *iter, traits );
    *iter += 1;
    return ret;
}

#define next_hash_n( z, n, unused )         \
extern "C" void* next_hash_ ## n(           \
    term_set_stable* p,                            \
    size_t* iter,                           \
    size_t const element_size              \
    BOOST_PP_ENUM_TRAILING_PARAMS(n, uint32_t* arg) )  \
{                                           \
    TRACE( "nhm %d %d\n", element_size, *iter );    \
    return next_hash<n>( p, iter, element_size  \
        BOOST_PP_ENUM_TRAILING_PARAMS(n, arg) ); \
}

BOOST_PP_REPEAT(MAX_ARITY,next_hash_n,~)

//
// Worklists
//
extern "C" size_t swap_worklist( worklist* p )
{
    TRACE( "swap %d %d\n", p->curr.size(), p->next.size() );
    p->curr.clear();
    p->curr.swap( p->next );
    return p->curr.size();
}

//
// Environment
//
struct eval_environment
{
    string_term_traits string_traits;
    term_set string_set;
    fixed_vector file_handles;
    FILE* show_fd{ nullptr };
};

struct output_file_context
{
    FILE* file;
    char const* file_name;
};

extern "C" void init_env( eval_environment* p )
{
    // Force function visibility
    my_printf( "" );
    new( p ) eval_environment;
    *p->file_handles.alloc<output_file_context>() = { stdin, "stdin" };
    *p->file_handles.alloc<output_file_context>() = { stdout, "stdout" };
    *p->file_handles.alloc<output_file_context>() = { stderr, "stderr" };
}

extern "C" void destroy_env( eval_environment* p )
{
    for( size_t i = 3; i < p->file_handles.size(); ++i ) {
        auto& f = p->file_handles.at<output_file_context>( i );
        my_fclose( f.file );
    }
}

extern "C" void open_file_handle( 
    eval_environment* p,
    char const* const file_name )
{
    auto* f = my_fopen( file_name, "w" );
    if( !f ) {
        my_fprintf( stderr, "Error opening file '%s.\n", file_name );
        my_exit( 1 );
    }
    *p->file_handles.alloc<output_file_context>() = { f, file_name };
}

#ifdef USE_HIGHPERFORMANCE_TIMER
#else
clock_t start_time;
#endif


extern "C" void reset_timer()
{
#ifdef USE_HIGHPERFORMANCE_TIMER
    high_performance_reset_timer();
#else
    start_time = clock();
#endif
}

extern "C" void print_timer()
{
#ifdef USE_HIGHPERFORMANCE_TIMER
    high_performance_print_timer();
#else
    double const elapsed = clock() - start_time;
    my_printf( "%.6f", elapsed/CLOCKS_PER_SEC );
#endif
    my_fflush( stdout );
}

extern "C" void init_list( fixed_vector* p )
{
    new( p ) fixed_vector;
}

extern "C" void init_term_set( term_set* p )
{
    new( p ) term_set;
}

extern "C" void init_term_set_stable( term_set_stable* p )
{
    new( p ) term_set_stable;
}

extern "C" void init_hash_set( hash_set* p )
{
    new( p ) hash_set;
}

extern "C" void init_bitmap( bitmap* p )
{
    new( p ) bitmap;
}

extern "C" void init_array( fixed_stable_vector<>* p )
{
    new( p ) fixed_stable_vector<>;
}

extern "C" void init_worklist( worklist* p )
{
    new( p ) worklist;
}

extern "C" void put_string( eval_environment* p, char const* str, size_t sz )
{
    p->string_set.insert( string_ref( str, sz ), p->string_traits );
}

extern "C" uint32_t cat_string( eval_environment* p, uint32_t const idx1, uint32_t const idx2 )
{
    char buf[1024*128];
    auto const str1 = p->string_set.get_key( idx1, p->string_traits );
    auto const str2 = p->string_set.get_key( idx2, p->string_traits );
	if( str1.size() + str2.size() > sizeof(buf) ) {
        my_fprintf( stderr, "Concatenation of strings overflows buffer.\n" );
        my_exit( 1 );
	}
	memcpy( buf, str1.data(), str1.size() );
	memcpy( buf+str1.size(), str2.data(), str2.size() );

	return p->string_set.insert( 
        string_ref( buf, str1.size()+str2.size() ),
        p->string_traits );
}

extern "C" uint32_t match_string( eval_environment* eval, uint32_t const sidx, uint32_t const pidx )
{
    auto const str = eval->string_set.get_key( sidx, eval->string_traits );
    auto const pat = eval->string_set.get_key( pidx, eval->string_traits );
	char const* star = nullptr;
	char const* ss = str.begin();
	char const* s = str.begin(), *p = pat.begin();
	while( s != str.end() ){
		if( p != pat.end() && ( ( *p=='?' ) || ( *p == *s ) ) ) {
			s++;
			p++;
			continue;
		} 

		if( p != pat.end() && *p=='*' ) { 
			star=p++; 
			ss=s;
			continue;
		} 

		if( star ) { 
			p = star + 1; 
			s = ++ss;
			continue;
		} 
		return false;
	}

	while( p != pat.end() && *p=='*' ) { 
		p++;
	}

	return p == pat.end();  
}

extern "C" int write_formatted(
    eval_environment* p, int32_t i, 
    char const* fmt, ... )
{
    va_list argp;
    va_start( argp, fmt );
    auto& f = p->file_handles.at<output_file_context>( i );
    assert( f.file );
    auto const ret = my_vfprintf( f.file, fmt, argp );
    va_end( argp );
    return ret;
}

extern "C" void set_show_fd(
    eval_environment* p, int32_t i )
{
    p->show_fd = p->file_handles.at<output_file_context>( i ).file;
    assert( p->show_fd );
}

extern "C" void show_cstring( 
    eval_environment* p, char const* str )
{
    my_fputs( str, p->show_fd );
}

extern "C" void show_i32( 
    eval_environment* p, int32_t val )
{
    my_fprintf( p->show_fd, "%" PRIi32, val );
}

extern "C" void show_string( 
    eval_environment* p, uint32_t idx )
{
    auto const str = p->string_set.get_key( idx, p->string_traits );
    my_fwrite( str.data(), 1, str.size(), p->show_fd );
}

extern "C" void _assert()
{
    assert( false );
}

extern "C" void flush_file(
    eval_environment* p, int32_t i )
{
    auto& f = p->file_handles.at<output_file_context>( i );
    assert( f.file );
    my_fflush( f.file );
}

struct file_context
{
    FILE* file;
    char const* file_name;
    char const* mode;
    size_t num_args;
    uint32_t* types;
    long lines;
};

extern "C" void open_file( 
    file_context* ctxt, 
    char const* file_name,
    char const* mode,
    size_t const num_args,
    uint32_t* types )
{
    auto* const full_file_name = find_file( file_name );
    ctxt->file = strcmp( file_name, "/dev/stdin" ) == 0 ? stdin
        : strcmp( file_name, "/dev/stdout" ) == 0 ? stdout
        : strcmp( file_name, "/dev/stderr" ) == 0 ? stderr
        : my_fopen( full_file_name, mode );
    ctxt->file_name = full_file_name;
    ctxt->mode = mode;
    ctxt->num_args = num_args;
    ctxt->types = types;
    ctxt->lines = 0;
    if( !ctxt->file ) {
        my_fprintf( stderr, "Warning: Error opening file '%s.\n", 
            ctxt->file_name );
    }
    //my_fprintf( stdout, "'%s': Opened with %ld columns...\n", 
    //    ctxt->file_name, (long)ctxt->num_args );
}

extern "C" bool scan_file(
    eval_environment* p,
    file_context* ctxt,
    uint32_t* args )
{
    if( !ctxt->file ) {
        return false;
    }

    char line[1024*128];
    if( !my_fgets( line, sizeof(line), ctxt->file ) ) {
        return false;
    }
    auto const len = strnlen( line, sizeof(line) );
    if( len == 0 || line[len-1] != '\n' ) {
        my_fprintf( stderr, "Error reading file '%s' at position %ld.\n", 
            ctxt->file_name, ftell( ctxt->file ) );
        my_exit( 1 );
    }

    auto str = string_ref( line, len );
    for( unsigned i = 0; i < ctxt->num_args; ++i ) {
        auto const field_len = str.find_first_of( "\n\t" );
        if( field_len == str.npos ) {
            my_fprintf( stderr, 
                "Unexpected end of line reading line %ld of "
                "file '%s'.\n", ctxt->lines, ctxt->file_name );
            my_exit( 1 );
        }

        if( ctxt->types[i] == 0 ) {
            char buf[13];
            if( !(field_len + 1 < sizeof( buf ) ) ) {
                my_fprintf( stderr, 
                    "Invalid i32 field of line %ld of file '%s'.\n",
                    ctxt->lines, ctxt->file_name );
                my_exit( 1 );
            }
            memcpy( buf, str.data(), field_len );
            buf[field_len] = 0;
            args[i] = atoi( buf );
        }
        else if( ctxt->types[i] == 1 ) {
            args[i] = p->string_set.insert( 
                string_ref( str.data(), field_len ),
                p->string_traits );
        }
        else {
            assert( false );
        }
        str = string_ref( str.data() + field_len + 1 );
    }
	if( !str.empty() ) {
        my_fprintf( stderr, 
			"Unexpected additional field while reading line %ld of "
            "file '%s'.\n", ctxt->lines, ctxt->file_name );
        my_exit( 1 );	
	}
    ctxt->lines += 1;
    return true;
}

extern "C" void close_file( file_context* ctxt )
{
    //my_fprintf( stdout, "'%s': Read %ld facts.\n", 
    //    ctxt->file_name, ctxt->lines );
    if( ctxt->file ) {
        my_fclose( ctxt->file );
    }
}
