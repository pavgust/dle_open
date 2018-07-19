#include "stdafx.h"
#include <bitset>

#include "utility.h"
#include "ds.h"
#include "rootref.h"

#ifdef _MSC_VER
#include <Windows.h>
auto inline reserve( size_t const sz ) -> void*
{
    return VirtualAlloc( NULL, sz, MEM_RESERVE, PAGE_NOACCESS );
}
auto inline commit( void* p, size_t const sz ) -> void
{
    VirtualAlloc( p, sz, MEM_COMMIT, PAGE_READWRITE );
}
#else
auto inline reserve( size_t const sz ) -> void*
{
    return nullptr;
}
auto inline commit( void* p, size_t const sz ) -> void
{
}
#endif

size_t const root_sz_pow = 31;
size_t const commit_threshold_pow = 26;
size_t const min_block_sz_pow = 16;
size_t const num_headers = 1<<(root_sz_pow-commit_threshold_pow);

struct block_header
{
    block_header* next;
};
struct buddy
{
    void* root;
    array<block_header*,root_sz_pow-min_block_sz_pow+1> free_list;
    array<block_header,num_headers> headers;
    bitset<num_headers> committed;

    buddy();
};

buddy g_bd;

auto ptoh_idx( block_header const* p ) -> size_t
{
    return (((char*)p) - ((char*)g_bd.root))>>commit_threshold_pow;
}

auto ptoh( block_header const* p ) -> block_header*
{
    return &g_bd.headers[ptoh_idx(p)];
}

buddy::buddy()
{
    g_bd.root = reserve( 1<<root_sz_pow );
    g_bd.free_list.assign( nullptr );
    g_bd.headers.assign( block_header{ nullptr } );
    g_bd.free_list[0] = (block_header*)g_bd.root;
    ptoh(g_bd.free_list[0])->next = nullptr;
}

auto bd_allocate( size_t const sz_pow ) -> void*
{
    if( sz_pow < min_block_sz_pow ) {
        return nullptr;
    }
    size_t i;
    for( i = sz_pow; ; ++i ) {
        if( i > root_sz_pow ) {
            return nullptr;
        }
        auto const idx = root_sz_pow-i;
        if( g_bd.free_list[idx] ) {
            break;
        }
    }

    for( ; i >= commit_threshold_pow; --i ) {
        auto const idx = root_sz_pow-i;
        auto const blk = g_bd.free_list[idx];
        g_bd.free_list[idx] = ptoh(g_bd.free_list[idx])->next;
        g_bd.free_list[idx+1] = blk;
        auto* neighbor = (block_header*)(((char*)blk) + (1<<(i-1)));
        ptoh(g_bd.free_list[idx+1])->next = neighbor;
        ptoh(neighbor)->next = nullptr;
    }

    if( i == commit_threshold_pow-1 ){
        auto const idx = root_sz_pow-i;
        auto const h_idx = ptoh_idx( g_bd.free_list[idx] );
        if( !g_bd.committed.test(h_idx) ) {
            commit( g_bd.free_list[idx], 1<<commit_threshold_pow );
            g_bd.committed.set(h_idx);
        }
    }

    for( ; i > sz_pow; --i ) {
        auto const idx = root_sz_pow-i;
        auto const blk = g_bd.free_list[idx];
        g_bd.free_list[idx] = g_bd.free_list[idx]->next;
        g_bd.free_list[idx+1] = blk;
        auto* neighbor = (block_header*)(((char*)blk) + (1<<(i-1)));
        g_bd.free_list[idx+1]->next = neighbor;
        neighbor->next = nullptr;
    }

    auto const idx = root_sz_pow-i;
    auto const ret = g_bd.free_list[idx];
    g_bd.free_list[idx] = g_bd.free_list[idx]->next;
    return ret;
}

auto bd_free( void* const p, size_t const sz_pow ) -> void
{
    if( !p ) return;
    auto i = sz_pow;
    auto* blk = (block_header*)p;

    auto const idx = root_sz_pow-i;
    auto const next = g_bd.free_list[idx];
    blk->next = next;
    g_bd.free_list[idx] = blk;
}
