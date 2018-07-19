#pragma once
#include "utility.h"
#include "rilparser.h"

RREF_TYPE(store)
RREF_TYPE(global_initializer)
RREF_TYPE(edge)
RREF_TYPE(def_attributes)

char const* const universe_def = "@universe";

template< typename T, typename K >
using bt_unordered_map = ::std::unordered_map< T, K, bt::hash<T> >;
template< typename T >
using bt_unordered_set = ::std::unordered_set< T, bt::hash<T> >;

template< typename T >
auto inline member_wise_switch( rref<T> const x ) -> void;

struct member_wise
{
    template< typename T >
    auto operator()( 
        T const& x, 
        members_pack<> const& ) const
        -> void
    {}

    template< typename T, typename Member, typename... Rest >
    auto operator()( 
        T const& x, 
        members_pack<Member,Rest...> const& ) const
        -> void
    {
        member_wise_switch( x.*Member::pointer() );
        operator()( x, members_pack<Rest...>() );
    }

    template< typename T >
    auto operator()(
        T const& x ) const
        -> void
    {
        operator()( x, typename T::members_type() );
    }
};

auto inline member_wise_switch( rref<string_tc> const x ) -> void
{
}

auto inline member_wise_switch( rref<i32_tc> const x ) -> void
{
}

template< typename T >
auto inline member_wise_switch( rref<T> const x ) -> void
{
    type_switch( member_wise(), x );
}

auto inline instantiate()
{
    type_switch( member_wise(), rref<function_tc>() );
    type_switch( member_wise(), rref_list<expression_tc>() );
}

auto inline to_name( rref<def_tc> const x, bool const local = false ) 
    -> rref_str
{
    struct helper
    {
        bool const local;
        auto operator()( def_tc::alloc_a const& x ) -> rref_str
        {
            return ( !local ? x.func.to_string() + "/" : "" )
                + "&" + x.variable.to_string();
        }
        auto operator()( def_tc::alloc const& x ) -> rref_str
        {
            return ( !local ? x.func.to_string() + "/" : "" )
                + x.name.to_string();
        }
        auto operator()( def_tc::load const& x ) -> rref_str
        {
            if( x.pointer.is<def_tc::alloc_a>() ) {
                return ( !local ? x.func.to_string() + "/" : "" )
                    + x.pointer.get<def_tc::alloc_a>().variable.to_string()
                    + "@" + to_string( x.line_no )
                    + "." + to_string( x.block.get() );
            }
            else {
                return ( !local ? x.func.to_string() + "/" : "" )
                    + "*("
                    + to_name( x.pointer, true ).to_string()
                    + ")@" + to_string( x.line_no )
                    + "." + to_string( x.block.get() );
            }
        }
        auto operator()( def_tc::getelementptr const& x ) -> rref_str
        {
            return to_name( x.base, local ).to_string()
                + "[" + to_name( x.offset, true ).to_string()
                + "]";
        }
        auto operator()( def_tc::invoke const& x ) -> rref_str
        {
            return ( !local ? x.func.to_string() + "/" : "" )
                + to_name( x.target, true ).to_string()
                + "@" + to_string( x.line_no )
                + "." + to_string( x.block.get() );
        }
        auto operator()( def_tc::other const& x ) -> rref_str
        {
            return ( !local ? x.func.to_string() + "/" : "" )
                + x.name.to_string();
        }
        auto operator()( def_tc::global const& x ) -> rref_str
        {
            return x.name.str();
        }
        auto operator()( def_tc::argument const& x ) -> rref_str
        {
            return ( !local ? x.func.to_string() + "/" : "" )
                + x.name.to_string();
        }
        auto operator()( def_tc::integer const& x ) -> rref_str
        {
            return to_string( x.value.get() );
        }
        auto operator()( def_tc::non_pointer const& x ) -> rref_str
        {
            return x.name.str();
        }
    };

    return type_switch( helper{ local }, x );
}

auto inline operator<<( ostream& o, rref<function_tc> const f ) -> ostream&
{
    return o << f->name;
}

struct get_function_helper
{
    template< typename T >
    auto operator()( T const& x ) { return x.func; }
    auto operator()( def_tc::global const& x ) -> rref_str
    { return ""; }
    auto operator()( def_tc::getelementptr const& x ) -> rref_str
    { return ""; }
    auto operator()( def_tc::non_pointer const& x ) -> rref_str
    { return ""; }
};

auto inline operator<<( ostream& o, rref<def_tc> const x ) -> ostream&
{
    return o << to_name( x );
}

template< typename Arg >
auto inline write_fields( ostream& o, Arg&& arg ) -> void
{
    o << arg << '\n';
}

template< typename Arg, typename... Args >
auto inline write_fields( ostream& o, Arg&& arg, Args&&... args ) -> void
{
    o << arg << '\t';
    write_fields( o, forward<Args>( args )... );
}

struct tables
{
    ofstream functions;
    ofstream function_args;
    ofstream function_returns;
    ofstream cfg_edges;
    ofstream allocs;
    ofstream loads;
    ofstream getelementptr;
    ofstream invokes;
    ofstream invoke_args;
    ofstream stores;
    ofstream others;
    ofstream integers;
    ofstream globals;
    ofstream assigns;
    ofstream def_attributes;
    ofstream global_initializers;
};

extern tables g_output;

struct options
{
    fs::path input_file_path;
    fs::path output_directory;
    int verbose{0};
    bool yy_debug{false};
};

extern options g_opt;

struct block
{
    uint32_t num;
};

auto inline operator<<( ostream& o, block const& x ) -> ostream&
{
    return o << x.num;
}

struct graph_ordering
{
    uint32_t preorder{(uint32_t)-1}, rpostorder{(uint32_t)-1};
};

struct function_context;
struct global_context
{
    vector<function_context> fctxts;
    rref_set<def_tc> defs;
    bt_unordered_set<pair<rref_str,rref_str>> assigns;

    template< typename... Args >
    auto construct_global( Args&&... args )
    {
        auto const x = ::construct<def_tc::global>( 
            ::std::forward<Args>( args )... );
        defs.emplace( x );
        return x;
    }
    template< typename... Args >
    auto construct_gep( Args&&... args )
    {
        auto const x = ::construct<def_tc::getelementptr>( 
            ::std::forward<Args>( args )... );
        defs.emplace( x );
        return x;
    }
    template< typename... Args >
    auto construct_integer( Args&&... args )
    {
        auto const x = ::construct<def_tc::integer>( 
            ::std::forward<Args>( args )... );
        defs.emplace( x );
        return x;
    }
    template< typename... Args >
    auto construct_non_ptr( Args&&... args )
    {
        auto const x = ::construct<def_tc::non_pointer>( 
            ::std::forward<Args>( args )... );
        defs.emplace( x );
        return x;
    }
};

struct function_context
{
    rref_str func;
    uint32_t next_block{0};
    uint32_t next_alloc{0};
    bt_unordered_map<rref_str,rref<def_tc>> allocs;
    rref_set<edge_tc> edges;

    vector<graph_ordering> ordering;

    vector<pair<rref<def_tc>,uint32_t>> returns;

    rref_vector<def_tc> defs;
    rref_vector<store_tc> stores;

    function_context( rref_str const func0 )
        : func( func0 )
    {}

    auto new_block() 
    { 
        return block{ next_block++ };
    }

    auto order_blocks()
    {
        my_assert( ordering.empty() );
        auto const num_block = next_block;
        ordering.resize( num_block );
        vector<vector<uint32_t>> out( num_block );
        vector<bool> visited( num_block );
        for( auto const edge : edges ) {
            out[edge->from].push_back( edge->to );
        }

        vector<uint32_t> wl;
        wl.push_back( 0 );

        uint32_t preorder = 0, postorder = 0;
        while( !wl.empty() ) {
            auto const curr = wl.back();
            if( !visited[curr] ) {
                visited[curr] = true;
                ordering[curr].preorder = preorder++;
                for( auto const next : out[curr] ) {
                    if( !visited[next] ) {
                        wl.push_back( next );
                    }
                }
            }
            else {
                if( ordering[curr].rpostorder == uint32_t( -1 ) ) {
                    ordering[curr].rpostorder = num_block - 1 - postorder++;
                }
                wl.pop_back();
            }
        }
        my_assert( postorder == num_block && preorder == num_block );
        my_assert( ordering[0].preorder == 0 && ordering[0].rpostorder == 0 );
    }

    template< typename T, typename... Args >
    auto construct_def( Args&&... args )
    {
        auto const x = ::construct<T>( ::std::forward<Args>( args )... );
        my_assert( !x.template is<def_tc::global>() 
            && !x.template is<def_tc::non_pointer>() );
        defs.push_back( x );
        return x;
    }

    template< typename... Args >
    auto construct_store( Args&&... args )
    {
        auto const x = ::construct<store_tc>( 
            ::std::forward<Args>( args )... );
        stores.push_back( x );
        return x;
    }
};


extern global_context g_ctxt;

auto inline add_edge( 
    function_context& fctxt, 
    block const& from, 
    block const& to )
{
    fctxt.edges.insert( construct<edge_tc>( fctxt.func, from.num, to.num ) );
}

// Creates a new successor block and returns old block number
auto inline fresh_block( block& x, function_context& fctxt )
   
{
    auto const newx = fctxt.new_block();
    auto const old_num = x.num;
    add_edge( fctxt, x, newx );
    x = newx;
    return old_num;
}

auto write_tables() -> void;

auto inline set_pointer_typed( rref<def_tc> const def )
{
    construct<def_attributes_tc>( def, rref_str( "pointer_typed" ) );
    return def;
}

auto inline set_extern( rref<def_tc> const def )
{
    construct<def_attributes_tc>( def, rref_str( "extern" ) );
    return def;
}

auto inline set_constant( rref<def_tc> const def )
{
    construct<def_attributes_tc>( def, rref_str( "constant" ) );
    return def;
}

auto inline set_vararg( rref<def_tc> const def )
{
    construct<def_attributes_tc>( def, rref_str( "vararg" ) );
    return def;
}
