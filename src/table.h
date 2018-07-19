#pragma once
#include "stdafx.h"
#include "utility.h"
#include "dleinst.h"

#define VERBOSE_PRINT( i ) if( g_opt.verbose >= i )

template< typename T, typename K >
using bt_unordered_map = ::std::unordered_map< T, K, bt::hash<T> >;
template< typename T >
using bt_unordered_set = ::std::unordered_set< T, bt::hash<T> >;

auto init_types() -> void;
auto compute_dep_graph() -> void;
auto process_indices() -> void;
auto print_indices(
	rref<index_structure_tc> const x,
	unsigned const indent = 0u)
	-> void;
auto root_structure() -> rref<index_structure_tc>;

auto process_rule(
    rref<location_tc> const loc,
    rref_list<literal_tc> const heads,
    rref_list<literal_tc> const body,
    bool const inline_rule = false )
    -> void;

auto process_fact( 
    rref<location_tc> const loc,
    rref<literal_tc> const lit )
    -> void;

auto process_predicate_decl(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<type_tc> const types,
    rref_list<string_tc> const specifiers,
    bool const variadic = false,
    bool const builtin = false )
    -> void;

auto process_data_decl(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<string_tc> const tyvars,
    rref_list<dconstr_tc> const dconstrs )
    -> void;

auto process_index_decl(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<binding_tc> const bp,
    rref<term_tc> const descr,
    rref_list<string_tc> const specifiers )
    -> void;

auto inline construct_bp(
    rref_str const str,
    rref<location_tc> const loc = rref<location_tc>() )
    -> rref_list<binding_tc>
{
    return transform_to_list( 
        str.str(),
        [&]( char const x ) 
    { 
        if( x == 'b' ) {
            return construct<binding_tc::bound>();
        } else if( x == 'f' ) {
            return construct<binding_tc::free>();
        }
        else {
            if( loc ) {
                // TODO
            }
            else {
            }
            die();
        }
    } );
}

auto b_bound() -> rref<binding_tc>;
auto b_free() -> rref<binding_tc>;

auto process_function_declaration(
    rref<location_tc> const loc,
    rref_str const name,
    rref<type_tc> const result_type,
    rref_list<type_tc> const arg_types,
    rref_str const overrides = rref_str{} )
    -> rref<function_decl_tc>;

auto process_function_definition(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<term_tc> const args,
    rref<term_tc> const body )
    -> void;

auto expand_inlines() -> void;

auto inline dummy_loc()
{
    return construct<location_tc>( "internal", 0u, 0u, 0u, 0u );
}

//*****************************************************************************
using bound_t = binding_tc::bound;
using free_t = binding_tc::free;
auto inline b_bound() -> rref<binding_tc>
{
    return construct<bound_t>();
}

auto inline b_free() -> rref<binding_tc>
{
    return construct<free_t>();
}

auto inline all_free( rref_list<binding_tc> const x )
{
    return ag::all_of_equal( x, b_free() );
}

struct index_category
{
    enum : uint32_t {
        INDEX,
        PRIMARY,
        WORKLIST,
        DIRECT,
        BUILTIN
    };
};

struct wl_category
{
    enum : int32_t { 
        NORMAL,
        DIRECT,
        NON_RECURS
    };
};

//*****************************************************************************
template< 
    typename T, 
    typename Value >
struct array_index : deque< Value >
{
    using deque<Value>::deque;
    Value const default_value{};
    auto at( rref<T> const x ) -> Value&
    {
        static_assert( T::is_single, "Non unary type." );
        if( x.id >= this->size() ) {
            this->resize( x.id + 1 );
        }
        return this->operator[]( x.id );
    }

    auto const_at( rref<T> const x ) const -> Value const&
    {
        static_assert(T::is_single, "Non unary type." );
        if( x.id < this->size() ) {
            return this->operator[]( x.id );
        }
        else {
            return default_value;
        }
    }
};

template< typename T >
auto inline at( vector<T>& c, size_t const i ) -> decltype(auto)
{
    if( i + 1 > c.size() ) {
        c.resize( i + 1 );
    }
    return c[i];
}

template< typename T >
auto inline to_vector( rref<list_tc<T>> const list )
{
    vector<rref<T>> ret;
    for( auto const x : list ) {
        ret.push_back( x );
    }
    return move( ret );
}

template< typename T >
auto inline at( rref<list_tc<T>> const list, size_t idx )
{
    for( auto const x : list ) {
        if( !idx ) {
            return x;
        }
        --idx;
    }
    die();
}

//*****************************************************************************
// print
//*****************************************************************************
template< typename T >
auto operator<<( ostream& o, rref<T> const x ) -> ostream&;

struct print_data
{
    ostream& o;

    template< typename T, typename... Members, size_t... Is >
    auto print_members(
        T const& x,
        members_pack<Members...> const&,
        index_sequence<Is...> const& ) const
        -> void
    {
        int dummy[] = { 0,
            ( o << ( Is == 0 ? "" : "," ) 
                << x.*(Members::pointer()), 0 )... };
    }

    template< typename T, typename... Members >
    auto print_members(
        T const& x,
        members_pack<Members...> const& members ) const
        -> void
    {
        print_members( x, members, index_sequence_for<Members...>() );
    }

    template< typename T >
    auto operator()(
        T const& x ) const
        -> void
    {
        o << T::d_name() << '(';
        print_members( 
            x, 
            typename T::members_type() );
        o << ')';
    }
};

template< typename T >
auto inline operator<<( ostream& o, rref<T> const x ) -> ostream&
{
    type_switch( print_data{ o }, x );
    return o;
}

template< typename T >
auto inline operator<<( ostream& o, rref<list_tc<T>> const x ) -> ostream&
{
    o << '[';
    bool first = true;
    for( auto const y : x ) {
        if( !first ) o << ',';
        o << y;
        first = false;
    }
    return o << ']';
}

auto inline operator<<( ostream& o, rref< proper_type_tc > const x ) 
    -> ostream&
{
    o << x->name;
    for( auto const y : x->args ) {
        o << ' ' << y;
    }
    return o;
}

auto inline operator<<( ostream& o, rref< proper_dconstr_tc > const x ) 
    -> ostream&
{
    o << x->name << "::";
    for( auto const y : x->args ) {
        o << y << "->";
    }
    return o << x->type;
}

auto inline operator<<( ostream& o, rref< typed_predicate_tc > const x ) 
    -> ostream&
{
    return o << x->decl->name << '/' << x->decl->types.size();
}

auto inline operator<<( ostream& o, rref_list<binding_tc> const x ) 
    -> ostream&
{
    o << '#';
    for( auto const y : x ) {
        if( y.is<bound_t>() ) {
            o << 'b';
        }
        else if( y.is<free_t>() ) {
            o << 'f';
        }
        else {
            die();
        }
    }
    return o;
}

auto inline operator<<( ostream& o, rref< adorned_typed_predicate_tc > const x ) 
    -> ostream&
{
    o << x->pred->decl->name;
    if( !all_free( x->bp ) ) {
        o << x->bp;
    }
    return o;
}

auto inline operator<<( ostream& o, rref<term_tc> const t ) -> ostream&
{
    BEGIN_TYPE_SWITCH_( term_tc, t )
    TYPE_CASE_X( i32 )
        o << x.value.get();
    TYPE_CASE_X( string )
        o << '"' << x.value.get() << '"';
    TYPE_CASE_X( variable )
        o << x.name;
        if( x.explicit_type ) {
            o << "::" << x.explicit_type;
        }
    TYPE_CASE_X( anonymous )
        o << '_';
    TYPE_CASE_X( compound )
        o << x.name;
        if( x.explicit_type ) {
            o << "::" << x.explicit_type;
        }
        o << '(' << delimited( x.inner ) << ')';
    TYPE_CASE_X( conditional )
        o << "if " << x.cond << " then " << x.true_block;
        if( x.false_block ) {
            o << " else " << x.false_block;
        }
    TYPE_CASE_X( aggregate )
        o << "$" << delimited( x.input )
            << "{" << delimited( x.lits ) << "}->" << x.output;
    END_TYPE_SWITCH_
    return o;
}

auto inline operator<<( ostream& o, rref< adorned_predicate_tc > const x ) 
    -> ostream&
{
    o << x->name;
    if( !all_free( x->bp ) ) {
        o << x->bp;
    }
    return o;
}

auto inline operator<<( ostream& o, rref< typed_term_tc > const x ) 
    -> ostream&
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, x )
    TYPE_CASE_X( i32 )
        o << x.value;
    TYPE_CASE_X( string )
        o << '"' << escaped_string{ x.value.str() } << '"';
    TYPE_CASE_X( variable )
        o << x.name; // << ':' << x.type;
    TYPE_CASE_X( anonymous )
        o << '_';
    TYPE_CASE_X( compound )
        o << x.dconstr->name << '(' << delimited( x.inner ) << ')';
    TYPE_CASE_X( function )
        o << x.func->decl->name << '(' << delimited( x.inner ) << ')';
    TYPE_CASE_X( conditional )
        o << "if " << x.cond << " then " << x.true_block;
        if( x.false_block ) {
            o << " else " << x.false_block;
        }
    TYPE_CASE_X( aggregate )
        o << "$" << delimited( x.input )
            << "{" << delimited( x.lits ) << "}->" << x.output;
    END_TYPE_SWITCH_
    return o;
}

auto inline operator<<( ostream& o, rref< typed_literal_tc > const x ) 
    -> ostream&
{
    if( x->negated.get() == 1 ) {
        o << '!';
    }
    else if( x->negated.get() == 2 ) {
        o << "!?";
    }
    if( all_free( x->apred->bp ) ) {
        o << x->apred->pred->decl->name;
    }
    else {
        o << x->apred->pred->decl->name << x->apred->bp;
    }
    return o << '(' << delimited( x->args ) << ')';
}

auto inline operator<<( ostream& o, rref<interned_string_tc> const x ) 
    -> ostream&
{
    return o << escaped_string{ x->value.str() };
}


auto inline operator<<( ostream& o, rref<index_info_tc> const idx_info ) 
    -> ostream& 
{
    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        o << x.d_name() << '@' << x.offset << "[" << x.arg << "]." << x.descr;
    TYPE_CASE_X( hash_map )
        o << x.d_name() << '@' << x.offset << x.args << "." << x.descr;
    TYPE_CASE_X( boolean )
        o << x.d_name() << '@' << x.offset;
    TYPE_CASE_X( value )
        o << x.d_name() << '@' << x.offset << x.args;
    TYPE_CASE_X( value_list )
        o << x.d_name() << '@' << x.offset << x.args;
    TYPE_CASE_X( hash_set )
        o << x.d_name() << '@' << x.offset << x.args;
    TYPE_CASE_X( bitmap )
        o << x.d_name() << '@' << x.offset << '[' << x.arg << ']';
    TYPE_CASE_X( replace )
        o << x.d_name() << '@' << x.offset << x.args;
    TYPE_CASE_X( join )
        o << x.d_name() << '@' << x.offset;
    TYPE_CASE_X( worklist )
        o << x.d_name() << '@' << x.offset;
    TYPE_CASE_X( direct )
        o << x.d_name();
    TYPE_CASE_X( source )
        o << x.d_name() << "[" << x.file_name << "]";
    TYPE_CASE_X( sink )
        o << x.d_name() << "@" << x.file_descriptor;
    TYPE_CASE_X( aggregate )
        o << x.d_name();
    TYPE_CASE_X( builtin )
        o << x.d_name();
    END_TYPE_SWITCH_
    return o;
}

auto inline operator<<( ostream& o, rref< predicate_index_tc > const x ) 
    -> ostream&
{
    return o << x->apred << ':' << x->idx_info;
}

auto inline operator<<( ostream& o, rref<location_tc> const x ) -> ostream&
{
    o << x->file_name << '(';
    if( x->first_line == x->last_line ) {
        o << x->first_line << ':' 
            << x->first_column << '-' << x->last_column;
    }
    else {
        o << x->first_line << ':' << x->first_column
            << '-'
            << x->last_line << ':' << x->last_column;
    }
    return o << ')';
}

auto inline operator<<( 
    ostream& o, 
    rref<function_decl_tc> const x )
    -> ostream&
{
    return o << x->name;
}

auto inline operator<( 
    rref< predicate_index_tc > const l, rref< predicate_index_tc > const r )
    -> bool
{
    return l->apred->pred->decl->name.str()
        < r->apred->pred->decl->name.str();
}

// Misc
struct indexed_literal
{
    rref<typed_literal_tc> lit;
    rref<predicate_index_tc> pidx;
    bool downstream_of_delta;
};

auto inline operator<<( ostream& o, indexed_literal const& x ) -> ostream&
{
    return o << x.lit << ':' << x.pidx->idx_info;
}

auto inline operator==( indexed_literal const& l, indexed_literal const& r )
{
    return l.lit == r.lit 
        && l.pidx == r.pidx 
        && l.downstream_of_delta == r.downstream_of_delta;
}

auto inline hash_value( indexed_literal const& x )
{
    size_t seed = 0u;
    bt::hash_combine( seed, x.lit );
    bt::hash_combine( seed, x.pidx );
    bt::hash_combine( seed, x.downstream_of_delta );
    return seed;
}

auto inline get_location(
    rref<term_tc> const x )
{
    return type_switch( []( auto const y ) { return y.loc; }, x );
}

auto inline get_location(
    rref<typed_term_tc> const x )
{
    return type_switch( []( auto const y ) { return y.loc; }, x );
}

//*****************************************************************************
struct typed_rule
{
    rref<location_tc> loc;
    rref<typed_literal_tc> head;
    vector<rref<typed_literal_tc>> body;
    ~typed_rule() {}
};

auto inline operator<<( ostream& o, typed_rule const& x ) -> ostream&
{
    return o << x.loc << ": " << x.head 
        << " :- "  << delimited( x.body );
}

//*****************************************************************************
// Equality and hashing without location
//*****************************************************************************
template< typename T >
auto inline unlocated_hash_members( 
    T const& x, 
    members_pack<> const&,
    size_t& seed )
    -> void
{}

template< typename T, typename Member, typename... Rest >
auto inline unlocated_hash_members( 
    T const& x, 
    members_pack<Member,Rest...> const&,
    size_t& seed )
    -> void;

template< typename T >
auto inline unlocated_equals_members(
    T const& l, T const& r, members_pack<> const& ) -> bool
{
    return true;
}

template< typename T, typename Member, typename... Rest >
auto inline unlocated_equals_members( 
    T const& l,
    T const& r,
    members_pack<Member,Rest...> const& )
    -> bool;

struct unlocated_hash
{
    template< typename T >
    auto operator()( T const& x ) const -> size_t
    {
        size_t seed = 0;
        unlocated_hash_members( x, typename T::members_type(), seed );
        return seed;
    }

    template< typename T >
    auto operator()( rref<T> const x ) const -> size_t
    {
        return type_switch( unlocated_hash{}, x );
    }

    auto operator()( rref<location_tc> const x ) const -> size_t
    {
        return 0;
    }

    auto operator()( rref<string_tc> const x ) const -> size_t
    {
        return hash_value( x );
    }

    auto operator()( rref<i32_tc> const x ) const -> size_t
    {
        return hash_value( x );
    }
};


template< typename T, typename Member, typename... Rest >
auto inline unlocated_hash_members( 
    T const& x, 
    members_pack<Member,Rest...> const&,
    size_t& seed )
    -> void
{
    seed ^= unlocated_hash()( x.*(Member::pointer()) ) 
        + 0x9e3779b9 + (seed<<6) + (seed>>2);
    unlocated_hash_members( x, members_pack<Rest...>(), seed );
}

struct unlocated_equality
{
    template< typename T >
    struct unlocated_compare
    {
        rref<T> r;
        template< typename DT >
        auto operator()( DT const& ld ) const -> bool
        {
            return r.template is<DT>()
                && unlocated_equality()( 
                    ld,
                    r.template get<DT>() );
        }
    };

    template< typename T >
    auto operator()( 
        T const& l, T const& r ) const -> bool
    {
        return unlocated_equals_members( l, r, typename T::members_type() );
    }

    template< typename T >
    auto operator()( 
        rref<T> const l, rref<T> const r,
        enable_if_t<!T::is_single>* = nullptr ) const 
        -> bool
    {
        return type_switch(
            unlocated_compare<T>{ r }, 
            l );
    }

    template< typename T >
    auto operator()( 
        rref<T> const l, rref<T> const r,
        enable_if_t<T::is_single>* = nullptr ) const
        -> bool
    {
        return unlocated_equals_members( 
            l.get(), 
            r.get(), 
            typename T::members_type() );
    }

    auto operator()(
        rref<location_tc> const l, rref<location_tc> const r ) const -> bool
    {
        return true;
    }

    auto operator()(
        rref<string_tc> const l, rref<string_tc> const r ) const -> bool
    {
        return l == r;
    }

    auto operator()( 
        rref<i32_tc> const l, rref<i32_tc> const r ) const -> bool
    {
        return l == r;
    }
};

template< typename T, typename Member, typename... Rest >
auto inline unlocated_equals_members( 
    T const& l, T const& r, members_pack<Member,Rest...> const& ) -> bool
{
    return unlocated_equality()( 
        l.*(Member::pointer()), r.*(Member::pointer()) )
            && unlocated_equals_members( l, r, members_pack<Rest...>() );
}

//*****************************************************************************
using pred_scc_set = rref_set<adorned_typed_predicate_tc>;

struct join_edge;
struct join_edge_set : list<join_edge>
{
    join_edge_set() = default;
    join_edge_set( join_edge_set const& ) = delete;
    join_edge_set( join_edge_set&& ) = default;
};

struct join_edge
{
    enum join_type { JOIN, INSERT, UINSERT, FIX } const type;
    unsigned const join_no;
    indexed_literal index;     
    pred_scc_set const* const scc{ nullptr };

    join_edge_set next;

    // TODO: also record the literal position with the rule
    struct info
    {
        typed_rule const rule;
    };
    vector<info> infos;

    join_edge( unsigned const join_no0, pred_scc_set const& scc0 )
        : type( FIX ),
        join_no( join_no0 ),
        scc( &scc0 )
    {}

    join_edge(
        join_type const type0,
        unsigned const join_no0,
        indexed_literal const& index0 )
        : type( type0 ), 
        join_no( join_no0 ),
        index( index0 )
    {}

    join_edge( join_edge const& ) = delete;
    join_edge( join_edge&& ) = default;

    auto front_info() const -> info const&
    { 
        my_assert( !infos.empty() );
        return infos.front();
    }
};

auto inline operator==( join_edge const& l, join_edge const& r ) -> bool
{
    my_assert( l.join_no == r.join_no );
    return l.type == r.type
        && ( l.type != join_edge::FIX 
            ? l.index == r.index 
            : l.scc == r.scc );
}

auto operator<<( ostream& o, join_edge const& x ) -> ostream&;

using var_set = bt_unordered_set<rref_str>;
struct join_root
{
    pred_scc_set const& scc;
    join_edge_set before_edges, after_edges, edb_edges, edges;
    rref_vector<adorned_typed_predicate_tc> worklist_apreds;
    rref_map<adorned_typed_predicate_tc,join_edge_set> direct_apreds;

    join_root( pred_scc_set const& scc0 ) : scc( scc0 ) {};
    join_root( join_root const& ) = delete;
    join_root( join_root&& ) = default;
};

//*****************************************************************************
using variable_type_map = bt_unordered_map< rref_str, rref<proper_type_tc> >;

struct block_context;
enum class predicate_type
{
    NORMAL, SINK, BUILTIN
};

struct data_decl
{
    rref_str name;
    vector<rref_str> tyvars;
    vector<rref<dconstr_tc>> dconstrs;
    bool declared{false};
};

struct environment
{
    // Decls
    bt_unordered_map<rref_str,data_decl> data_decls;
    bt_unordered_map<
        rref_str,
        vector<pair<rref<type_tc>,rref<dconstr_tc>>>
    > dconstrs;

    bt_unordered_map<rref_str,rref<predicate_decl_tc>> predicate_decls;
    array_index<adorned_predicate_tc,vector<rref<index_decl_tc>>> 
        index_decls;
    array_index<adorned_predicate_tc,vector<rref<index_decl_tc>>> 
        demand_index_decls;

    rref_set<predicate_decl_tc> inline_predicates;

    // Proper predicates and types
    rref_set<proper_type_tc> all_proper_atypes;
    rref_set<adorned_typed_predicate_tc> apreds; // excludes demand
    rref_set<adorned_typed_predicate_tc> all_apreds; // includes demand
    rref_set<adorned_typed_predicate_tc> defined;
    rref_set<adorned_typed_predicate_tc> underivable;

    rref_set<typed_term_tc> all_aggregates;
    array_index<
        proper_type_tc,
        vector<rref<proper_dconstr_tc>>> proper_dconstrs;
    rref_set<typed_function_tc> all_typed_functions;

    // Structures
    rref_map<
        index_node_tc,
        vector<rref<index_node_tc>>
    > index_children;

    rref_map<
        index_node_tc,
        unsigned 
	> index_offsets;

    rref_map<
        index_node_tc,
        rref<index_structure_tc>
    > index_decl_locations;

    rref_map<
        index_node_tc,
        rref<index_structure_tc>
    > index_structures;

    bt_unordered_map<
        pair<rref<index_node_tc>,rref<proper_dconstr_tc>>,
        rref<index_structure_tc>
    > subindex_structures;

    rref_map<
        index_node_tc,
        rref<index_info_tc>
    > index_infos;

    rref_map<
        index_node_tc,
        vector<rref_list<index_node_tc>>
    > subidx_chains;

    // Compilation
    array_index<adorned_typed_predicate_tc,vector<typed_rule>> rules;
    rref_map<adorned_typed_predicate_tc,vector<typed_rule>> inline_rules;
    rref_set<literal_tc> preferred_literals;
	rref_set<typed_literal_tc> preferred_typed_literals;

    using pred_dep_lists = array_index<
        adorned_typed_predicate_tc,
        bt_unordered_set<rref<adorned_typed_predicate_tc>>>;
    pred_dep_lists pred_succs;
    using aggr_dep_lists = rref_map<
        typed_term_tc,
        bt_unordered_set<rref<adorned_typed_predicate_tc>>>;
    aggr_dep_lists aggr_succs;


    vector<pred_scc_set> pred_sccs;
    array_index<
        adorned_typed_predicate_tc,
        pred_scc_set const*
    > pred_to_scc;

    // indices
    array_index<adorned_typed_predicate_tc,rref<predicate_index_tc>> 
        primary_index;
    array_index<adorned_typed_predicate_tc,rref<predicate_index_tc>> 
        worklist_index;
    array_index<adorned_typed_predicate_tc,vector<rref<predicate_index_tc>>>
        secondary_indices;
    array_index<typed_dconstr_tc,dconstr_tag_t> dconstr_tag;
    array_index<proper_dconstr_tc,size_t> dconstr_offset;
    
    array_index<adorned_typed_predicate_tc,rref<adorned_typed_predicate_tc>>
        demand_predicates, parent_predicates;

    // Joins
    vector<join_root> join_roots;
    rref_map<typed_term_tc,join_edge> aggregate_join_edges;

    // Types
    rref<proper_type_tc> i32_type;
    rref<proper_type_tc> string_type;
    rref<proper_type_tc> void_type;

    // Functions
    bt_unordered_map<
        pair<rref_str,unsigned>,
        rref<function_decl_tc>> function_decls;
    rref_map<
        function_decl_tc,
        rref_vector<function_decl_tc>
    > function_overrides;
    bt_unordered_map<
        rref_str,
        rref_vector<function_defn_tc>
    > function_defns;

    // IO
    vector<rref_str> file_names;

    // Commands
    rref_set<predicate_decl_tc> 
        traced_preds, 
        print_scc_preds;

    rref_map<adorned_typed_predicate_tc,vector<rref_vector<typed_literal_tc>>>
        eval_before, eval_after;

    // TODO: builtin predicates

    environment()
    {}
    environment( environment const& ) = delete;
};

extern environment env;

//*****************************************************************************
auto construct_demand_predicate(
    rref<adorned_typed_predicate_tc> const apred )
    -> rref<adorned_typed_predicate_tc>;

auto construct_demand_literal(
    rref<typed_literal_tc> const head )
    -> rref<typed_literal_tc>;

auto get_index_decls(
	rref<adorned_typed_predicate_tc> atpred)
	->vector<rref<index_decl_tc>> const&;

auto get_parent(
    rref<adorned_typed_predicate_tc> const apred )
    -> rref<adorned_typed_predicate_tc>;

auto get_predicate_decl(
    rref_str const name,
    rref<location_tc> const loc )
    -> rref<predicate_decl_tc>;

auto get_data_decl(
    rref<location_tc> const loc,
    rref_str const name )
    -> data_decl&;

auto get_function_declaration(
    rref<location_tc> const loc,
    rref_str const name,
    unsigned const arity )
    -> rref<function_decl_tc>;

auto get_proper_dconstr(
    rref_str const name,
    rref<proper_type_tc> const ptype,
    rref<location_tc> const loc = dummy_loc() )
    -> rref<proper_dconstr_tc>;

auto compile_rules() -> void;

auto is_proper_type(
    rref<type_tc> const atype,
    variable_type_map const& tyvar_types )
    -> bool;

auto to_proper_type(
    rref<type_tc> const atype,
    variable_type_map const& tyvar_types,
    rref_str const name = rref_str(),
    rref<location_tc> const loc = rref<location_tc>() )
    -> rref<proper_type_tc>; // may return null

auto to_proper_types(
    rref_list<type_tc> const x,
    variable_type_map const& tyvar_types,
    rref_str const name = rref_str(),
    rref<location_tc> const loc = rref<location_tc>() )
    -> rref_list<proper_type_tc>; // may contain null

auto to_atype(
    rref<proper_type_tc> const x )
    -> rref<type_tc>;

auto try_unify_type_variables(
    variable_type_map& tyvar_types,
    rref_list<proper_type_tc> const ptypes, // may contian nulls
    rref_list<type_tc> const types,
    rref_str const name = rref_str(),
    rref<location_tc> const loc = rref<location_tc>() )
    -> bool;

auto unify_term_var_types(
    rref<term_tc> const t,
    rref<proper_type_tc> const ptype,
    variable_type_map& var_types )
    -> void;

auto type_term(
    rref<term_tc> const term,
    variable_type_map const& var_types,
    rref<proper_type_tc> expected_ptype ) // may be null
    -> rref<typed_term_tc>;

auto term_to_proper_type(
    rref<typed_term_tc> const t )
    -> rref<proper_type_tc>;

auto parse_predicate(
    rref<location_tc> const loc,
    string_ref const str )
    -> rref<predicate_decl_tc>;

auto get_bp_from_arg_list(
    rref_list<i32_tc> const args,
    rref_list<binding_tc> const bp,
    unsigned const index = 0u )
    -> rref_list<binding_tc> const;

auto instantiate_function(
    rref<typed_function_tc> const tfunc,
    rref<function_defn_tc> const fdefn )
    -> pair<rref_list<typed_term_tc>,rref<typed_term_tc>>;

auto get_placeholder_list( unsigned i ) 
    -> rref_list<string_tc>;

namespace codegen2 {
auto codegen() -> void;
}

namespace type2 {
auto test_type() -> void;
}

//*****************************************************************************
// Tarjan's SCC
//*****************************************************************************
struct node_context
{
    unsigned index{(unsigned)-1};
    unsigned low_link{0};
    bool on_stack{false};
};

template< typename T >
struct scc_context
{
    unsigned index{0};
    vector<rref<T>> stack;
    array_index<T,node_context> nodes;
};

template< typename T >
auto inline strongly_connect(
    scc_context<T>& ctxt,
    array_index<T,bt_unordered_set<rref<T>>>& succ,
    vector<bt_unordered_set<rref<T>>>& sccs,
    rref<T> const v ) -> void
{
    ctxt.nodes.at( v ).index = ctxt.index;
    ctxt.nodes.at( v ).low_link = ctxt.index;
    ctxt.index += 1u;
    ctxt.stack.push_back( v );
    ctxt.nodes.at( v ).on_stack = true;

    for( auto const w : succ.at( v ) ) {
        if( ctxt.nodes.at( w ).index == (unsigned)-1 ) {
            strongly_connect( ctxt, succ, sccs, w );
            ctxt.nodes.at( v ).low_link = min(
                ctxt.nodes.at( v ).low_link,
                ctxt.nodes.at( w ).low_link );
        }
        else if( ctxt.nodes.at( w ).on_stack ) {
            ctxt.nodes.at( v ).low_link = min(
                ctxt.nodes.at( v ).low_link,
                ctxt.nodes.at( w ).index );
        }
    }

    if( ctxt.nodes.at( v ).low_link == ctxt.nodes.at( v ).index ) {
        rref<T> w;
        sccs.emplace_back();
        do {
            w = ctxt.stack.back();
            ctxt.stack.pop_back();

            ctxt.nodes.at( w ).on_stack = false;
            sccs.back().emplace( w );
        } while( w != v );
    }
}
