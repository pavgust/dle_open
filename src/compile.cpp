#include "stdafx.h"
#include "utility.h"

#include "main.h"
#include "table.h"

using namespace ::std::placeholders;

auto contains_function(
    rref<typed_term_tc> const t )
    -> bool;

auto contains_function(
    rref_list<typed_term_tc> const x )
    -> bool
{
    return ag::any_of( 
        x,
        [&]( auto const y )
        { return contains_function( y ); } );
}

auto contains_function(
    rref<typed_term_tc> const t )
    -> bool
{
    if( t.is<typed_term_tc::function>() ) {
        return true;
    }
    else if( t.is<typed_term_tc::compound>() ) {
        return contains_function( t.get<typed_term_tc::compound>().inner );
    }
    else {
        return false;
    }
}

auto get_var( rref<typed_term_tc> const x ) -> rref_str
{
    return x.get<typed_term_tc::variable>().name;
}

auto term_is_bound(
    rref<typed_term_tc> const t,
    bt_unordered_set< rref_str > const& bound_vars )
    -> bool
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        return true;
    TYPE_CASE_X( string )
        return true;
    TYPE_CASE_X( variable )
        return set_contains(
            bound_vars,
            get_var( t ) );
    TYPE_CASE_X( anonymous )
        return false;
    TYPE_CASE_X( compound )
        return ag::all_of(
            x.inner,
            [&]( auto const y ) { return term_is_bound( y, bound_vars ); } );
    TYPE_CASE_X( function )
        return ag::all_of(
            x.inner,
            [&]( auto const y ) { return term_is_bound( y, bound_vars ); } );
    TYPE_CASE_X( conditional )
        return term_is_bound( x.cond, bound_vars )
            && term_is_bound( x.true_block, bound_vars ) 
            && term_is_bound( x.false_block, bound_vars );
    TYPE_CASE_X( aggregate )
        return ag::all_of( 
            x.input,
            [&]( auto const y ) { return term_is_bound( y, bound_vars ); } );
    END_TYPE_SWITCH_
}

auto bind_vars(
    rref<typed_term_tc> const t,
    bt_unordered_set< rref_str >& bound_vars )
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        return;
    TYPE_CASE_X( string )
        return;
    TYPE_CASE_X( variable )
        bound_vars.insert( get_var( t ) );
    TYPE_CASE_X( anonymous )
        return;
    TYPE_CASE_X( compound )
        for( auto const y : x.inner ) {
            bind_vars( y, bound_vars );
        }
    TYPE_CASE_X( function )
        // all arguments should already be bound
        my_assert( term_is_bound( t, bound_vars ) );
    TYPE_CASE_X( conditional )
        // all arguments should already be bound
        my_assert( term_is_bound( t, bound_vars ) );
    TYPE_CASE_X( aggregate )
        // all arguments should already be bound
        my_assert( term_is_bound( t, bound_vars ) );
    END_TYPE_SWITCH_
}

auto get_binding_pattern_of_vars(
    rref<typed_literal_tc> const lit,
    bt_unordered_set< rref_str > const& bound_vars )
    -> rref_list<binding_tc>
{
    return transform_to_list(
        lit->args,
        [&]( rref<typed_term_tc> const t )
    {
        return term_is_bound( t, bound_vars )
            ? b_bound()
            : b_free();
    } );
}


// TODO: move this elsewhere
auto builtin_binding_satisfied(
    rref<typed_literal_tc> const lit,
    rref_list<binding_tc> const bp,
    bt_unordered_set< rref_str > const& bound_vars )
    -> bool
{
    auto const pred = lit->apred->pred;
    if( pred->decl->name == "equals" ) {
        auto func_bound = ag::all_of(
            lit->args,
            [&]( rref<typed_term_tc> const t )
            {
                return !t.is<typed_term_tc::function>() 
                    || term_is_bound( t, bound_vars );
            } );
        if( !func_bound ) {
            return false;
        }
        return rg::count( bp, b_bound() ) >= 1;
    }
    else if( pred->decl->name == "not_zero" ) {
        return rg::count( bp, b_bound() ) == 1;
    }
    else if( pred->decl->name == "print" ) {
        cerr << bt::format(
            "%s: Print predicate cannot appear in bodies of rules.\n" )
            % lit->loc;
        exit( 1 );
    }
    else {
        die();
    }
}

auto function_satisfied(
    rref<typed_term_tc> const t,
    bt_unordered_set< rref_str > const& bound_vars )
    -> bool;

auto function_satisfied(
    rref_list<typed_term_tc> const x,
    bt_unordered_set< rref_str > const& bound_vars )
    -> bool
{
    return ag::all_of( 
        x,
        [&]( auto const y )
        { return function_satisfied( y, bound_vars ); } );
}

auto function_satisfied(
    rref<typed_term_tc> const t,
    bt_unordered_set< rref_str > const& bound_vars )
    -> bool
{
    if( t.is<typed_term_tc::function>() ) {
        return term_is_bound( t, bound_vars );
    }
    else if( t.is<typed_term_tc::compound>() ) {
        auto const& x = t.get<typed_term_tc::compound>();
        return function_satisfied( x.inner, bound_vars );
    }
    else {
        return true;
    }
}

auto all_bound(
    rref_list<i32_tc> const args,
    rref_list<binding_tc> const bp )
    -> bool
{
    return ag::all_of(
        args, [&bp]( uint32_t const arg )
        { return at( bp, arg ).is<bound_t>(); } );
}

auto all_bound(
    rref_list<binding_tc> const bp )
    -> bool
{
    return ag::all_of(
        bp, []( rref<binding_tc> const arg )
        { return arg.is<bound_t>(); } );
}

auto get_cost(
    rref<adorned_typed_predicate_tc> const apred,
    rref<index_info_tc> const idx_info,
    rref_list<binding_tc> const bp )
    -> unsigned
{
    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        auto const newbp = get_bp_from_arg_list(
            rref_list<i32_tc>{ x.arg }, bp );
        return at( bp, x.arg ).is<bound_t>()
            ? get_cost( apred, x.descr, newbp )
            : x.cost.get() * get_cost( apred, x.descr, newbp );
    TYPE_CASE_X( hash_map )
        auto const newbp = get_bp_from_arg_list( x.args, bp );
        return all_bound( x.args, bp )
            ? get_cost( apred, x.descr, newbp )
            : x.cost.get() * get_cost( apred, x.descr, newbp );
    TYPE_CASE_X( boolean )
        return 1;
    TYPE_CASE_X( value )
        return 1;
    TYPE_CASE_X( value_list )
        return x.cost;
    TYPE_CASE_X( hash_set )
        return all_bound( bp ) ? 1 : x.cost.get();
    TYPE_CASE_X( bitmap )
        return all_bound( bp ) ? 1 : x.cost.get();
    TYPE_CASE_X( replace )
        return 1;
    TYPE_CASE_X( join )
        return 1;
    TYPE_CASE_DIE( worklist )
    TYPE_CASE_DIE( direct )
    TYPE_CASE_X( source )
        return x.cost;
    TYPE_CASE_X( sink )
        return (unsigned)-1;
    TYPE_CASE_X( aggregate )
        return (unsigned)-1;
    TYPE_CASE_X( builtin )
        return 0;
    END_TYPE_SWITCH_
}

auto get_cheapest_access_path(
    rref<adorned_typed_predicate_tc> const apred,
    rref_list<binding_tc> const bp )
    -> pair<unsigned,rref<predicate_index_tc>>
{
    vector< pair<unsigned,rref<predicate_index_tc> > > costs;
    auto const primary = env.primary_index.at( apred );
    auto const& secondaries = env.secondary_indices.at( apred );

    if( primary ) {
        costs.emplace_back( 
            get_cost( apred, primary->idx_info, bp ), primary );
    }
    for( auto const secondary : env.secondary_indices.at( apred ) ) {
        auto const cost = get_cost( apred, secondary->idx_info, bp );
        if( cost != (decltype(cost))-1 ) {
            costs.emplace_back(
                cost,
                secondary );
        }
    }
    if( costs.empty() ) {
        return {0,{}};
    }

    rg::sort(
        costs,
        []( auto const& l, auto const& r ) { return l.first < r.first; } );

    VERBOSE_PRINT( 2 )
        cout << bt::format(
            "            %s%s picks %s with cost %d.\n" )
            % apred
            % bp
            % costs.front().second->idx_info
            % costs.front().first;
    return costs.front();
}

char const* const edb_predicate_name = "@edb";
auto construct_edb_predicate()
{
    static rref<adorned_typed_predicate_tc> edb_apred;
    if( edb_apred ) {
        return edb_apred;
    }
    auto const edb_decl = construct<predicate_decl_tc>(
        dummy_loc(), rref_str( edb_predicate_name ), nil<type_tc>(),
        0, 0, 1 );
    return edb_apred = construct<adorned_typed_predicate_tc>(
        construct<typed_predicate_tc>( edb_decl, nil<proper_type_tc>() ),
        nil<binding_tc>() );
}

auto construct_edb_lit()
{
    static rref<typed_literal_tc> edb_literal;
    if( edb_literal ) {
        return edb_literal;
    }
    auto const apred = construct_edb_predicate();
    env.worklist_index.at( apred ) 
        = construct<predicate_index_tc>(
            apred,
            construct<index_info_tc::builtin>(
                construct<builtin_pred_tc::edb>() ),
            index_category::BUILTIN );
    return edb_literal = construct<typed_literal_tc>(
        dummy_loc(), apred, nil<typed_term_tc>(), 0 );
}

auto construct_dummy_predicate( string_ref const name )
{
    static bt_unordered_map<string_ref,rref<adorned_typed_predicate_tc>> 
        apreds;
    auto& apred = apreds[name];
    if( apred ) {
        return apred;
    }
    auto const decl = construct<predicate_decl_tc>(
        dummy_loc(), rref_str( name ), nil<type_tc>(),
        0, 0, 1 );
    return apred = construct<adorned_typed_predicate_tc>(
        construct<typed_predicate_tc>( decl, nil<proper_type_tc>() ),
        nil<binding_tc>() );
}

auto construct_dummy_lit( string_ref const name )
{
    static bt_unordered_map<string_ref,rref<typed_literal_tc>> lits;
    auto& lit = lits[name];
    if( lit ) {
        return lit;
    }
    auto const apred = construct_dummy_predicate( name );
    return lit = construct<typed_literal_tc>(
        dummy_loc(), apred, nil<typed_term_tc>(), 0 );
}

auto construct_aggregate_lit( rref<typed_term_tc> const t )
{
    auto const& x = t.get<typed_term_tc::aggregate>();
    auto const decl = construct<predicate_decl_tc>(
        dummy_loc(), rref_str( "@aggregate" ), 
        rref_list<type_tc>{ to_atype( x.type ) },
        0, 0, 1 );
    auto const apred = construct<adorned_typed_predicate_tc>(
        construct<typed_predicate_tc>( 
            decl, rref_list<proper_type_tc>{ x.type } ),
        rref_list<binding_tc>{ b_free() } );
    if( env.secondary_indices.at( apred ).empty() ) {
        env.secondary_indices.at( apred ).push_back( 
            construct<predicate_index_tc>( 
                apred, construct<index_info_tc::aggregate>(),
                index_category::INDEX ) );
    }
    return construct<typed_literal_tc>(
        dummy_loc(), apred, 
        rref_list<typed_term_tc>{ x.output }, 
        0 );
}

auto requires_delta( 
    bt_unordered_set< rref<adorned_typed_predicate_tc> > const& scc,
    typed_rule const& rule,
    rref<typed_literal_tc> const lit )
    -> bool
{
    auto const apred = lit->apred;
    if( construct_edb_predicate() == apred ) {
        return true;
    }
    if( apred->pred->decl->name.str().starts_with( "@aggregate" ) ) {
        return true;
    }
    if( lit->negated.get() == 2 ) {
        return false;
    }

    auto const parent = env.parent_predicates.at( apred );
        
    bool const in_scc =  set_contains( scc, apred )
        || ( parent && set_contains( scc, parent ) );
    // if predicate is non-recursive, only its magic predicate
    // requires a delta rule
    // TODO: change interpretation of non-recursive
    // to mean that every (recursive) demand is treated as as a demand
    // of a lower relation
    bool const recursive_idb 
        = apred->pred->decl->direct.get() != wl_category::NON_RECURS
        || ( parent && parent == rule.head->apred ); 

    return in_scc && recursive_idb;
}

auto requires_edb_rule(
    bt_unordered_set< rref<adorned_typed_predicate_tc> > const& scc,
    typed_rule const& rule ) -> bool
{
    if( !all_free( rule.head->apred->bp ) ) {
        return false;
    }
    for( auto const lit : rule.body ) {
        if( !all_free( lit->apred->bp ) ) {
            return true;
        }
        if( requires_delta( scc, rule, lit ) ) {
            return false;
        }
    }
    return true;
}

auto operator<<( ostream& o, join_edge const& x ) -> ostream&
{
    o << indenter{ x.join_no };
    if( x.type == join_edge::FIX ) {
        o << "FIX {" << delimited( *x.scc ) << '}' << '\n';
    }
    else {
        o << ( x.type == join_edge::JOIN ? "" 
                : x.type == join_edge::UINSERT ? "UINS "
                : "INS " ) 
            << x.index << '\n';
    }
    return o;
}

auto operator<<( ostream& o , join_edge_set const& x ) -> ostream&
{
    for( auto const& e : x ) {
        o << e;
        o << e.next;
    }
    return o;
}

struct unifier_map : bt_unordered_map<rref_str, rref_str>
{
    bt_unordered_set<rref_str> tgts; // target of remap
    bt_unordered_set<rref_str> unavailable; // names already used
    using parent_type = bt_unordered_map<rref_str, rref_str>;
    auto insert( rref_str const from, rref_str const to )
    {
        if( parent_type::insert( { from, to } ).second ) {
            VERBOSE_PRINT( 3 )
                cout << bt::format(
                    "\tRemapped %s to %s.\n" )
                    % from % to;
            tgts.insert( to );
        }
    }

    auto remap_name( rref_str const name )
    {
        string new_name = name.to_string();
        do {
            new_name.push_back( '\'' );
        } while( set_contains( tgts, new_name )
            || set_contains( unavailable, new_name ) );
        
        unavailable.insert( new_name );
        VERBOSE_PRINT( 3 )
            cout << bt::format(
                "\tRemapped %s to %s.\n" )
                % name % new_name;
        parent_type::insert( { name, new_name } );
        return rref_str( new_name );
    }
};
using bound_var_set = rref_set<string_tc>;

auto remap_term(
    rref<typed_term_tc> const t,
    unifier_map& umap )
    -> rref<typed_term_tc>
{
    if( umap.empty() && umap.tgts.empty() ) { 
        return t; 
    }

    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        return t;
    TYPE_CASE_X( string )
        return t;
    TYPE_CASE_X( variable )
        auto const tgt = find_default( umap, x.name );
        if( tgt ) {
            return construct<typed_term_tc::variable>( 
                x.loc, tgt, x.type );
        }
        else if( !set_contains( umap.tgts, x.name ) ) {
            return t;
        }
        else {
            // Rename to prevent capture
            return construct<typed_term_tc::variable>( 
                x.loc, umap.remap_name( x.name ), x.type );
        }
    TYPE_CASE_X( anonymous )
        return t;
    TYPE_CASE_X( compound )
        return construct<typed_term_tc::compound>(
            x.loc,
            x.dconstr,
            transform_to_list( x.inner, [&]( auto const t ) 
                { return remap_term( t, umap ); } ) );
    TYPE_CASE_X( function )
        return construct<typed_term_tc::function>(
            x.loc,
            x.func,
            transform_to_list( x.inner, [&]( auto const t ) 
                { return remap_term( t, umap ); } ) );
    TYPE_CASE_X( conditional )
        return construct<typed_term_tc::conditional>(
            x.loc, x.type, 
            remap_term( x.cond, umap ), 
            remap_term( x.true_block, umap ),
            remap_term( x.false_block, umap ) );
    TYPE_CASE_X( aggregate )
        return construct<typed_term_tc::aggregate>(
            x.loc,
            x.type,
            x.apred,
            transform_to_list( x.input, [&]( auto const t ) 
                { return remap_term( t, umap ); } ),
            x.output,
            x.lits );
    END_TYPE_SWITCH_
}

auto remap_literal(
    rref<typed_literal_tc> const lit,
    unifier_map& umap )
    -> rref<typed_literal_tc>
{
    if( umap.empty() && umap.tgts.empty() ) { return lit; }
    return construct<typed_literal_tc>(
        lit->loc,
        lit->apred,
        transform_to_list( lit->args, [&]( auto const t ) 
            { return remap_term( t, umap ); } ),
        lit->negated );
}

// TODO: this should return a unified term so that
// anonymous variables can be unified
auto unify_term(
    rref<typed_term_tc> const from,
    rref<typed_term_tc> const to,
    bound_var_set const& bound_vars,
    unifier_map& umap )
    -> bool
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, from )
    TYPE_CASE_X( i32 )
        return unlocated_equality{}( from, to );
    TYPE_CASE_X( string )
        return unlocated_equality{}( from, to );
    TYPE_CASE_X( variable )
        if( !to.is<typed_term_tc::variable>() ) {
            return false;
        }
        auto const& y = to.get<typed_term_tc::variable>();
        if( x.name != y.name ) {
            if( set_contains( bound_vars, x.name )
                || set_contains( bound_vars, y.name ) )
            {
                // Bound vars must already be renamed or can't be unified
                return false;
            }
            umap.insert( x.name, y.name );
        }
        return true;
    TYPE_CASE_X( anonymous )
        return unlocated_equality{}( from, to );
    TYPE_CASE_X( compound )
            if( !to.is<typed_term_tc::compound>() ) {
                return false;
            }
            auto const& y = to.get<typed_term_tc::compound>();
            if( x.dconstr != y.dconstr ) {
                return false;
            }
            for( auto const& pr : zip( x.inner, y.inner ) ) {
                if( !unify_term( get<0>( pr ), get<1>( pr ), 
                    bound_vars, umap ) ) 
                {
                    return false;
                }
            }
            return true;
    TYPE_CASE_X( function )
        if( !to.is<typed_term_tc::function>() ) {
            return false;
        }
        auto const& y = to.get<typed_term_tc::function>();
        if( x.func != y.func ) {
            return false;
        }
        for( auto const& pr : zip( x.inner, y.inner ) ) {
            if( !unify_term( get<0>( pr ), get<1>( pr ), 
                bound_vars, umap ) ) 
            {
                return false;
            }
        }
        return true;
    TYPE_CASE_X( conditional )
        return unlocated_equality{}( from, to );
    TYPE_CASE_X( aggregate )
        if( !to.is<typed_term_tc::aggregate>() ) {
            return false;
        }
        auto const& y = to.get<typed_term_tc::aggregate>();
        if( !unlocated_equality{}( x.lits, y.lits )
            || !unlocated_equality{}( x.output, y.output ) ) 
        {
            return false;
        }
        for( auto const& pr : zip( x.input, y.input ) ) {
            if( !unify_term( get<0>( pr ), get<1>( pr ), 
                bound_vars, umap ) ) 
            {
                return false;
            }
        }
        return true;
    END_TYPE_SWITCH_
}

auto free_variables( 
    rref<typed_term_tc> const t,
    rref_set<string_tc>& vars )
    -> void
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
    TYPE_CASE_X( string )
    TYPE_CASE_X( variable )
        vars.insert( x.name );
    TYPE_CASE_X( anonymous )
    TYPE_CASE_X( compound )
        for( auto const y : x.inner ) {
            free_variables( y, vars );
        }
    TYPE_CASE_X( function )
        for( auto const y : x.inner ) {
            free_variables( y, vars );
        }
    TYPE_CASE_X( conditional )
        free_variables( x.cond, vars );
        free_variables( x.true_block, vars );
        free_variables( x.false_block, vars );
    TYPE_CASE_X( aggregate )
        for( auto const y : x.input ) {
            free_variables( y, vars );
        }
    END_TYPE_SWITCH_
}

auto free_variables( 
    rref<typed_literal_tc> const lit,
    rref_set<string_tc>& vars )
    -> void
{
    for( auto const arg : lit->args ) {
        free_variables( arg, vars );
    }
}

auto unify_literal(
    rref<typed_literal_tc> const from,
    rref<typed_literal_tc> const to,
    bound_var_set const& bound_vars,
    vector<indexed_literal>& body_lits,
    rref<typed_literal_tc>& head_lit )
    -> bool
{
    if( from->apred != to->apred || from->negated != to->negated ) {
        return false;
    }
    unifier_map umap;
    for( auto& lit_idx : body_lits ) {
        free_variables( lit_idx.lit, umap.unavailable );
    }
    free_variables( head_lit, umap.unavailable );


    for( auto const& pr : zip( from->args, to->args ) ) {
        if( !unify_term( get<0>( pr ), get<1>( pr ), bound_vars, umap ) ) {
            return false;
        }
    }

    for( auto& lit_idx : body_lits ) {
        lit_idx.lit = remap_literal( lit_idx.lit, umap );
    }
    head_lit = remap_literal( head_lit, umap );
    return true;
}

auto get_unified_indexed_literal(
    join_edge_set& edges,
    bound_var_set const& bound_vars,
    indexed_literal const& idx_lit,
    vector<indexed_literal>& body_lits,
    rref<typed_literal_tc>& head_lit )
    -> indexed_literal
{
    if( g_opt.no_term_unification ) {
        return idx_lit;
    }
    for( auto& e : edges ) {
        if( e.type == join_edge::JOIN 
            && idx_lit.pidx == e.index.pidx
            && idx_lit.downstream_of_delta == e.index.downstream_of_delta
            && unify_literal( idx_lit.lit, e.index.lit, bound_vars,
                body_lits, head_lit ) )
        {
            return e.index;
        }
    }
    return idx_lit;
}

auto insert_join_edge(
    join_edge_set& edges,
    bound_var_set const& bound_vars,
    join_edge&& nedge )
    -> join_edge&
{
    for( auto& e : edges ) {
        if( e == nedge ) {
            return e;
        }
    }

    edges.push_back( move( nedge ) );
    return edges.back();
}

auto insert_into_head( 
    typed_rule const& rule,
    rref<typed_literal_tc> const head,
    bound_var_set const& bound_vars,
    unsigned& join_no, 
    join_edge_set* edges )
    -> join_edge_set&
{
    auto const primary = env.primary_index.at( head->apred );
    if( primary ) {
        auto& new_edge = insert_join_edge( 
            *edges, bound_vars,
            { join_edge::UINSERT, join_no, { head, primary, true } } );
        new_edge.infos.push_back( { rule } );
        edges = &new_edge.next;
        join_no += 1;
    }

    auto const& secondaries = env.secondary_indices.at( head->apred );
    for( auto const secondary : secondaries ) {
        auto& new_edge = insert_join_edge( 
            *edges, bound_vars,
            { join_edge::INSERT, join_no, { head, secondary, true } } );
        new_edge.infos.push_back( { rule } );
    }
    auto const worklist = env.worklist_index.at( head->apred );
    if( worklist ) {
        auto& new_edge = insert_join_edge( 
            *edges, bound_vars,
            { join_edge::INSERT, join_no, { head, worklist, true } } );
        new_edge.infos.push_back( { rule } );
    }
    return *edges;
}

auto pick_indices(
    pred_scc_set const& scc,
    join_root& root,
    typed_rule const& rule,
    size_t const delta_idx )
    -> vector<indexed_literal>
{
    auto head_lit = rule.head;
    auto body_lits = rule.body;
    bound_var_set bound_vars;

    bool downstream_of_delta = false;
    vector<indexed_literal> idx_lits;

    auto const delta_lit = rule.body[delta_idx];
    if( contains_function( delta_lit->args ) ) {
        cerr << bt::format( 
            "%s: IDB literals must not contain functions.\n" )
            % delta_lit->loc;
        exit( 1 );
    }

    // Pick delta
    auto const pidx = env.worklist_index.at( delta_lit->apred );

    VERBOSE_PRINT( 2 )
        cout << bt::format(
            "        initial relation %s. Index: %s.\n" )
            % delta_lit
            % pidx->idx_info;

    // add worklist to list of worklists that reqire fixpoint comp.
    if( pidx->category.get() == index_category::WORKLIST ) {
        if( !seq_contains( root.worklist_apreds, pidx->apred ) ) {
            root.worklist_apreds.push_back( pidx->apred );
        }
    }

    idx_lits.push_back( { delta_lit, pidx, downstream_of_delta } );

    // Bind vars
    for( auto const t : delta_lit->args ) {
        bind_vars( t, bound_vars );
    }

    vector<refw<rref<typed_literal_tc> const>> remaining;
    size_t last_umap_sz = 0;
    for( size_t curr_idx = 0; curr_idx < body_lits.size(); ) {
        for( ; curr_idx < body_lits.size(); ) {
            if( curr_idx == delta_idx ) {
                curr_idx += 1;
                // delta is already joined
            }
            else if( body_lits[curr_idx] == construct_edb_lit() ) {
                curr_idx += 1;
                // Ignore edb literals
            }
            else if( curr_idx < delta_idx ) {
                remaining.push_back( bt::cref( body_lits[curr_idx] ) );
                curr_idx += 1;
                // join literals before next demand literal
            }
            else if( !all_free( body_lits[curr_idx]->apred->bp ) ) {
                if( !remaining.empty() ) {
                    break; // join literals before demand literal
                }
                downstream_of_delta = true; 
                remaining.push_back( bt::cref( body_lits[curr_idx] ) );
                curr_idx += 1;
                break; // join only the demand literal
            }
            else {
                remaining.push_back( bt::cref( body_lits[curr_idx] ) );
                curr_idx += 1;
                // join literals after delta and before the next demand
            }
        }

        size_t last_umap_sz = 0;
        while( !remaining.empty() ) {
            struct join_cost
            {
                unsigned cost;
                decltype( remaining )::iterator iter;
                indexed_literal idx_lit;
            };
            vector< join_cost > costs;
            for( auto iter = remaining.begin();
                iter != remaining.end();
                ++iter )
            {
                auto const lit = iter->get();
                auto const pred = lit->apred->pred;
                rref_list<binding_tc> const bp
                    = get_binding_pattern_of_vars( lit, bound_vars );
                bool const demand_sat = !downstream_of_delta
                    || ag::all_of( zip( lit->apred->bp, bp ),
                    []( auto const& tpl )
                    {
                        return get<0>( tpl ) != b_bound()
                            || get<1>( tpl ) == b_bound();
                    } );
                bool const negation_sat
                    = lit->negated.get() == 0
                    || ag::all_of(
                        zip( lit->args, bp ),
                        []( auto const& x )
                        {
                            rref<typed_term_tc> const arg = get<0>( x );
                            rref<binding_tc> const bp = get<1>( x );
                            return bp == b_bound()
                                || arg.is<typed_term_tc::anonymous>();
                        } );

                bool const builtin_sat
                    = pred->decl->builtin.get() == 0
                    || builtin_binding_satisfied( lit, bp, bound_vars );

                bool const function_sat 
                    = function_satisfied( lit->args, bound_vars );

                if( demand_sat && negation_sat 
                    && builtin_sat && function_sat ) 
                {
                    auto const pr = get_cheapest_access_path(
                        lit->apred,
                        bp );
                    if( pr.second ) {
						auto cost = pr.first;
						if( set_contains( env.preferred_typed_literals, lit ) ) {
							cost = 0;
							VERBOSE_PRINT( 2 )
								cout << "Preferring " << lit << "\n";
						}
                        costs.push_back( {
                            cost,
                            iter,
                            { lit, 
                                pr.second,
                                downstream_of_delta } } );
                    }
                }
            }
            if( costs.empty() ) {
                cerr << bt::format(
                    "No viable join order exists for rule\n"
                    "%s :- %s\n"
                    "Remaining literals: %s\n"
                    "Bound variables: %s\n" )
                    % rule.head
                    % delimited( body_lits )
                    % delimited( remaining )
                    % delimited( bound_vars );
                exit( 1 );
            }
            rg::stable_sort(
                costs,
                []( auto const& l, auto const& r )
                { return l.cost < r.cost; } );
            remaining.erase( costs.front().iter );

            VERBOSE_PRINT( 2 )
                cout << bt::format(
                    "        picked %s. Cost: %d. Index: %s.\n" )
                    % costs.front().idx_lit.lit
                    % costs.front().cost
                    % costs.front().idx_lit.pidx->idx_info;


            idx_lits.push_back( costs.front().idx_lit );

            // bind variables
            for( auto const t : costs.front().idx_lit.lit->args ) {
                bind_vars( t, bound_vars );
            }
        }
    }

    // check that all head variables are bound
    for( auto const t : head_lit->args ) {
        if( !term_is_bound( t, bound_vars ) ) {
            cerr << bt::format(
                "%s: Term '%s' is not fully bound "
                "by the body.\n" )
                % rule.loc
                % t;
            exit( 1 );
        }
    }
    my_assert( bound_vars.find( "_" ) == bound_vars.end() );
    return idx_lits;
}

unsigned free_name_num = 0;
auto separate_unification(
    rref<typed_literal_tc> const lit,
    rref_list<i32_tc> const args,
    rref_list<binding_tc> const bp,
    vector<pair<rref<typed_term_tc>,rref<typed_term_tc>>>& unifiers )
    -> rref<typed_literal_tc>
{
    // don't separate if its a demand argument
    for( auto const arg : args ) {
        if( lit->apred->bp[arg].is<bound_t>() ) {
            return lit;
        }
    }

	// don't separate negated literals
	if( lit->negated.get() != 0 ) {
		return lit;
	}

    unsigned idx = 0;
    auto const new_args = transform_to_list(
        zip( lit->args, lit->apred->pred->types ),
        [&]( auto const& pr )
    {
        rref<typed_term_tc> const t = get<0>( pr );
        rref<proper_type_tc> const ptype = get<1>( pr );
        if( !seq_contains( args, idx++ ) 
            || t.is<typed_term_tc::variable>() ) 
        {
            return t;
        }

        rref_str new_var_name = string_ref( 
            "__" + to_string( free_name_num++ ) );
        auto const new_var = construct<typed_term_tc::variable>(
            get_location( t ),
            new_var_name,
            ptype );
        if( !t.is<typed_term_tc::anonymous>() ) {
            unifiers.push_back( { new_var, t } );
        }
        return new_var; 
    } );
    return construct<typed_literal_tc>(
        lit->loc, lit->apred, new_args, lit->negated );
}

auto make_index_list( unsigned i ) -> rref_list<i32_tc>
{
    auto x = nil<i32_tc>();
    while( i > 0 ) {
        x.push_front( --i );
    } 
    return x;
}

auto separate_unification(
    rref<typed_literal_tc> const lit,
    rref<index_info_tc> const idx_info,
    rref_list<binding_tc> const bp,
    vector<pair<rref<typed_term_tc>,rref<typed_term_tc>>>& unifiers )
    -> rref<typed_literal_tc>
{
    BEGIN_TYPE_SWITCH_( index_info_tc, idx_info )
    TYPE_CASE_X( array_map )
        return separate_unification( lit, x.descr, bp, unifiers );
    TYPE_CASE_X( hash_map )
        return separate_unification( lit, x.descr, bp, unifiers );
    TYPE_CASE_X( boolean )
        return lit;
    TYPE_CASE_X( value )
        return separate_unification( lit, x.args, bp, unifiers );
    TYPE_CASE_X( value_list )
        return separate_unification( lit, x.args, bp, unifiers );
    TYPE_CASE_X( hash_set )
        return all_bound( x.args, bp )
            ? lit 
            : separate_unification( lit, x.args, bp, unifiers );
    TYPE_CASE_X( bitmap )
        return at( bp, x.arg ).is<bound_t>()
            ? lit 
            : separate_unification( lit, { x.arg }, bp, unifiers );
    TYPE_CASE_X( replace )
        return separate_unification( lit, x.args, bp, unifiers );
    TYPE_CASE_X( join )
        return separate_unification( lit, { x.arg }, bp, unifiers );
    TYPE_CASE_X( worklist )
        return separate_unification( 
            lit, make_index_list( lit->args.size() ), bp, unifiers );
    TYPE_CASE_X( direct )
        return separate_unification( 
            lit, make_index_list( lit->args.size() ), bp, unifiers );
    TYPE_CASE_X( source )
        return separate_unification( 
            lit, make_index_list( lit->args.size() ), bp, unifiers );
    TYPE_CASE_DIE( sink )
    TYPE_CASE_DIE( aggregate )
    TYPE_CASE_X( builtin )
        return lit;
    END_TYPE_SWITCH_
}

auto separate_unification(
    vector<indexed_literal> const& body_lits )
{
    free_name_num = 1;
    bound_var_set bound_vars;
    vector<indexed_literal> new_body_lits;
    vector<pair<rref<typed_term_tc>,rref<typed_term_tc>>> unifiers;
    bool unified_one = false;
    for( auto& idx_lit : body_lits ) {
        rref_list<binding_tc> const bp
            = get_binding_pattern_of_vars( idx_lit.lit, bound_vars );
        auto const new_lit =  separate_unification( 
            idx_lit.lit, idx_lit.pidx->idx_info, bp, unifiers );
        new_body_lits.push_back( { 
            new_lit, idx_lit.pidx, idx_lit.downstream_of_delta } );
        for( auto const& pr : unifiers ) {
            auto const unifier_lit = construct<typed_literal_tc>(
                dummy_loc(), 
                construct<adorned_typed_predicate_tc>(
                    construct<typed_predicate_tc>( 
                        get_predicate_decl( 
                            rref_str( "equals" ), dummy_loc() ),
                        construct_n( 2, term_to_proper_type( pr.second ) ) ),
                    construct_n( 2, b_free() ) ),
                rref_list<typed_term_tc>{ pr.first, pr.second },
                0 );
            auto const unifier_pidx = construct<predicate_index_tc>(
                unifier_lit->apred,
                construct<index_info_tc::builtin>(
                    construct<builtin_pred_tc::equals>() ),
                index_category::BUILTIN );
            new_body_lits.push_back( { 
                unifier_lit, unifier_pidx, idx_lit.downstream_of_delta } );
            unified_one = true;
        }
        unifiers.clear();

        for( auto const t : idx_lit.lit->args ) {
            bind_vars( t, bound_vars );
        }
    }

    if( g_opt.verbose >= 2 ) {
        if( unified_one ) {
            cout << bt::format( "Modified %s\n---> %s\n" )
                % delimited( body_lits )
                % delimited( new_body_lits );
        }
    }

    return new_body_lits;
}

auto compile_to_join_edges(
    pred_scc_set const& scc,
    join_root& root,
    typed_rule const& rule,
    vector<indexed_literal>& body_lits )
    -> void
{
    auto head_lit = rule.head;
    bound_var_set bound_vars;
    join_edge_set* edges;
    unsigned join_no = 0u;

    auto const delta_lit = body_lits.front().lit;
    auto const delta_pidx = body_lits.front().pidx;
    if( delta_lit != construct_edb_lit() ) {
        auto& root_edges 
            = delta_pidx->category.get() != index_category::DIRECT 
                ? root.edges : root.direct_apreds[delta_pidx->apred];

        indexed_literal picked_join = get_unified_indexed_literal(
            root_edges, bound_vars, { delta_lit, delta_pidx, false },
            body_lits, head_lit );

        auto& new_edge = insert_join_edge( 
            root_edges,
            bound_vars,
            { join_edge::JOIN, join_no, picked_join } );
        new_edge.infos.push_back( { rule } );
        edges = &new_edge.next;
        join_no += 1u;

        // Bind vars
        for( auto const t : new_edge.index.lit->args ) {
            bind_vars( t, bound_vars );
        }
    }
    else {
        auto const head_name = head_lit->apred->pred->decl->name;
        edges = head_name == "@eval_before" ? &root.before_edges
            : head_name == "@eval_after" ? &root.after_edges
            : &root.edb_edges;
    }

    auto const rest = bt::make_iterator_range( 
        body_lits.begin()+1, body_lits.end() );
    for( auto const idx_lit : rest ) {
        auto const& picked_join = get_unified_indexed_literal(
            *edges, bound_vars, idx_lit,
            body_lits, head_lit );

        // Create demand insert
        auto const picked_apred = picked_join.pidx->apred;
        if( picked_join.downstream_of_delta
            && !all_free( picked_apred->bp )
            && picked_join.pidx->category.get()
                != index_category::BUILTIN )
        {
            auto dem_join_no = join_no;
            auto const demand_lit = construct_demand_literal(
                picked_join.lit );
            auto& demand_edges = insert_into_head( 
                rule, demand_lit, bound_vars, dem_join_no, edges );
            // Process worklist?
            auto const wl_type = picked_apred->pred->decl->direct.get();
            if( wl_type == wl_category::NORMAL
                && !set_contains( scc, picked_apred ) ) 
            {
                auto* pred_scc = env.pred_to_scc.const_at( picked_apred );
                auto& edge = insert_join_edge(
                    demand_edges, bound_vars,
                    { dem_join_no, *pred_scc } );
                edge.infos.push_back( { rule } );
            }
        }

        auto& new_edge = insert_join_edge(
            *edges, bound_vars,
            { join_edge::JOIN, join_no, picked_join } );
        new_edge.infos.push_back( { rule } );
        edges = &new_edge.next;
        join_no += 1u;

        // bind variables
        for( auto const t : new_edge.index.lit->args ) {
            bind_vars( t, bound_vars );
        }
    }
    insert_into_head( rule, head_lit, bound_vars, join_no, edges );
}

auto compile_rule(
    bt_unordered_set< rref<adorned_typed_predicate_tc> > const& scc,
    join_root& root,
    typed_rule const& rule )
{
    VERBOSE_PRINT( 2 )
        cout << "compiling " << rule.head << " :- "
            << delimited( rule.body ) << endl;

    for( size_t i = 0; i < rule.body.size(); ++i ) {
        auto const lit = rule.body[i];
        if( requires_delta( scc, rule, lit ) ) {
            VERBOSE_PRINT( 2 )
                cout << "    compiling delta rule for " << lit << endl;

            auto body_lits = pick_indices( scc, root, rule, i );
            if( !g_opt.no_term_offload ) {
                body_lits = separate_unification( body_lits );
            }
            compile_to_join_edges( scc, root, rule, body_lits );
        }
    }
}

auto magic_decorate_rule(
    bt_unordered_set< rref<adorned_typed_predicate_tc> > const& scc,
    join_root& root,
    typed_rule const& rule )
{
    for( auto const lit : rule.body ) {
        if( !lit->negated && set_contains( env.underivable, lit->apred ) ) {
            VERBOSE_PRINT( 2 )
                cout << "Skipping rule " << rule << endl;
            return;
        }
    }

    typed_rule new_rule;
    new_rule.loc = rule.loc;
    new_rule.head = rule.head;

    auto const head_dpred = rule.head->apred;
    if( !all_free( head_dpred->bp ) ) {
        new_rule.body.insert(
            new_rule.body.begin(),
            construct_demand_literal( rule.head ) );
    }

    if( requires_edb_rule( scc, rule ) ) {
        new_rule.body.insert(
            new_rule.body.begin(),
            construct_edb_lit() );
    }

    new_rule.body.insert(
        new_rule.body.end(),
        rule.body.begin(),
        rule.body.end() );

    compile_rule( scc, root, new_rule );
}

auto compile_evals(
    bt_unordered_set< rref<adorned_typed_predicate_tc> > const& scc,
    join_root& root,
    rref_vector<typed_literal_tc> const& rule,
    bool const before )
{
    typed_rule new_rule{ 
        dummy_loc(),
        construct_dummy_lit( before ? "@eval_before" : "@eval_after" ),
        {} };
    new_rule.body.insert(
        new_rule.body.begin(),
        construct_edb_lit() );
    new_rule.body.insert(
        new_rule.body.end(),
        rule.begin(), rule.end() );

    auto body_lits = pick_indices( scc, root, new_rule, 0 );
    if( !g_opt.no_term_offload ) {
        body_lits = separate_unification( body_lits );
    }
    compile_to_join_edges( scc, root, new_rule, body_lits );
}

auto compile_rules() -> void
{
    for( auto const& scc : ad::reverse( env.pred_sccs ) ) {
        VERBOSE_PRINT( 2 )
            cout << "compiling scc {" << delimited( scc ) << '}' << endl;
				
		size_t rules = 0, indices = 0;
        join_root root{ scc };
        for( auto const apred : scc ) {
            for( auto const rule : env.rules.at( apred ) ) {
                magic_decorate_rule( scc, root, rule );
            }
            for( auto const& body : find_default( env.eval_before, apred ) ) {
                compile_evals( scc, root, body, true );
            }
            for( auto const& body : find_default( env.eval_after, apred ) ) {
                compile_evals( scc, root, body, false );
            }

            if( apred->pred->decl->direct.get() != wl_category::NORMAL ) {
                // add entry to give it a possible empty function to call
                root.direct_apreds[apred];
            }
			rules += env.rules.at( apred ).size();
			indices += get_index_decls( apred ).size();
        }

        VERBOSE_PRINT( 2 ) {
            if( !root.edb_edges.empty() ) {
                cout << "EDB rules:\n"
                    << root.edb_edges << '\n';
            }
            if( !root.edges.empty() ) {
                cout << "Delta rules for {" 
                    << delimited( root.worklist_apreds ) << "}:\n"
                    << root.edges;
            }
            for( auto const& x : root.direct_apreds ) {
                cout << "Direct rule for " << x.first << ":\n"
                    << x.second;
            }
            cout << endl;
        }

        env.join_roots.push_back( move( root ) );
		VERBOSE_PRINT( 2 ) {
			cout << bt::format( "%d rules, %d indices.\n" )
				% rules % indices;
		}
    }

    static rref_set<adorned_typed_predicate_tc> const empty_scc{};
    for( auto const aggr : env.all_aggregates ) {
        auto const& x = aggr.get<typed_term_tc::aggregate>();
        VERBOSE_PRINT( 2 )
            cout << "compiling aggregate " << aggr << endl;
        join_root root{ empty_scc };

        typed_rule new_rule;
        new_rule.loc = x.loc;
        new_rule.head = construct_aggregate_lit( aggr );

        env.worklist_index.at( x.apred ) = construct<predicate_index_tc>(
            x.apred, construct<index_info_tc::direct>(), 
            index_category::DIRECT );

        auto const lit = construct<typed_literal_tc>(
            dummy_loc(), x.apred, 
            transform_to_list(
                zip( get_placeholder_list( x.input.size() ), 
                    x.apred->pred->types ),
                []( auto const& pr ) {
                    return construct<typed_term_tc::variable>( 
                        dummy_loc(), get<0>( pr ), get<1>( pr ) );
                } ),
            false );

        new_rule.body.push_back( lit );
        new_rule.body.insert( 
            new_rule.body.end(), 
            x.lits.begin(), x.lits.end() );

        compile_rule( empty_scc, root, new_rule );
        my_assert( root.direct_apreds.size() == 1
            && root.direct_apreds.begin()->second.size() == 1 );
        env.aggregate_join_edges.emplace( 
            aggr, move( root.direct_apreds.begin()->second.front() ) );
    }
}

//*****************************************************************************
template< typename T >
struct var_subst
{
    rref_str const name;
    mutable rref_str parent;
    mutable rref<T> non_var;
    explicit var_subst( rref_str const name0, rref_str const parent0 = {} ) 
        : name( name0 ),
        parent( parent0 ) 
    {}
};

template< typename T >
auto inline operator==( 
    var_subst<T> const& l, var_subst<T> const& r ) -> bool
{
    return l.name == r.name;
}

template< typename T >
auto inline hash_value( var_subst<T> const& x ) -> size_t
{
    return hash_value( x.name );
}

using var_term_subst = var_subst<typed_term_tc>;
using unifier_type = bt_unordered_set<var_term_subst>;
using literal_unifier_type = rref_map<
    typed_literal_tc,rref_list<typed_literal_tc>>;

environment tenv;

template< typename T >
auto find_root( 
    bt_unordered_set<T> const& unifier, 
    typename bt_unordered_set<T>::value_type const& n ) 
    -> typename bt_unordered_set<T>::value_type const&
{
    using node_type = typename bt_unordered_set<T>::value_type;
    if( n.parent ) {
        auto& newn = find_root( 
            unifier, must_find( unifier, node_type{ n.parent } ) );
        n.parent = newn.name;
        my_assert( n.name != n.parent );
        return newn;
    }
    return n;
}


auto find_node( unifier_type& unifier, rref_str const name ) 
    -> var_term_subst const&
{
    auto const& ret = unifier.emplace( name );
    return find_root( unifier, *ret.first );
}

auto unified_term( 
    unifier_type const& unifier, rref<typed_term_tc> const t ) 
    -> rref<typed_term_tc>;

auto unified_variable( 
    unifier_type& unifier, typed_term_tc::variable const& x ) 
    -> rref<typed_term_tc>
{
    auto const& node = find_node( unifier, x.name );
    return node.non_var 
        ? unified_term( unifier, node.non_var )
        : construct<typed_term_tc::variable>( x.loc, node.name, x.type );
}

auto unified_variable( 
    unifier_type const& unifier, typed_term_tc::variable const& x ) 
    -> rref<typed_term_tc>
{
    auto const iter = unifier.find( var_term_subst( x.name ) );
    if( iter != unifier.end() ) {
        auto const& node = find_root( unifier, *iter );
        return node.non_var 
            ? unified_term( unifier, node.non_var )
            : construct<typed_term_tc::variable>( x.loc, node.name, x.type );
    }
    return construct<typed_term_tc::variable>( x.loc, x.name, x.type );
}

auto unified_literal(
    unifier_type const& unifier, rref<typed_literal_tc> const lit )
    -> rref<typed_literal_tc>;

auto unified_term( 
    unifier_type const& unifier, rref<typed_term_tc> const t ) 
    -> rref<typed_term_tc>
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
        return t;
    TYPE_CASE_X( string )
        return t;
    TYPE_CASE_X( variable )
        return unified_variable( unifier, x );
    TYPE_CASE_X( anonymous )
        return t;
    TYPE_CASE_X( compound )
        return construct<typed_term_tc::compound>(
            x.loc, x.dconstr, 
            transform_to_list( 
                x.inner, 
                bind( &unified_term, std::cref( unifier ), _1 ) ) );
    TYPE_CASE_X( function )
        return construct<typed_term_tc::function>(
            x.loc, x.func, 
            transform_to_list( 
                x.inner, 
                bind( &unified_term, std::cref( unifier ), _1 ) ) );
    TYPE_CASE_X( conditional )
        return construct<typed_term_tc::conditional>(
            x.loc, x.type, 
            unified_term( unifier, x.cond ),
            unified_term( unifier, x.true_block ), 
            unified_term( unifier, x.false_block ) );
    TYPE_CASE_X( aggregate )
        return construct<typed_term_tc::aggregate>(
            x.loc, x.type, x.apred,
            transform_to_list( 
                x.input, 
                bind( &unified_term, std::cref( unifier ), _1 ) ),
            x.output, x.lits );
    END_TYPE_SWITCH_
}

auto unified_literal(
    unifier_type const& unifier, rref<typed_literal_tc> const lit )
    -> rref<typed_literal_tc>
{
    return construct<typed_literal_tc>(
        lit->loc, lit->apred, 
        transform_to_list( 
            lit->args, 
            [&]( auto const t ) { return unified_term( unifier, t ); } ), 
        lit->negated );
}

auto var_bind(
    unifier_type& unifier,
    rref<location_tc> const loc,
    var_term_subst const& n, rref<typed_term_tc> const tgt ) -> void
{
    my_assert( !n.non_var );
    my_assert( !n.parent );
    if( tgt.is<typed_term_tc::variable>() ) {
        auto const& tgtn = find_node( 
            unifier, tgt.get<typed_term_tc::variable>().name );
        if( n.name == tgtn.name ) {
            return;
        }
        if( tgtn.non_var ) {
            VERBOSE_PRINT( 3 )
                cout << bt::format( 
                    "\tUnified %s with %s.\n" )
                    % n.name %tgtn.non_var;
            n.non_var = tgtn.non_var;
        }
        else {
            VERBOSE_PRINT( 3 )
                cout << bt::format( 
                    "\tUnified %s with %s.\n" )
                    % n.name % tgtn.name;
            n.parent = tgtn.name;
        }
    }
    //else if( set_contains( free_type_vars( tgt ), name ) ) {
    //    cerr << bt::format( "%s: Occurs check failed binding '%s' to %s.\n" )
    //        % loc % name % tgt;
    //    exit( 1 );
    //}
    else {
        VERBOSE_PRINT( 3 )
            cout << bt::format( 
                "\tUnified %s with %s.\n" )
                % n.name % tgt;
        n.non_var = tgt;
    }
}

auto unify_terms( 
    unifier_type& unifier,
    rref<typed_term_tc> const l, rref<typed_term_tc> const r ) 
    -> bool
{
    if( unlocated_equality{}( l, r ) ) {
        return true;
    }
    if( l.is<typed_term_tc::anonymous>() 
        || r.is<typed_term_tc::anonymous>() ) 
    {   // TODO: get rid of anonymous variable type
        return true;
    }
    if( l.is<typed_term_tc::variable>() && r.is<typed_term_tc::variable>() ) {
        auto const& lv = l.get<typed_term_tc::variable>();
        auto const& rv = r.get<typed_term_tc::variable>();
        auto& ln = find_node( unifier, lv.name );
        auto& rn = find_node( unifier, rv.name );
        if( ln.non_var ) {
            return unify_terms( unifier, ln.non_var, r );
        }
        if( rn.non_var ) {
            return unify_terms( unifier, l, rn.non_var );
        }
        rg::count( ln.name.str(), '\'' ) <= rg::count( rn.name.str(), '\'' )
            ? var_bind( unifier, rv.loc, rn, l )
            : var_bind( unifier, lv.loc, ln, r );        
        return true;
    }
    if( l.is<typed_term_tc::variable>() ) {
        auto const& lv = l.get<typed_term_tc::variable>();        
        auto& ln = find_node( unifier, lv.name );
        if( ln.non_var ) {
            return unify_terms( unifier, ln.non_var, r );
        }
        var_bind( unifier, lv.loc, ln, r );
        return true;
    }
    if( r.is<typed_term_tc::variable>() ) {
        auto const& rv = r.get<typed_term_tc::variable>();
        auto& rn = find_node( unifier, rv.name );
        if( rn.non_var ) {
            return unify_terms( unifier, l, rn.non_var );
        }
        var_bind( unifier, rv.loc, rn, l );
        return true;
    }
    rref_list<typed_term_tc> li, ri;
    if( l.get_tag() != r.get_tag() ) {
        if( l.is<typed_term_tc::function>() 
            || r.is<typed_term_tc::function>() )
        {
            cerr << bt::format( 
                "%s: Unification of function terms not supported for now.\n" )
                % ( l.is<typed_term_tc::function>() 
                    ? get_location( l ) : get_location( r ) );
            exit( 1 );
        }

        goto failure;
    }
    BEGIN_TYPE_SWITCH_( typed_term_tc, l )
    TYPE_CASE_X( i32 )
        goto failure;
    TYPE_CASE_X( string )
        goto failure;
    TYPE_CASE_X( compound )
        auto const& y = r.get<typed_term_tc::compound>();
        if( x.dconstr != y.dconstr ) {
            goto failure;
        }
        li = x.inner;
        ri = y.inner;
    TYPE_CASE_X( function )
        auto const& y = r.get<typed_term_tc::function>();
        if( x.func != y.func ) {
            cerr << bt::format( 
                "%s: Unification of function terms not supported.\n" )
                % ( l.is<typed_term_tc::function>() 
                    ? get_location( l ) : get_location( r ) );
            exit( 1 );
        }
        li = x.inner;
        ri = y.inner;
    TYPE_CASE_X( conditional )
        auto const& y = r.get<typed_term_tc::conditional>();
        li = rref_list<typed_term_tc>{ 
            x.cond, x.true_block, x.false_block };
        ri = rref_list<typed_term_tc>{ 
            y.cond, y.true_block, y.false_block };
    TYPE_CASE_X( aggregate )
        auto const& y = r.get<typed_term_tc::aggregate  >();
        li = x.input;
        ri = y.input;
    END_TYPE_SWITCH_

    for( auto const& y : zip( li, ri ) ) {
        if( !unify_terms( unifier, get<0>( y ), get<1>( y ) ) ) {
            goto failure;
        }
    }
    return true;

failure:
    VERBOSE_PRINT( 3 )
        cout << bt::format( "\tFailure unifying %s with %s.\n" )
            % l % r;
    return false;
}

auto unify_literals( 
    unifier_type& unifier,
    rref<typed_literal_tc> const l, rref<typed_literal_tc> const r ) 
    -> bool
{
    my_assert( l->apred == r->apred );
    for( auto const& y : zip( l->args, r->args ) ) {
        if( !unify_terms( unifier, get<0>( y ), get<1>( y ) ) ) {
            return false;
        }
    }
    return true;
}

auto rule_contains_inlines( typed_rule const& rule )
{
    for( auto const lit : rule.body ) {
        if( set_contains( env.inline_predicates, lit->apred->pred->decl ) ) {
            return true;
        }
    }
    return set_contains( env.inline_predicates, rule.head->apred->pred->decl );
}

auto expand_inlines( 
    typed_rule const& rule,
    function< void( typed_rule&& ) > const& func )
    -> void;

auto expand_inlines_inner( 
    typed_rule const& rule,
    size_t const idx,
    unifier_type const& unifier,
    unifier_map const& var_map,
    literal_unifier_type& lit_unifier,
    function< void( typed_rule&& ) > const& func )
    -> void
{
    auto const lit = idx < rule.body.size() ? rule.body[idx] 
        : idx == rule.body.size() ? rule.head
        : rref<typed_literal_tc>();
    if( !lit ) {
        auto const head_list = find_default( lit_unifier, rule.head );
        if( head_list && head_list.size() != 1 ) {
            cerr << bt::format( 
                "%s: Inline predicate in the head of a rule must "
                "be defined only by rules having exactly one "
                "literal in their bodies.\n" )
                % rule.head;
        }

        rref<typed_literal_tc> head = unified_literal(
            unifier,
            head_list ? head_list.head() : rule.head );

        rref_vector<typed_literal_tc> body;
        for( auto const lit : rule.body ) {
            auto const lit_list = find_default( lit_unifier, lit );
            if( lit_list ) {
                for( auto const y : lit_list ) {
                    body.push_back( unified_literal( unifier, y ) );
                }
            }
            else {
                body.push_back( unified_literal( unifier, lit ) );
            }
        }

        typed_rule new_rule{ rule.loc, head, move( body ) };
        if( rule_contains_inlines( new_rule ) ) {
            expand_inlines( new_rule, func );
        }
        else {
            func( move( new_rule ) );
        }
        return;
    }

    auto const apred = lit->apred;
    if( set_contains( env.inline_predicates, apred->pred->decl ) ) {
        if( lit->negated.get() != 0 ) {
            cerr << bt::format( 
                "%s: Inline predicates cannot be negated.\n" )
                % lit->loc;
            exit( 1 );
        }

        for( auto const& irule : find_default( env.inline_rules, apred ) ) {
            auto new_var_map = var_map;
            auto const tgt_head = remap_literal( irule.head, new_var_map );
            auto const tgt_body = transform_to_list(
                irule.body, 
                bind( &remap_literal, _1, std::ref( new_var_map ) ) );
            new_var_map.clear();

            auto new_unifier = unifier;
            VERBOSE_PRINT( 3 )
                cout << bt::format( 
                    "\tUnifying %s with %s.\n" )
                    % tgt_head % lit;

            if( unify_literals( new_unifier, tgt_head, lit ) ) {
                lit_unifier[lit] = tgt_body;
                VERBOSE_PRINT( 3 )
                    cout << bt::format( 
                        "\tSuccess.\n"
                        "\tReplaced %s with %s.\n" )
                        % lit % tgt_body;

                free_variables( tgt_head, new_var_map.tgts );
                for( auto const tgt_lit : tgt_body ) {
                    free_variables( tgt_lit, new_var_map.tgts );
                }

                expand_inlines_inner( 
                    rule, idx+1, new_unifier, new_var_map, 
                    lit_unifier, func );
            }
            else {
                VERBOSE_PRINT( 3 )
                    cout << "\tFailure.\n";
            }
        }
    }
    else {
        expand_inlines_inner( 
            rule, idx+1, unifier, var_map, lit_unifier, func );
    }
}

auto expand_inlines( 
    typed_rule const& rule,
    function< void( typed_rule&& ) > const& func )
    -> void
{
    unifier_map var_map;
    free_variables( rule.head, var_map.tgts );
    for( auto const lit : rule.body ) {
        free_variables( lit, var_map.tgts );
    }

    literal_unifier_type lit_unifier;
    return expand_inlines_inner( 
        rule, 0, {}, var_map, lit_unifier, func );
}

auto expand_inlines() -> void
{
    rref_map<predicate_decl_tc,unsigned> num_inline_rules;
    for( auto const& pr : env.inline_rules ) {
        auto const pred_decl = pr.first->pred->decl;
        num_inline_rules[pred_decl] += 1;
    }

    for( auto const pred_decl : env.inline_predicates ) {
        if( find_default( num_inline_rules, pred_decl, 0 ) == 0
            && g_opt.warning >= 1 ) 
        {
            cerr << bt::format( 
                "Warning: '%s' is declared inline but has no inline rules.\n" )
                % pred_decl->name;
        }
    }

    decltype(env.rules) new_rules;
    for( auto const& pred_rules : env.rules ) {
        for( auto const rule : pred_rules ) {
            if( rule_contains_inlines( rule ) ) {
                VERBOSE_PRINT( 2 )
                    cout << rule << " expansions:\n";
                expand_inlines( 
                    rule, 
                    [&]( typed_rule&& new_rule ) 
                { 
                    new_rules.at( new_rule.head->apred ).push_back( 
                        move( new_rule ) );
                    VERBOSE_PRINT( 2 )
                        cout << '\t' << new_rule.head 
                            << " :- " << delimited( new_rule.body ) << '\n';
                } );
            }
            else {
                new_rules.at( rule.head->apred ).push_back( rule );
            }
        }
    }
    env.rules.swap( new_rules );
}
