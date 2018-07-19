#include "stdafx.h"
#include "utility.h"

#include "ds.h"
#include "main.h"
#include "table.h"

char const* const asterisks =
    "****************************************"
    "****************************************";

environment env;


//*****************************************************************************
// Predicate dependency graph
//*****************************************************************************
auto collect_literal(
    rref<typed_literal_tc> const x,
    rref<adorned_typed_predicate_tc> const head_apred )
    -> void;

auto collect_term(
    rref<typed_term_tc> const t,
    rref<adorned_typed_predicate_tc> const head_apred )
    -> void
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 )
    TYPE_CASE_X( string )
    TYPE_CASE_X( variable )
    TYPE_CASE_X( anonymous )
    TYPE_CASE_X( compound )
        env.all_proper_atypes.emplace( x.dconstr->type );
        for( auto const y : x.inner ) {
            collect_term( y, head_apred );
        }
    TYPE_CASE_X( function )
        if( env.all_typed_functions.insert( x.func ).second ) {
            for( auto const fdefn 
                : find_default( env.function_defns, x.func->decl->name ) ) 
            {
                if( fdefn->args.size() == x.func->arg_types.size() ) {
                    auto const ifunc = instantiate_function( x.func, fdefn );
                    for( auto const y : ifunc.first ) {
                        collect_term( y, head_apred );
                    }
                    collect_term( ifunc.second, head_apred );
                }
            }
        }
        for( auto const y : x.inner ) {
            collect_term( y, head_apred );
        }
    TYPE_CASE_X( conditional )
        collect_term( x.cond, head_apred );
        collect_term( x.true_block, head_apred );
        collect_term( x.false_block, head_apred );
    TYPE_CASE_X( aggregate )
        env.all_aggregates.insert( t );
        if( head_apred ) {
            env.aggr_succs[t].insert( head_apred );
        }
        for( auto const y : x.input ) {
            collect_term( y, head_apred );
        }
        for( auto const lit : x.lits ) {
            collect_literal( lit, head_apred );
        }
    END_TYPE_SWITCH_
};

auto collect_literal(
    rref<typed_literal_tc> const lit,
    rref<adorned_typed_predicate_tc> const head_apred )
    -> void
{
    env.apreds.emplace( lit->apred );
    env.all_apreds.emplace( lit->apred );
    env.all_apreds.emplace(
        construct_demand_predicate( lit->apred ) );
    if( head_apred && lit->apred != head_apred ) {
        env.pred_succs.at( lit->apred ).emplace( head_apred );
    }
    for( auto const y : lit->args ) {
        collect_term( y, head_apred );
    }
};

auto collect_body( 
    rref_vector<typed_literal_tc> const& body,
    rref<adorned_typed_predicate_tc> const head_apred )
{
    for( auto const lit : body ) {
        collect_literal( lit, head_apred );
    }
}

auto collect_rule( typed_rule const& rule ) 
    -> void
{
    collect_literal( rule.head, rule.head->apred );
    env.apreds.emplace( rule.head->apred );
    env.all_apreds.emplace( rule.head->apred );
    env.all_apreds.emplace(
        construct_demand_predicate( rule.head->apred ) );
    env.defined.emplace( rule.head->apred );
    collect_body( rule.body, rule.head->apred );
}

// Initializes
// - env.pred_sccs
auto compute_dep_graph() -> void
{
    // collect all atype types and typed predicates including demand versions
    for( auto const& pred_rules : env.rules ) {
        for( auto const& rule : pred_rules ) {
            collect_rule( rule );
        }
    }
    for( auto const& pr : rg::join( env.eval_before, env.eval_after ) ) 
    {
        for( auto const& body : pr.second ) {
            collect_body( body, pr.first );
        }
    }
    for( auto const& declpr : env.function_decls ) {
        auto const decl = declpr.second;

        if( is_proper_type( decl->result_type, {} )
            && ag::all_of( decl->arg_types, 
            []( auto const y ) { return is_proper_type( y, {} ); } ) ) 
        {
            auto const tfunc = construct<typed_function_tc>( 
                decl,
                to_proper_type( decl->result_type, {} ),
                to_proper_types( decl->arg_types, {} ) );
            if( env.all_typed_functions.insert( tfunc ).second ) {
                for( auto const fdefn 
                    : find_default( env.function_defns, decl->name ) ) 
                {
                    if( fdefn->args.size() == tfunc->arg_types.size() ) {
                        auto const ifunc = instantiate_function( 
                            tfunc, fdefn );
                        for( auto const y : ifunc.first ) {
                            collect_term( y, {} );
                        }
                        collect_term( ifunc.second, {} );
                    }
                }
            }
        }
    }

    // compute sccs
    scc_context<adorned_typed_predicate_tc> ctxt;
    for( auto const pred : env.apreds ) {
        if( ctxt.nodes.at( pred ).index == (unsigned)-1 ) {
            strongly_connect( ctxt, env.pred_succs, env.pred_sccs, pred );
        }
    }

    // check stratification
    for( auto const& apred_scc : env.pred_sccs ) {
        for( auto const apred : apred_scc ) {
            env.pred_to_scc.at( apred ) = &apred_scc;
            for( auto const rule : env.rules.at( apred ) ) {
                for( auto const lit : rule.body ) {
                    if( lit->negated.get() == 1
                        && set_contains( apred_scc, lit->apred ) )
                    {
                        cerr << bt::format(
                            "%s: The negative appearance of "
                            "'%s' is unstratified:\n"
                            "%s :- %s\n" )
                            % lit->loc
                            % lit->apred
                            % rule.head
                            % delimited( rule.body );
                    }
                }
            }
            for( auto const& body : find_default( env.eval_before, apred ) ) {
                for( auto const lit : body ) {
                    if( set_contains( apred_scc, lit->apred ) ) {
                        cerr << bt::format(
                            "Predicate '%1%' used in an eval_before rule"
                            "must be in a lower stratification than '%2%'.\n" )
                            % lit->apred % apred;
                    }
                }
            }

        }
    }
    for( auto const aggr : env.all_aggregates ) {
        for( auto const apred : find_default( env.aggr_succs, aggr ) ) {
            auto const* apred_scc = env.pred_to_scc.at( apred );
            auto const& x = aggr.get<typed_term_tc::aggregate>();
            for( auto const lit : x.lits ) {
                if( set_contains( *apred_scc, lit->apred ) ) {
                    cerr << bt::format(
                        "%s: %s is not an EDB predicates "
                        "in an aggregate term.\n" )
                        % lit->loc
                        % lit->apred;
                }
            }
        }
    }
}

//*****************************************************************************
// Index processing
//*****************************************************************************
auto root_index_node()
{
    return construct<index_node_tc::root>();
}

auto root_structure() -> rref<index_structure_tc>
{
    return must_find( env.index_structures, root_index_node() );
}

auto get_bp_from_arg_list(
    rref_list<i32_tc> const args,
    rref_list<binding_tc> const bp,
    unsigned const index )
    -> rref_list<binding_tc> const
{
    if( bp.empty() ) {
        return nil<binding_tc>();
    }
    else {
        bool const bound = ag::any_of(
            args,
            [&]( uint32_t const x ) { return x == index; } );
        return cons(
            bound ? b_bound() : bp.head(),
            get_bp_from_arg_list(
                args,
                bp.rest(),
                index+1 ) );
    }
}

auto get_free_args(
    rref_list<binding_tc> const bp,
    size_t const idx = 0 )
    -> rref_list<i32_tc>
{
    if( bp.empty() ) {
        return nil<i32_tc>();
    }
    else {
        if( bp.head().is<free_t>() ) {
            return cons(
                rref<i32_tc>( idx ),
                get_free_args( bp.rest(), idx+1 ) );
        }
        else {
            return get_free_args( bp.rest(), idx+1 );
        }
    }
}

template< typename Range >
auto filter_bound(
    Range const& list,
    rref_list<binding_tc> const bp )
    -> rref_list<
        typename remove_reference<decltype(*bt::begin( list ))>
            ::type::inner_type>
{
    using ref_type = remove_reference_t<decltype(*bt::begin( list ))>;
    using value_type = typename ref_type::inner_type;
    return foldr(
        zip( list, bp ),
        []( auto const& tpl, rref_list<value_type> const rest )
        {
            return get<1>( tpl ) == b_bound()
                ? cons( get<0>( tpl ), rest )
                : rest;
        },
        nil<value_type>() );
}

auto construct_demand_predicate(
    rref<adorned_typed_predicate_tc> const apred )
    -> rref<adorned_typed_predicate_tc>
{
    if( all_free( apred->bp ) ) {
        return apred;
    }

    auto& ret = env.demand_predicates.at( apred );
    if( ret ) {
        return ret;
    }
    stringstream name;
    name << '@' << apred->pred->decl->name << apred->bp;
    auto const demand_types = filter_bound( apred->pred->types, apred->bp );

    auto const demand_decl = construct<predicate_decl_tc>(
        dummy_loc(),
        rref_str( name.str() ),
        transform_to_list(
            demand_types,
            []( auto const x )
            { return to_atype( x ); } ),
        apred->pred->decl->direct,
        false,
        false );

    auto const demand_pred = construct<typed_predicate_tc>(
        demand_decl, demand_types );
    ret = construct<adorned_typed_predicate_tc>(
        demand_pred,
        construct_n( demand_types.size(), b_free() ) );
    env.parent_predicates.at( ret ) = apred;
    return ret;
}

auto get_parent(
    rref<adorned_typed_predicate_tc> const apred )
    -> rref<adorned_typed_predicate_tc>
{
    return env.parent_predicates.at( apred );
}

auto create_identity_literal(
    rref<adorned_typed_predicate_tc> const apred )
    -> rref<typed_literal_tc>
{
    auto const pred = apred->pred;
    size_t i = 0;
    auto const args = transform_to_list(
        pred->types,
        [&]( rref<proper_type_tc> const type ) -> rref<typed_term_tc>
    {
        return construct<typed_term_tc::variable>(
            dummy_loc(),
            rref_str( "#" + to_string( i++ ) ),
            type );
    } );
    return construct<typed_literal_tc>(
        dummy_loc(),
        apred,
        args,
        0 );
}

auto predicate_is_recursive(
    rref<adorned_typed_predicate_tc> const apred,
    bt_unordered_set< rref<adorned_typed_predicate_tc> > const& scc )
{
    my_assert( set_contains( scc, apred ) );
    for( auto const rule : env.rules.at( apred ) ) {
        for( auto const lit : rule.body ) {
            if( set_contains( scc, lit->apred ) ) {
                return true;
            }
        }
    }
    return false;
}

auto get_index_decls( 
    rref<adorned_typed_predicate_tc> atpred )
    -> vector<rref<index_decl_tc>> const&
{
    bool const is_demand = (bool)env.parent_predicates.at( atpred );
    auto const parent_tpred = !is_demand
        ? atpred 
        : env.parent_predicates.at( atpred );
    auto const parent_apred = construct<adorned_predicate_tc>(
        parent_tpred->pred->decl->name,
        parent_tpred->bp );

    return !is_demand
        ? env.index_decls.at( parent_apred )
        : env.demand_index_decls.at( parent_apred );
}

auto make_index_node(
    rref<index_descriptor_tc> const descr,
    rref<index_node_tc> const parent,
    rref<adorned_typed_predicate_tc> const apred,
    rref<index_decl_tc> const decl )
    -> rref_list<index_node_tc>
{
    auto const types = apred->pred->types;
    BEGIN_TYPE_SWITCH( index_descriptor_tc, descr )
    TYPE_CASE_DEFAULT
        auto& children = env.index_children[parent];
        auto const node = construct<index_node_tc::leaf>(
            parent, apred, decl );
        if( env.index_offsets.insert( { 
            node, (unsigned)children.size() } ).second ) 
        {
            children.push_back( node );
        }
        
        return cons( node );
    TYPE_CASE_X( source )
        my_assert( parent.is<index_node_tc::root>() );
        return nil<index_node_tc>();
    TYPE_CASE_X( sink )
        my_assert( parent.is<index_node_tc::root>() );
        return nil<index_node_tc>();
    TYPE_CASE_X( array_map )
        auto& children = env.index_children[parent];
        auto const node = construct<index_node_tc::single_map>(
            parent, types[x.arg] );
        if( !seq_contains( children, node ) ) {
            my_assert( !map_contains( env.index_offsets, node ) );
            env.index_offsets[node] = (unsigned)children.size();
            children.push_back( node );
        }
        else {
            my_assert( map_contains( env.index_offsets, node ) );
        }
        return cons( node, make_index_node( x.descr, node, apred, decl ) );
    TYPE_CASE_X( hash_map )
        auto& children = env.index_children[parent];
        auto const node = construct<index_node_tc::multi_map>(
            parent, slice( types, x.args ), 
            apred ); // turn off hash_map merging
        if( !seq_contains( children, node ) ) {
            my_assert( !map_contains( env.index_offsets, node ) );
            env.index_offsets[node] = (unsigned)children.size();
            children.push_back( node );
        }
        else {
            my_assert( map_contains( env.index_offsets, node ) );
        }
        return cons( node, make_index_node( x.descr, node, apred, decl ) );
    END_TYPE_SWITCH_
}

struct make_inner_structures
{
    rref<index_node_tc> const index;

    auto operator()()
    {
        type_switch( *this, index );
    }

    template< typename T >
    auto operator()( T const& x ) -> void // array_map, hash_map, subidx
    {
        rref_vector<index_node_tc> children;
        for( auto const node : env.index_children[index] ) {
            make_inner_structures{ node }();
            children.push_back( node );
        }

        env.index_structures[index] = construct<index_structure_tc>(
            index,
            concat( children ) );
    }
    auto operator()( index_node_tc::leaf const& x ) -> void
    {
    }
    auto operator()( index_node_tc::term_set const& x ) -> void
    {
    }
    auto operator()( index_node_tc::worklist const& x ) -> void
    {
    }
};

auto get_index_info_inner(
    rref<index_descriptor_tc> const descr,
    rref_list<proper_type_tc> const types,
    rref<location_tc> const loc,
    rref_list<index_node_tc> const inners,
    rref_list<binding_tc> const terminal_bp )
    -> rref<index_info_tc>
{
    my_assert( terminal_bp.size() == types.size() );
    auto const node = !inners.empty() ? inners.head() : rref<index_node_tc>();

    BEGIN_TYPE_SWITCH_( index_descriptor_tc, descr )
    TYPE_CASE_X( array_map )
        auto const structure = must_find( env.index_structures, node );

        // check validity
        auto const type = types[x.arg];
        auto const& type_decl = get_data_decl( 
            loc,
            type->name );
        if( type_decl.dconstrs.size() > 1 ) {
            cerr << bt::format(
                "%s: Array maps can only be constructed "
                "on types with only one data constructor.\n" )
                % loc;
            exit( 1 );
        }

        auto const new_bp = get_bp_from_arg_list(
            rref_list<i32_tc>{ x.arg }, terminal_bp );
        return env.index_infos[node] 
            = construct<index_info_tc::array_map>(
                x.arg,
                x.cost,
                must_find( env.index_offsets, node ),
                structure,
                get_index_info_inner(
                    x.descr, types, loc, inners.rest(), new_bp ) );
    TYPE_CASE_X( hash_map )
        auto const structure = must_find( 
            env.index_structures, node );

        auto const new_bp = get_bp_from_arg_list(
            x.args, terminal_bp );
        return env.index_infos[node] 
            = construct<index_info_tc::hash_map>(
                x.args,
                x.cost,
                must_find( env.index_offsets, node ),
                structure,
                get_index_info_inner(
                    x.descr, types, loc, inners.rest(), new_bp ) );
    TYPE_CASE_X( boolean )
        // check validity
        auto const num_free = rg::count( terminal_bp, b_free() );
        if( num_free > 0 ) {
            cerr << bt::format(
                "%s: Boolean indices must have no free arguments.\n" )
                % loc;
            exit( 1 );
        }

        return env.index_infos[node] 
            = construct<index_info_tc::boolean>( 
                must_find( env.index_offsets, node ) );
    TYPE_CASE_X( value )
        auto const free_args = get_free_args( terminal_bp );

        // check validity
        if( free_args.empty() ) {
            cerr << bt::format(
                "%s: Value indices must have at least one argument.\n" )
                % loc;
            exit( 1 );
        }

        return env.index_infos[node] 
            = construct<index_info_tc::value>( 
                free_args, must_find( env.index_offsets, node ) );
    TYPE_CASE_X( value_list )
        auto const free_args = get_free_args( terminal_bp );
        return env.index_infos[node] 
            = construct<index_info_tc::value_list>( 
                free_args, x.cost,
                must_find( env.index_offsets, node ) );
    TYPE_CASE_X( hash_set )
        auto const free_args = get_free_args( terminal_bp );
        return env.index_infos[node] 
            = construct<index_info_tc::hash_set>( 
                free_args, x.cost,
                must_find( env.index_offsets, node ) );
    TYPE_CASE_X( bitmap )
        auto const free_args = get_free_args( terminal_bp );
        if( free_args.size() != 1 ) {
            cerr << bt::format(
                "%s: Bitmap indices must have exactly one argument.\n" )
                % loc;
            exit( 1 );
        }
        return env.index_infos[node] 
            = construct<index_info_tc::bitmap>( 
                free_args.head(), x.cost,
                must_find( env.index_offsets, node ) );
    TYPE_CASE_X( replace )
        auto const free_args = get_free_args( terminal_bp );
        return env.index_infos[node] 
            = construct<index_info_tc::replace>( 
                free_args,
                must_find( env.index_offsets, node ) );
    TYPE_CASE_X( join )
        auto const free_args = get_free_args( terminal_bp );
        my_assert( free_args.size() == 1 );

        auto const type = types[free_args.head()];
        auto const func_decl = find_default(
            env.function_decls,
            { x.function, 2 } );
        if( !func_decl ) {
            cerr << bt::format(
                "%s: Unknown function '%s'.\n" )
                % loc
                % x.function;
            exit( 1 );
        }

        auto const tfunc = construct<typed_function_tc>(
            func_decl,
            type,
            rref_list<proper_type_tc>{ type, type} );

        return env.index_infos[node] 
            = construct<index_info_tc::join>(
                free_args.head(), tfunc,
                must_find( env.index_offsets, node ) );
    TYPE_CASE_X( source )
        // check validity
        for( auto const type : types ) {
            if( type != env.i32_type && type != env.string_type ) {
                cerr << bt::format(
                    "%s: Only i32 and string types may "
                    "be used in source indices.\n" )
                    % loc;
                exit( 1 );
            }
            
            if( !all_free( terminal_bp ) ) {
                cerr << bt::format(
                    "%s: Source indices must not be nested.\n" )
                    % loc;
                exit( 1 );
            }
        }

        auto const wtypes = transform_to_list(
            types,
            []( rref<proper_type_tc> const t ) -> rref<i32_tc>
            {
                return t == env.i32_type ? 0
                    : t == env.string_type ? 1
                    : ( die_exp() );
            } );

        return construct<index_info_tc::source>( x.name, x.cost, wtypes );
    TYPE_CASE_X( sink )
        size_t i = (size_t)-1;
        for( size_t k = 0; k < env.file_names.size(); ++k ) {
            if( env.file_names[k] == x.name ) {
                i = k;
                break;
            }
        }

        if( i == (size_t)-1 ) {
            i = env.file_names.size();
            env.file_names.push_back( x.name );
        }

        return construct<index_info_tc::sink>( (unsigned)i );
    END_TYPE_SWITCH_
}

auto get_index_info(
    rref<index_descriptor_tc> const descr,
    rref_list<proper_type_tc> const types,
    rref<location_tc> const loc,
    rref_list<index_node_tc> const inners )
    -> rref<index_info_tc>
{
    return get_index_info_inner( 
        descr,
        types,
        loc,
        inners,
        construct_n( types.size(), b_free() ) );
}

auto process_indices() -> void
{
    // check that all subindices are defined for all data constructors
    // TODO:

    // populate root
    auto const root_node = construct<index_node_tc::root>();
    auto& root_children = env.index_children[root_node];

    // add term sets
    for( auto const ptype : env.all_proper_atypes ) {
        auto const& dconstrs = env.data_decls[ptype->name].dconstrs;
        for( auto const dconstr : dconstrs ) {
            auto const pdconstr = get_proper_dconstr( 
                dconstr->name, ptype );

            env.proper_dconstrs.at( ptype ).push_back( pdconstr );
            env.dconstr_offset.at( pdconstr ) = root_children.size();

            root_children.push_back(
                construct<index_node_tc::term_set>( pdconstr ) );
        }
    }

    // add worklists
    rref_vector<adorned_typed_predicate_tc> worklist_required;
    for( auto const& pred_scc : env.pred_sccs ) {
        for( auto const apred : pred_scc ) {
            if( predicate_is_recursive( apred, pred_scc ) ) {
                worklist_required.push_back( apred );
            }
            if( !all_free( apred->bp ) ) {
                auto const demand_apred 
                    = construct_demand_predicate( apred );
                worklist_required.push_back( demand_apred );
            }
        }
    }

    for( auto const apred : worklist_required ) {
        if( !apred->pred->decl->direct.get() ) {
            env.worklist_index.at( apred ) 
                = construct<predicate_index_tc>(
                    apred,
                    construct<index_info_tc::worklist>(
                        root_children.size() ),
                    index_category::WORKLIST );
            root_children.push_back(
                construct<index_node_tc::worklist>( apred ) );
        }
        else {
            env.worklist_index.at( apred )
                = construct<predicate_index_tc>(
                    apred,
                    construct<index_info_tc::direct>(),
                    index_category::DIRECT );
        }

    }

    // make bstructure
    array_index<index_decl_tc,rref_list<index_node_tc>> root_chains;
    auto const root = root_index_node();
    for( auto const apred : env.all_apreds ) {
        for( auto const index_decl : get_index_decls( apred ) ) {
            root_chains.at( index_decl ) = make_index_node(
                index_decl->descr,
                construct<index_node_tc::root>(), 
                apred, index_decl );
        }
    }

    // make structure
    make_inner_structures{ root }();

    for( auto const apred : env.all_apreds ) {
        auto const parent = env.parent_predicates.at( apred );
        auto const pred_decl = apred->pred->decl;

        for( auto const index_decl : get_index_decls( apred ) ) {
            auto const chain = root_chains.const_at( index_decl );
            auto const idx_info = get_index_info(
                index_decl->descr,
                apred->pred->types,
                index_decl->loc,
                chain );

            auto const pidx = construct<predicate_index_tc>(
                apred,
                idx_info,
                index_decl->category );

            if( index_decl->category.get() == index_category::PRIMARY ) {
                if( env.primary_index.at( apred ) ) {
                    cerr << bt::format(
                        "%s: Multiple primary indices declared for '%s'.\n" )
                        % index_decl->loc
                        % apred;
                    exit( 1 );
                }
                env.primary_index.at( apred ) = pidx;
            }
            else {
                env.secondary_indices.at( apred ).emplace_back( pidx );
            }
        }


        // check for violations and create implicit indices
        if( pred_decl->builtin.get() == 1 ) {
            bt_unordered_map<rref_str, rref<builtin_pred_tc>> builtin_preds{
                { "equals", construct<builtin_pred_tc::equals>() },
                { "not_zero", construct<builtin_pred_tc::not_zero>() }
            };

            auto const bpred = find_default( builtin_preds, pred_decl->name );
            if( pred_decl->name == "print" ) {
                env.secondary_indices.at( apred ).push_back(
                    construct<predicate_index_tc>(
                        apred,
                        construct<index_info_tc::sink>( 1u ),
                        index_category::BUILTIN ) );
            }
            else if( bpred ) {
                env.primary_index.at( apred ) = construct<predicate_index_tc>(
                    apred,
                    construct<index_info_tc::builtin>( bpred ),
                    index_category::BUILTIN );
            }
            else {
                die();
            }
        }

        auto const primary = env.primary_index.at( apred );
        auto const& secondaries = env.secondary_indices.at( apred );
        if( !primary && secondaries.empty() 
            && !parent && g_opt.warning >= 1 ) 
        {
            cerr << bt::format(
                "Warning: No indices declared for '%s'.\n" )
                % apred;
        }
    }

    // check for underivable predicates
    for( auto const apred : env.all_apreds ) {
        if( apred->pred->decl->builtin.get() == 1 ) {
            continue;
        }

        auto const primary = env.primary_index.at( apred );
        auto const parent = env.parent_predicates.at( apred );
        if( parent && set_contains( env.defined, parent ) ) {
            env.defined.emplace( apred );
        }
        else if( primary && primary->idx_info.is<index_info_tc::source>() ) {
            env.defined.emplace( apred );
        }
        else if( !set_contains( env.defined, apred ) ) {
            env.underivable.emplace( apred );
            VERBOSE_PRINT( 1 )
                cerr << bt::format( 
                    "Warning: '%s' has no rules to derive it.\n" )
                    % apred;
        }
    }

    size_t underivable_sz;
    do {
        underivable_sz = env.underivable.size();
        for( auto const apred : env.all_apreds ) {
            bool apred_derivable = false;
            auto const primary = env.primary_index.at( apred );
            if( ( primary && primary->idx_info.is<index_info_tc::source>() )
                || env.parent_predicates.at( apred )
                || apred->pred->decl->builtin.get() == 1 ) 
            {
                continue;
            }
            for( auto const rule : env.rules.at( apred ) ) {
                bool rule_derivable = true;
                for( auto const lit : rule.body ) {
                    if( !lit->negated 
                        && set_contains( env.underivable, lit->apred ) ) 
                    {
                        VERBOSE_PRINT( 2 )
                            cout << "Skipping rule " << rule << endl;
                        rule_derivable = false;
                        break;
                    }
                }
                if( rule_derivable ) {
                    apred_derivable = true;
                    break;
                }
            }
            if( !apred_derivable ) {
                if( env.underivable.insert( apred ).second ) {
                    VERBOSE_PRINT( 1 )
                        cerr << bt::format( 
                            "Warning: '%s' is indirectly underivable.\n" )
                            % apred;
                }
            }
        }
    } while( underivable_sz != env.underivable.size() );
}

auto print_indices(
    rref<index_structure_tc> const x,
    unsigned const indent )
    -> void
{
    if( x->node.is<index_node_tc::root>() ) {
        cout << bt::format( "Root:\n" );
    }
    else {
        auto const types = x->node.is<index_node_tc::multi_map>()
            ? x->node.get<index_node_tc::multi_map>().types
            : rref_list<proper_type_tc>{ 
                x->node.get<index_node_tc::single_map>().type };
        cout << indenter{indent} << bt::format( 
            "Types: %s\n" )
            % types;
    }
    for( auto const child : x->children ) {
        auto const next_strc = find_default( env.index_structures, child );
        if( next_strc ) {
            print_indices( next_strc, indent + 1 );
        }
        else {
            if( child.is<index_node_tc::leaf>() ) {
                auto const& y = child.get<index_node_tc::leaf>();
                cout << indenter{indent+1} << bt::format( 
                    "predicate: %s index: %s\n" )
                    % y.apred
                    % y.decl;
            }
            else if( child.is<index_node_tc::term_set>() ) {
                auto const& y = child.get<index_node_tc::term_set>();
                cout << indenter{indent+1} << bt::format( 
                    "term set: %s\n" )
                    % y.pdconstr;
            }
            else if( child.is<index_node_tc::worklist>() ) {
                auto const& y = child.get<index_node_tc::worklist>();
                cout << indenter{indent+1} << bt::format( 
                    "worklist: %s\n" )
                    % y.apred;
            }
        }
    }
}

//*****************************************************************************
auto construct_demand_literal(
    rref<typed_literal_tc> const head )
    -> rref<typed_literal_tc>
{
    auto const dargs = filter_bound( head->args, head->apred->bp );
    auto const demand_apred = construct_demand_predicate( head->apred );
    my_assert( demand_apred->pred->types.size() == dargs.size() );
    return construct<typed_literal_tc>(
        head->loc,
        demand_apred,
        dargs,
        0 );
}

auto parse_predicate(
    rref<location_tc> const loc,
    string_ref const str )
    -> rref<predicate_decl_tc>
{
    auto const slash = str.find( '/' );
    if( slash == str.npos ) {
        rref<predicate_decl_tc> decl;
        for( auto const x : env.predicate_decls ) {
            if( x.second->name == str ) {
                if( decl ) {
                    cerr << bt::format( "%s: Ambiguous predicate '%s'\n" )
                        % loc
                        % str;
                    exit( 1 );
                }
                decl = x.second;
            }
        }
        if( !decl ) {
            cerr << bt::format( "%s: Undeclared predicate '%s'\n" )
                % loc
                % str;
            exit( 1 );
        }
        return decl;
    }
    else {
        auto const arity_str = str.substr( slash+1 );
        try {
            auto const arity = stoul( arity_str.to_string() );
            for( auto const x : env.predicate_decls ) {
                auto const decl = x.second;
                if( decl->name == str && decl->types.size() == arity )
                {
                    return x.second;
                }
            }
        } catch( invalid_argument const& ) {
            cerr << bt::format( "%s: Invalid predicate identifier '%s'\n" )
                % loc
                % str;
            exit( 1 );
        }
        cerr << bt::format( "%s: Unknown predicate '%s'\n" )
            % loc
            % str;
        exit( 1 );
    }
}
