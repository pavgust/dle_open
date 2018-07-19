#include "stdafx.h"
#include "utility.h"

#include "main.h"
#include "table.h"


//*****************************************************************************
template<
    typename Tc,
    template< typename... > class Func,
    typename Arg >
struct make_list
{};

template<
    typename Tc,
    template< typename... > class Func,
    template< typename... > class Pack >
struct make_list<Tc,Func,Pack<>>
{
    static auto ref() -> rref<list_tc<Tc>>
    {
        return nil<Tc>();
    }
};

template<
    typename Tc,
    template< typename... > class Func,
    template< typename... > class Pack,
    typename Arg,
    typename... Rest >
struct make_list<Tc,Func,Pack<Arg, Rest...>>
{
    static auto ref() -> rref<list_tc<Tc>>
    {
        return cons(
            Func<Arg>::ref(),
            make_list<Tc,Func,Pack<Rest...>>::ref() );
    }
};

template<
    typename Tc,
    template< typename... > class Func,
    typename Arg,
    typename... Rest >
struct make_list<Tc,Func,members_pack<Arg, Rest...>>
{
    static auto ref() -> rref<list_tc<Tc>>
    {
        return cons(
            Func<typename Arg::value_type>::ref(),
            make_list<Tc,Func,members_pack<Rest...>>::ref() );
    }
};

template< typename T >
struct string_constant_ref
{
    static auto ref() -> rref<string_tc>
    {
        return rref_str( T::t_name() );
    }
};

template< typename T >
struct atype_ref
{
    static auto ref() -> rref<type_tc>
    {
        return isupper( T::t_name()[0] )
            ? construct<type_tc::variable>( rref_str( T::t_name() ) )
            : construct<type_tc::atype>(
                rref_str( T::t_name() ),
                make_list<
                    type_tc,
                    ::atype_ref,
                    typename T::arguments_type>::ref() );
    }
};

template< typename T >
struct dconstr_ref
{
    static auto ref() -> rref<dconstr_tc>
    {
        return construct<dconstr_tc>(
            rref_str( T::d_name() ),
            make_list<type_tc,atype_ref,typename T::members_type>::ref() );
    }
};

template< typename T >
struct data_decl_ref
{
    static auto ref() -> rref<data_decl_tc>
    {
        return construct<data_decl_tc>(
            rref_str( T::t_name() ),
            make_list<
                string_tc,
                string_constant_ref,
                typename T::arguments_type
            >::ref(),
            make_list<
                dconstr_tc,
                dconstr_ref,
                typename T::dconstrs_type
            >::ref() );
    }
};

template< typename T >
struct proper_type_ref
{
    static auto ref() -> rref<proper_type_tc>
    {
        return construct<proper_type_tc>(
            rref_str( T::t_name() ),
            make_list<
                proper_type_tc,
                ::proper_type_ref,
                typename T::arguments_type>::ref() );
    }
};

template< typename T, typename Enable = void >
struct proper_dconstr_ref
{
    static auto ref() -> rref<proper_dconstr_tc>
    {
        return construct<proper_dconstr_tc>(
            proper_type_ref< typename T::parent_type >::ref(),
            rref_str( T::d_name() ),
            make_list<
                proper_type_tc,
                proper_type_ref,
                typename T::members_type
            >::ref() );
    }
};

template< typename T >
struct proper_dconstr_ref<T, typename enable_if<T::is_single>::type >
{
    static auto ref() -> rref<proper_dconstr_tc>
    {
        return construct<proper_dconstr_tc>(
            proper_type_ref<T>::ref(),
            rref_str( T::t_name() ),
            make_list<
                proper_type_tc,
                proper_type_ref,
                typename T::members_type
            >::ref() );
    }
};

//*****************************************************************************
// Flatten
template< typename T, typename Dc, typename... Accum >
auto assign_each_member(
    rref_list<typed_term_tc> const terms,
    members_pack<> const&,
    Accum&&... accum )
    -> rref<T>;

template<
    typename T,
    typename Dc,
    typename Arg,
    typename... Rest,
    typename... Accum >
auto assign_each_member(
    rref_list<typed_term_tc> const terms,
    members_pack<Arg,Rest...> const&,
    Accum&&... accum )
    -> rref<T>;

template< typename T >
struct flatten_term
{
    template< typename... Dcs >
    auto helper( 
        typed_term_tc::compound const& x, 
        dconstrs_pack<Dcs...> const& ) const
        -> rref<T>
    {
        rref<T> ret;
        int dummy[] = { 0,
            ( ( ret = x.dconstr == proper_dconstr_ref<Dcs>::ref() 
                ? assign_each_member<T,Dcs>( 
                    x.inner, 
                    typename Dcs::members_type() )
                : ret ), 0 )... };
        my_assert( ret );
        return ret;
    }

    auto operator()( rref<typed_term_tc> const t ) const -> rref<T>
    {
        auto const& x = t.get<typed_term_tc::compound>();
        return helper( x, typename T::dconstrs_type() );
    }
};

template<>
struct flatten_term<string_tc>
{
    auto operator()( rref<typed_term_tc> const t ) const -> rref<string_tc>
    {
        return t.get<typed_term_tc::string>().value;
    }
};

template<>
struct flatten_term<i32_tc>
{
    auto operator()( rref<typed_term_tc> const t ) const -> rref<i32_tc>
    {
        return t.get<typed_term_tc::i32>().value;
    }
};

template< typename T, typename Dc, typename... Accum >
auto assign_each_member(
    rref_list<typed_term_tc> const terms,
    members_pack<> const&,
    Accum&&... accum )
    -> rref<T>
{
    assert( terms.empty() );
    return construct<Dc>( forward<Accum>( accum )... );
}

template<
    typename T,
    typename Dc,
    typename Arg,
    typename... Rest,
    typename... Accum >
auto assign_each_member(
    rref_list<typed_term_tc> const terms,
    members_pack<Arg,Rest...> const&,
    Accum&&... accum )
    -> rref<T>
{
    return assign_each_member<T,Dc>(
        terms.rest(),
        members_pack<Rest...>(),
        forward<Accum>( accum )...,
        flatten_term<typename Arg::value_type>()( terms.head() ) );
}

//*****************************************************************************
auto get_predicate_decl(
    rref_str const name,
    rref<location_tc> const loc )
    -> rref<predicate_decl_tc>
{
    auto const decl = env.predicate_decls[name];
    if( !decl ) {
        cerr << bt::format(
            "%s: Use of undeclared predicate '%s'.\n" )
            % loc
            % name;
        exit( 1 );
    }
    return decl;
}

auto to_index_type()
    -> rref<proper_type_tc>
{
    // TODO...
    return construct<proper_type_tc>( "Set", nil<proper_type_tc>() );
}

auto to_atype(
    rref<proper_type_tc> const x )
    -> rref<type_tc>
{
    return construct<type_tc::atype>(
        x->name,
        transform_to_list( x->args, []( auto const y ) 
        { return to_atype( y ); } ) );
}

auto is_proper_type(
    rref<type_tc> const atype,
    variable_type_map const& tyvar_types )
    -> bool
{
    BEGIN_TYPE_SWITCH_( type_tc, atype )
    TYPE_CASE_X( variable )
        auto const iter = tyvar_types.find( x.name );
        return iter != tyvar_types.end() && iter->second;
    TYPE_CASE_X( atype )
        return ag::all_of(
            x.args,
            [&]( auto const y )
            { return is_proper_type( y, tyvar_types ); } );
    END_TYPE_SWITCH_
}

auto to_proper_type(
    rref<type_tc> const atype,
    variable_type_map const& tyvar_types,
    rref_str const name,
    rref<location_tc> const loc )
    -> rref<proper_type_tc> // may return null
{
    BEGIN_TYPE_SWITCH_( type_tc, atype )
    TYPE_CASE_X( variable )
        auto const iter = tyvar_types.find( x.name );
        auto const ret = iter != tyvar_types.end()
            ? iter->second 
            : rref<proper_type_tc>();
        if( !ret && name && loc ) {
            cerr << bt::format(
                "%s: Unable to infer type of type variable '%s'"
                " of '%s'.\n" )
                % loc
                % x.name
                % name;
            exit( 1 );
        }
        return ret;
    TYPE_CASE_X( atype )
        auto const pargs = transform_to_list(
            x.args,
            [&]( auto const y )
            { return to_proper_type( y, tyvar_types, name, loc ); } );
        if( !ag::all_of( pargs, identity_function{} ) ) {
            return rref<proper_type_tc>();
        }
        return construct<proper_type_tc>(
            x.name,
            pargs );
    END_TYPE_SWITCH_
}
auto to_proper_types(
    rref_list<type_tc> const x,
    variable_type_map const& tyvar_types,
    rref_str const name,
    rref<location_tc> const loc )
    -> rref_list<proper_type_tc> // may contain null
{
    return transform_to_list(
        x,
        [&]( rref<type_tc> const y )
        { return to_proper_type( y, tyvar_types, name, loc ); } );
}

auto get_proper_dconstr(
    rref_str const name,
    rref<proper_type_tc> const ptype,
    rref<location_tc> const loc )
    -> rref<proper_dconstr_tc>
{
    auto const data_decl = get_data_decl( loc, ptype->name );
    auto const dconstr = seq_find_default(
        data_decl.dconstrs,
        [&]( auto x ){ return x->name == name; } );
    if( !dconstr ) {
        cerr << bt::format( "%s: Type '%s' has no data constructor '%s'.\n" )
            % loc
            % ptype->name
            % name;
        exit( 1 );
    }

    auto const ptype_args_sz = ptype->args.length();
    if( ptype_args_sz != data_decl.tyvars.size() ) {
        cerr << bt::format(
            "%s: %d arguments provided to type constructor %s. "
            "Expected %d.\n" )
            % loc
            % ptype_args_sz
            % data_decl.name
            % data_decl.tyvars.size();
        exit( 1 );
    }

    variable_type_map tyvar_types;
    for( auto const x : zip( data_decl.tyvars, ptype->args ) ) {
        auto const tyvar = get<0>( x );
        auto const arg = get<1>( x );
        tyvar_types[get<0>( x )] = arg;
    }

    auto pargs = transform_to_list(
        dconstr->args,
        [&]( auto const x )
        { return to_proper_type( x, tyvar_types, name, loc ); } );

    return construct<proper_dconstr_tc>( ptype, name, pargs );
}

auto unify_type_variable(
    rref<proper_type_tc> const ptype, // may be null
    rref<type_tc> const atype,
    variable_type_map& tyvar_types,
    rref_str const name,
    rref<location_tc> const loc )
    -> bool
{
    if( !ptype ) {
        return true;
    }
    BEGIN_TYPE_SWITCH_( type_tc, atype )
    TYPE_CASE_X( variable )
        auto& tyvar_type = tyvar_types[x.name];
        if( tyvar_type && tyvar_type != ptype ) {
            if( name ) {
                cerr << bt::format(
                    "%s: Type '%s' of type variable '%s' "
                    "does not unify with type '%s' "
                    "inside '%s'.\n" )
                    % loc
                    % tyvar_type
                    % x.name
                    % ptype
                    % name;
                exit( 1 );
            }
            return false;
        }
        else if( !tyvar_type ) {

            if( !ptype->args.empty() && !ptype->args.head() ) {
                int x= 0;
            }
            tyvar_type = ptype;
        }
        return true;
    TYPE_CASE_X( atype )
        if( ptype->name != x.name ) {
            if( name ) {
                cerr << bt::format(
                    "%s(%s): Type constructor '%s' "
                    "does not unify '%s'.\n" )
                    % loc
                    % name
                    % x.name
                    % ptype->name;
                exit( 1 );
            }
            return false;
        }

        if( ptype->args.size() != x.args.size() ) {
            if( name ) {
                cerr << bt::format(
                    "%s(%s): %d arguments provided to "
                    "type constructor '%s'. "
                    "Expected %d.\n" )
                    % loc
                    % name
                    % x.args.size()
                    % ptype->name
                    % ptype->args.size();
                exit( 1 );
            }
            return false;
        }
        for( auto const& tpl : zip( ptype->args, x.args ) ) {
            if( !unify_type_variable( 
                get<0>( tpl ), 
                get<1>( tpl ), 
                tyvar_types,
                name, loc ) )
            {
                return false;
            }
        }
        return true;
    END_TYPE_SWITCH_
}

auto try_unify_type_variables(
    variable_type_map& tyvar_types,
    rref_list<proper_type_tc> const ptypes, // may contian nulls
    rref_list<type_tc> const types,
    rref_str const name,
    rref<location_tc> const loc )
    -> bool
{
    if( ptypes.size() != types.size() ) {
        my_assert( !name && !loc );
        return false;
    }

    for( auto const& tpl : zip( ptypes, types ) ) {
        if( !unify_type_variable( 
            get<0>( tpl ), get<1>( tpl ), tyvar_types, name, loc ) )
        {
            my_assert( !name && !loc );
            return false;
        }
    }
    return true;
}

auto unify_type_variables(
    variable_type_map& tyvar_types,
    rref_list<proper_type_tc> const ptypes, // may contain nulls
    rref_list<type_tc> const types,
    rref_str const name,
    rref<location_tc> const loc )
    -> void
{
    my_assert( name && loc );
    bool res = try_unify_type_variables( 
        tyvar_types, ptypes, types, name, loc );
    my_assert( res );
}

auto type_terms(
    rref_list<term_tc> const x,
    variable_type_map const& var_types,
    rref_list<proper_type_tc> const ptypes ) // may contain nulls
    -> rref_list<typed_term_tc>
{
    return transform_to_list(
        zip( x, ptypes ),
        [&]( auto const& tpl )
        { return type_term( get<0>( tpl ), var_types, get<1>( tpl ) ); } );
}

auto term_to_proper_type(
    rref<typed_term_tc> const t )
    -> rref<proper_type_tc>
{
    BEGIN_TYPE_SWITCH_( typed_term_tc, t )
    TYPE_CASE_X( i32 ) return env.i32_type;
    TYPE_CASE_X( string ) return env.string_type;
    TYPE_CASE_X( variable ) return x.type;
    TYPE_CASE_X( anonymous ) return rref<proper_type_tc>();
    TYPE_CASE_X( compound ) return x.dconstr->type;
    TYPE_CASE_X( function ) return x.func->result_type;
    TYPE_CASE_X( conditional ) return x.type;
    TYPE_CASE_X( aggregate ) return x.type;
    END_TYPE_SWITCH_
}

auto unify_literals_var_types( 
    rref_list<literal_tc> const body,
    rref_list<literal_tc> const heads,
    variable_type_map& var_types )
    -> void;

auto get_placeholder_list( unsigned i ) 
    -> rref_list<string_tc>
{
    auto l = nil<string_tc>();
    while( i > 0 ) {
        l.push_front( string_ref( '_' + to_string( i-- ) ) );
    }
    return l;
}

auto term_to_proper_type(
    rref<term_tc> const t, 
    variable_type_map const& var_types )
    -> rref<proper_type_tc> // may be null
{
    BEGIN_TYPE_SWITCH_( term_tc, t )
    TYPE_CASE_X( i32 ) 
        return env.i32_type;
    TYPE_CASE_X( string ) 
        return env.string_type;
    TYPE_CASE_X( variable ) 
        return find_default( var_types, x.name );
    TYPE_CASE_X( anonymous ) 
        return rref<proper_type_tc>();
    TYPE_CASE_X( compound ) 
        return rref<proper_type_tc>();
    TYPE_CASE_X( conditional )
        auto const ttype = term_to_proper_type( x.true_block, var_types );
        return ttype 
            ? ttype
            : term_to_proper_type( x.false_block, var_types );
    TYPE_CASE_X( aggregate )
        variable_type_map inner_var_types;
        auto const args = get_placeholder_list( x.input.size() );
        for( auto const& pr : zip( args, x.input ) ) {
            inner_var_types[get<0>(pr)] = term_to_proper_type(
                get<1>( pr ), var_types );
        }
        unify_literals_var_types( x.lits, nil<literal_tc>(), inner_var_types );
        return term_to_proper_type( x.output, inner_var_types );
    END_TYPE_SWITCH_
}

auto terms_to_proper_types(
    rref_list<typed_term_tc> const t )
    -> rref_list<proper_type_tc> // may contain null
{
    return transform_to_list(
        t,
        []( auto const y ) { return term_to_proper_type( y ); } );
}

auto terms_to_proper_types(
    rref_list<term_tc> const t, variable_type_map const& var_types )
    -> rref_list<proper_type_tc> // may contain null
{
    return transform_to_list(
        t,
        [&]( auto const y ) 
        { return term_to_proper_type( y, var_types ); } );
}

auto assert_typed( 
    rref<proper_type_tc> const x, 
    rref<location_tc> const loc )
{
    if( !x ) {
        cerr << bt::format(
            "%s: Unable to infer type.\n" )
            % loc;
        exit( 1 );
    }
}

auto assert_typed( 
    rref_list<proper_type_tc> const x,
    rref<location_tc> const loc )
{
    for( auto const y : x ) {
        assert_typed( y, loc );
    }
}

auto assert_type_expected(
    rref<proper_type_tc> const ptype,
    rref<proper_type_tc> const expected,
    rref<location_tc> const loc )
{
    if( expected && ptype != expected ) {
        cerr << bt::format(
            "%s: Inferred type '%s' does not match expected type '%s'.\n" )
            % loc
            % ptype
            % expected;
        exit( 1 );
    }
}

auto type_literal(
    rref<literal_tc> const lit,
    variable_type_map const& var_types )
    -> rref<typed_literal_tc>;

auto type_term(
    rref<term_tc> const term,
    variable_type_map const& var_types,
    rref<proper_type_tc> expected_ptype ) // may be null
    -> rref<typed_term_tc>
{
    BEGIN_TYPE_SWITCH_( term_tc, term )
    TYPE_CASE_X( i32 )
        assert_type_expected( env.i32_type, expected_ptype, x.loc );
        return construct<typed_term_tc::i32>(
            x.loc,
            x.value );
    TYPE_CASE_X( string )
        assert_type_expected( env.string_type, expected_ptype, x.loc );
        return construct<typed_term_tc::string>(
            x.loc,
            x.value );
    TYPE_CASE_X( variable )
        auto const vtype = find_default( var_types, x.name );
        if( !vtype ) {
            cerr << bt::format(
                "%s: Unable to infer type of variable '%s'.\n" )
                % x.loc
                % x.name;
            exit( 1 );
        }
        assert_type_expected( vtype, expected_ptype, x.loc );
        return construct<typed_term_tc::variable>( 
            x.loc, x.name, vtype );
    TYPE_CASE_X( anonymous )
        return construct<typed_term_tc::anonymous>( x.loc );
    TYPE_CASE_X( compound )
        auto const fdecl = find_default( 
            env.function_decls, 
            { x.name, (uint32_t)x.inner.size() } );
        if( x.explicit_type ) {
            expected_ptype = x.explicit_type;
        }
        if( fdecl ) {
            rref_list<proper_type_tc> expected_arg_types;
            if( expected_ptype ) {
                variable_type_map tyvar_types; 
                bool res = expected_ptype == env.void_type
                    || try_unify_type_variables(
                        tyvar_types,
                        { expected_ptype },
                        { fdecl->result_type } );
                if( res ) {
                    expected_arg_types = to_proper_types(
                        fdecl->arg_types,
                        tyvar_types );
                }
            }

            if( !expected_arg_types ) {
                expected_arg_types = construct_n( 
                    x.inner.size(), 
                    rref<proper_type_tc>() );
            }
            auto const arg_tterms 
                = type_terms( x.inner, var_types, expected_arg_types );
            auto const arg_ptypes = terms_to_proper_types( arg_tterms );
            assert_typed( arg_ptypes, x.loc );

            variable_type_map tyvar_types; 
            unify_type_variables( 
                tyvar_types,
                arg_ptypes, 
                fdecl->arg_types,
                x.name,
                x.loc );


            // check if arg_types unify
            auto result_ptype = to_proper_type( 
                fdecl->result_type, 
                tyvar_types,
                x.name,
                x.loc );
            if( expected_ptype != env.void_type ) {
                assert_type_expected( result_ptype, expected_ptype, x.loc );

            }
            return construct<typed_term_tc::function>(
                x.loc,
                construct<typed_function_tc>(
                    fdecl,
                    result_ptype,
                    arg_ptypes ),
                arg_tterms );
        }
        // data constructor
        auto const expected_arg_types
            = expected_ptype
            ? get_proper_dconstr( x.name, expected_ptype, x.loc )->args
            : construct_n( x.inner.size(), rref<proper_type_tc>() );

        if( x.inner.size() != expected_arg_types.size() ) {
            cerr << bt::format(
                "%s: %d arguments provided to data constructor %s "
                "of type '%s'. "
                "Expected %d.\n" )
                % x.loc
                % x.inner.size()
                % x.name
                % expected_ptype
                % expected_arg_types.size();
            exit( 1 );
        }

        auto const arg_tterms 
            = type_terms( x.inner, var_types, expected_arg_types );
        vector<rref<proper_dconstr_tc>> candidates;
        if( expected_ptype ) {
            candidates.push_back( 
                get_proper_dconstr( x.name, expected_ptype, x.loc ) );
        }
        else {
            auto const& tdconstrs = env.dconstrs[x.name];
            auto const arg_ptypes = terms_to_proper_types( arg_tterms );
            for( auto const tdconstr : tdconstrs ) {
                if( arg_tterms.size() != tdconstr.second->args.size() ) {
                    continue;
                }

                variable_type_map tyvar_types; 
                auto const res = try_unify_type_variables( 
                    tyvar_types,
                    arg_ptypes, 
                    tdconstr.second->args );
                if( res ) {
                    auto const ptype = to_proper_type( 
                        tdconstr.first,
                        tyvar_types );
                    if( ptype ) {
                        auto const pdconstr = get_proper_dconstr(
                            tdconstr.second->name,
                            ptype );
                        candidates.push_back( pdconstr );
                    }
                }
            }
        }

        if( candidates.empty() ) {
            cerr << bt::format( 
                "%s: '%s' is not recognized either as a function "
                "or a data constructor.\n" )
                % x.loc
                % x.name;
            exit( 1 );
        }
        else if( candidates.size() > 1 ) {
            cerr << bt::format( 
                "%s: Symbol '%s' is ambiguous: May be %s.\n" )
                % x.loc
                % x.name
                % delimited( candidates, " or " );
            exit( 1 );
        }

        auto const pdconstr = candidates.front();
        assert_type_expected( pdconstr->type, expected_ptype, x.loc );
        return construct<typed_term_tc::compound>( 
            x.loc, 
            pdconstr, 
            arg_tterms );
    TYPE_CASE_X( conditional )
        auto const cond_term = type_term( 
            x.cond, var_types, env.i32_type );
        auto const true_term = type_term( 
            x.true_block, var_types, expected_ptype );
        auto const false_term = x.false_block
            ? type_term( x.false_block, var_types, expected_ptype )
            : rref<typed_term_tc>();
        auto const true_type = term_to_proper_type( true_term );
        auto const false_type = term_to_proper_type( false_term );
        assert_typed( true_type, get_location( x.true_block ) ); 
        assert_typed( false_type, get_location( x.false_block ) );
        if( true_type != false_type ) {
            cerr << bt::format( "%s: true have false branches have "
                "differing types: '%s' and '%s'.\n" )
                % x.loc
                % true_type
                % false_type;
            die();
        }
        assert_type_expected( true_type, expected_ptype, x.loc );
        return construct<typed_term_tc::conditional>(
            x.loc,
            true_type,
            cond_term,
            true_term,
            false_term );
    TYPE_CASE_X( aggregate )
        auto const input = transform_to_list( x.input,
            [&]( auto const y ) { return type_term( y, var_types, {} ); } );
        variable_type_map inner_var_types;
        auto const args = get_placeholder_list( x.input.size() );
        for( auto const& pr : zip( args, x.input ) ) {
            inner_var_types[get<0>(pr)] = term_to_proper_type(
                get<1>( pr ), var_types );
        }
        unify_literals_var_types( x.lits, nil<literal_tc>(), inner_var_types );

        auto const lits = transform_to_list( x.lits,
            [&]( auto const lit ) {
                return type_literal( lit, inner_var_types );
            } );
        auto const output = type_term( x.output, inner_var_types, {} );
        auto const arg_types = transform_to_list(
            input,
            []( auto const y ) { return term_to_proper_type( y ); } );
        auto const decl = construct<predicate_decl_tc>(
            dummy_loc(),
            rref_str( ( bt::format( "@aggregate@%s" ) % x.loc ).str() ),
            transform_to_list( arg_types, []( auto const y ) 
                { return to_atype( y ); } ),
            true, false, false );
        auto const pred = construct<typed_predicate_tc>(
            decl, arg_types );
        auto const apred = construct<adorned_typed_predicate_tc>(
            pred, construct_n( arg_types.size(), b_free() ) );

        return construct<typed_term_tc::aggregate>(
            x.loc, term_to_proper_type( output ),
            apred, input,
            output, lits );
    END_TYPE_SWITCH_
}

auto get_predicate_types(
    rref<literal_tc> const lit )
    -> rref_list<type_tc>
{
    auto const decl = get_predicate_decl( lit->apred->name, lit->loc );
    auto const lit_args_sz = lit->args.length();
    auto types = decl->types;
    if( decl->variadic.get() == 1 ) {
        int diff = (int)lit_args_sz - types.size();
        auto tyvars = nil<type_tc>();
        while( diff-- > 0 ) {
            tyvars.push_front(
                construct<type_tc::variable>( "T" + to_string( diff ) ) );
        }
        types = concat( types, tyvars );
    }

    if( lit_args_sz != types.size() ) {
        cerr << bt::format(
            "%s: %d arguments provided to predicate %s. "
            "Expected %d.\n" )
            % lit->loc
            % lit_args_sz
            % decl->name.str()
            % types.size();
        exit( 1 );
    }
    return types;
}

auto type_literal(
    rref<literal_tc> const lit,
    variable_type_map const& var_types )
    -> rref<typed_literal_tc>
{
    auto const decl = get_predicate_decl( lit->apred->name, lit->loc );
    auto const types = get_predicate_types( lit );
    
    variable_type_map tyvar_types;
    try_unify_type_variables( 
        tyvar_types,
        terms_to_proper_types( lit->args, var_types ),
        types );
    auto const expected_ptypes = to_proper_types( 
        types,
        tyvar_types );

    auto const tterms = type_terms( lit->args, var_types, expected_ptypes );
    variable_type_map tyvar_types2;
    unify_type_variables(
        tyvar_types2,
        terms_to_proper_types( tterms ),
        types,
        decl->name,
        lit->loc );
    rref_list<proper_type_tc> ptypes = to_proper_types(
        types,
        tyvar_types2,
        decl->name,
        lit->loc );
    auto const tpred = construct<typed_predicate_tc>( decl, ptypes );
    auto const tapred = construct<adorned_typed_predicate_tc>(
        tpred,
        lit->apred->bp );
    return construct<typed_literal_tc>( 
        lit->loc, tapred, tterms, lit->negated );
}

auto unify_literal_var_types(
    rref<literal_tc> const lit,
    variable_type_map& var_types )
    -> void;

auto unify_term_var_types(
    rref<term_tc> const t,
    rref<proper_type_tc> const ptype, // may be null
    variable_type_map& var_types )
    -> void
{
    BEGIN_TYPE_SWITCH_( term_tc, t )
        TYPE_CASE_X( i32 )
        TYPE_CASE_X( string )
        TYPE_CASE_X( variable )
        if( ptype ) {
            var_types[x.name] = ptype;
        }
        else if( x.explicit_type ) {
            var_types[x.name] = x.explicit_type;
        }
        TYPE_CASE_X( anonymous )
        TYPE_CASE_X( compound )
        auto const fdecl = find_default( 
            env.function_decls, { x.name, (uint32_t)x.inner.size() } );
        if( fdecl ) {
            // nothing
        }
        else if( ptype ){
            auto const pdconstr = get_proper_dconstr(
                x.name,
                ptype,
                x.loc );
            if( x.inner.size() == pdconstr->args.size() ) {
                for( auto const& tpl : zip( x.inner, pdconstr->args ) ) {
                    unify_term_var_types( 
                        get<0>( tpl ), 
                        get<1>( tpl ),
                        var_types );
                }
            }
        }
        TYPE_CASE_X( conditional )
        unify_term_var_types( x.true_block, ptype, var_types );
        if( x.false_block ) {
            unify_term_var_types( x.false_block, ptype, var_types );
        }
        TYPE_CASE_X( aggregate )
        // nothing
    END_TYPE_SWITCH_
}

auto unify_literal_var_types(
    rref<literal_tc> const lit,
    variable_type_map& var_types )
    -> void
{
    auto const types = get_predicate_types( lit );
    
    variable_type_map tyvar_types; 
    auto const res = try_unify_type_variables(
        tyvar_types,
        terms_to_proper_types( lit->args, var_types ),
        types );

    if( res ) {
        auto const ptypes = to_proper_types( types, tyvar_types );
        for( auto const& tpl : zip( lit->args, ptypes ) ) {
            unify_term_var_types( 
                get<0>( tpl ),
                get<1>( tpl ),
                var_types );
        }
    }
}

auto process_predicate_decl(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<type_tc> const types,
    rref_list<string_tc> const specifiers,
    bool const variadic,
    bool const builtin )
    -> void
{
    if( env.predicate_decls[name] ) {
        cerr << bt::format( "%s: Duplicate declaration of predicate '%s'.\n" )
            % loc
            % name.str();
        exit( 1 );
    }

    bool direct = false;
    bool nonrec = false;
    bool inline_predicate = false;
    for( auto const spec : specifiers ) {
        if( spec == "direct" ) {
            direct = true;
        }
        else if( spec == "nonrec" ) {
            nonrec = true;
        }
        else if( spec == "inline" ) {
            inline_predicate = true;
        }
        else {
            cerr << bt::format( "%s: Unknown specifier '%s'.\n" )
                % loc
                % spec.str();
            exit( 1 );
        }
    }

    auto decl = env.predicate_decls[name] = construct<predicate_decl_tc>(
        loc,
        name,
        types,
        nonrec ? wl_category::NON_RECURS 
            : direct ? wl_category::DIRECT 
            : wl_category::NORMAL,
        variadic ? 1 : 0,
        builtin ? 1 : 0 );

    if( inline_predicate ) {
        env.inline_predicates.insert( decl );
    }
}

auto process_function_declaration(
    rref<location_tc> const loc,
    rref_str const name,
    rref<type_tc> const result_type,
    rref_list<type_tc> const arg_types,
    rref_str const overrides )
    -> rref<function_decl_tc>;
auto process_function_definition(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<term_tc> const args,
    rref<term_tc> const body )
    -> void;

auto create_show(
    rref<term_tc> const arg )
    -> rref<term_tc>
{
    return construct<term_tc::compound>(
        dummy_loc(),
        "show",
        rref<proper_type_tc>(),
        rref_list<term_tc>{ arg } );
}

auto create_show(
    string_ref const str )
    -> rref<term_tc>
{
    return create_show( construct<term_tc::string>( dummy_loc(), str ) );
}

auto create_implicit_print(
    rref<type_tc> const type,
    rref_list<dconstr_tc> const dconstrs )
    -> void
{
    auto const decl = process_function_declaration(
        dummy_loc(),
        "show$" + to_string( type.get<type_tc::atype>().name.str() ), 
        to_atype( env.void_type ), 
        { type },
        "show" );

    for( auto const dconstr : dconstrs ) {
        rref_vector<term_tc> arg_vars;
        rref_vector<term_tc> seq{
            create_show( to_string( dconstr->name.str() ) + "(" ) };
        for( auto const arg : dconstr->args | ad::indexed() ) {
            if( arg.index() > 0 ) {
                seq.push_back( create_show( "," ) );
            }
            auto const arg_var = construct<term_tc::variable>(
                dummy_loc(), 
                "V" + to_string( arg.index() ),
                rref<proper_type_tc>() );
            arg_vars.push_back( arg_var );
            seq.push_back( create_show( arg_var ) );
        }
        seq.push_back( create_show( ")" ) );

        auto const arg_term = construct<term_tc::compound>(
            dummy_loc(),
            dconstr->name,
            rref<proper_type_tc>(),
            concat( arg_vars ) );
        auto const body_term = foldr(
            seq,
            []( rref<term_tc> const t, rref<term_tc> const tseq )
            {
                return tseq 
                    ? construct<term_tc::compound>(
                        dummy_loc(),
                        "seq", rref<proper_type_tc>(),
                        rref_list<term_tc>{ t, tseq } )
                    : t;
            },
            rref<term_tc>() 
        );
        process_function_definition(
            dummy_loc(),
            decl->name,
            { arg_term },
            body_term );
    }
}

auto process_data_decl(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<string_tc> const tyvars,
    rref_list<dconstr_tc> const dconstrs )
    -> void
{
    auto& decl = env.data_decls[name];
    if( decl.declared ) {
        cerr << bt::format( "%s: Duplicate declaration of data type '%s'.\n" )
            % loc
            % name.str();
        exit( 1 );
    }
    decl.name = name;
    for( auto const tyvar : tyvars ) {
        decl.tyvars.push_back( tyvar );
    }

    auto const type = construct<type_tc::atype>(
        name,
        transform_to_list( 
            tyvars, 
            []( auto const y )
            { return construct<type_tc::variable>( y ); } ) );

    dconstr_tag_t tag = 0;
    for( auto const dconstr : dconstrs ) {
        auto const tdconstr = construct<typed_dconstr_tc>(
            name,
            dconstr->name );
        decl.dconstrs.push_back( dconstr );
        env.dconstr_tag.at( tdconstr ) = tag++;
        env.dconstrs[dconstr->name].emplace_back( type, dconstr );
    }
    decl.declared = true;

    if( map_contains( env.function_decls, { "show", 1 } ) ) {
        create_implicit_print( type, dconstrs );
    }
}

auto process_data_decl(
    rref<data_decl_tc> const decl )
    -> void
{
    return process_data_decl(
        dummy_loc(),
        decl->name,
        decl->tyvars,
        decl->dconstrs );
}

auto get_function_declaration(
    rref<location_tc> const loc,
    rref_str const name,
    unsigned const arity )
    -> rref<function_decl_tc>
{
    auto const decl = find_default( env.function_decls, { name, arity } );
    if( !decl ) {
        cerr << bt::format( 
            "%s: Function '%s' must be declared before being defined.\n" )
            % loc
            % name;
        exit( 1 );
    }
    return decl;
}

auto process_function_declaration(
    rref<location_tc> const loc,
    rref_str const name,
    rref<type_tc> const result_type,
    rref_list<type_tc> const arg_types,
    rref_str const overrides )
    -> rref<function_decl_tc>
{
    auto const arity = arg_types.size();
    auto& decl = env.function_decls[{ name, (uint32_t)arity }];
    if( decl ) {
        cerr << bt::format( "%s: Duplicate declaration of function '%s'.\n" )
            % loc
            % name;
        exit( 1 );
    }

    decl = construct<function_decl_tc>(
        loc,
        name,
        result_type,
        arg_types );
    if( overrides ) {
        auto const odecl = get_function_declaration( loc, overrides, arity );
        env.function_overrides[odecl].push_back( decl );
    }
    return decl;
}

auto process_function_definition(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<term_tc> const args,
    rref<term_tc> const body )
    -> void
{
    auto const decl = get_function_declaration( loc, name, args.size() );

    auto& defns = env.function_defns[name];
    defns.push_back( 
        construct<function_defn_tc>(
            loc,
            decl,
            args,
            body ) );
}

auto instantiate_function(
    rref<typed_function_tc> const tfunc,
    rref<function_defn_tc> const fdefn )
    -> pair<rref_list<typed_term_tc>,rref<typed_term_tc>>
{
    variable_type_map var_types;
    for( auto const x : zip( fdefn->args, tfunc->arg_types ) ) {
        unify_term_var_types( get<0>( x ), get<1>( x ), var_types );
    }

    auto const targs = transform_to_list(
        zip( fdefn->args, tfunc->arg_types ),
        [&]( auto const& x )
        { return type_term( get<0>( x ), var_types, get<1>( x ) ); } );
    return make_pair(
        targs,
        type_term( fdefn->body, var_types, tfunc->result_type ) );
}

auto process_index_decl(
    rref<location_tc> const loc,
    rref_str const name,
    rref_list<binding_tc> bp,
    rref<term_tc> const term,
    rref_list<string_tc> const specifiers )
    -> void
{
    auto const pred_decl = get_predicate_decl( name, loc );
    if( !bp ) {
        bp = construct_n( pred_decl->types.size(), b_free() );
    }

    variable_type_map vmap;
    auto const tterm = type_term(
        term,
        vmap,
        proper_type_ref<index_descriptor_tc>::ref() );
    auto const descr = flatten_term<index_descriptor_tc>()( tterm );
    
    // parse specifiers
    bool demand = false;
    uint32_t category = index_category::PRIMARY;
    for( auto const spec : specifiers ) {
        if( spec == "secondary" ) {
            category = index_category::INDEX;
        }
        else if( spec == "demand" ) {
            demand = true;
        }
        else {
            cerr << bt::format( "%s: Unknown specifier '%s'.\n" )
                % loc
                % spec.str();
            exit( 1 );
        }
    }
    if( descr.is<index_descriptor_tc::source>()
        && category != index_category::PRIMARY )
    {
        cerr << bt::format( "%s: Source index must be a primary index.\n" )
            % loc;
        exit( 1 );
    }
    if( descr.is<index_descriptor_tc::sink>() ) {
        category = index_category::INDEX;
    }

    auto const pred = construct<adorned_predicate_tc>( name, bp ); 
    auto const decl = construct<index_decl_tc>(
        loc,
        name,
        bp,
        descr,
        category );
    ( !demand 
        ? env.index_decls 
        : env.demand_index_decls ).at( pred ).push_back( decl );
}

auto get_data_decl(
    rref<location_tc> const loc,
    rref_str const name )
    -> data_decl&
{
    auto& decl = env.data_decls[name];
    if( !decl.declared ) {
        cerr << bt::format(
            "%s: Use of undeclared data type '%s'.\n" )
            % loc
            % name;
        exit( 1 );
    }
    return decl;
}

auto check_adornment(
    rref<literal_tc> const lit )
{
    if( lit->apred->bp.size() != lit->args.size() ) {
        cerr << bt::format(
            "%s: Arity mismatch of binding pattern '%s' for "
            "predicate '%s'.\n" )
            % lit->loc
            % lit->apred->bp
            % lit->apred->name;
        exit( 1 );
    }
}

auto command_expects(
    rref<location_tc> const loc,
    string_ref const name,
    rref<literal_tc> const lit,
    rref_list<proper_type_tc> types )
    -> void
{
    if( lit->args.size() != types.size() ) {
        cerr << bt::format(
            "%s: %d arguments provided to command %s. "
            "Expected %d.\n" )
            % loc
            % lit->args.size()
            % name
            %  types.size();
        exit( 1 );
    }

    int arg_no = 1;
    for( auto const& pr : zip( lit->args, types ) ) {
        auto const ptype = term_to_proper_type( 
            get<0>( pr ), variable_type_map{} );
        if( !ptype || ptype != get<1>( pr ) ) {
            cerr << bt::format(
                "%s: argument %d of command %s "
                "is expected to be of type %s.\n" )
                % loc
                % arg_no
                % name
                % get<1>( pr );
            exit( 1 );
        }
        arg_no +=1;
    }
}

auto unify_literals_var_types( 
    rref_list<literal_tc> const body,
    rref_list<literal_tc> const heads,
    variable_type_map& var_types )
    -> void
{
    size_t var_types_sz;
    do {
        var_types_sz = var_types.size();
        for( auto const lit : body ) {
            unify_literal_var_types( lit, var_types );
        }
        for( auto const head : heads ) {
            auto const name = head->apred->name.str();
            if( name != "eval_before" && name != "eval_after" ) {
                unify_literal_var_types( head, var_types );
            }
        }
    } while( var_types.size() != var_types_sz );
}

auto to_exhaustive_predicate( rref<predicate_decl_tc> const decl )
{
    return construct<adorned_typed_predicate_tc>(
        construct<typed_predicate_tc>( 
            decl, 
            to_proper_types( decl->types, {}, decl->name, decl->loc ) ),
        construct_n( decl->types.size(), b_free() ) );
}

auto process_rule(
    rref<location_tc> const loc,
    rref_list<literal_tc> const heads,
    rref_list<literal_tc> const body,
    bool inline_rule )
    -> void
{
    for( auto const lit : body ) {
        check_adornment( lit );
    }
    for( auto const lit : heads ) {
        check_adornment( lit );
    }

    variable_type_map var_types;
    unify_literals_var_types( body, heads, var_types );

    vector<rref<typed_literal_tc>> tbody;
    for( auto const lit : body ) {
        tbody.push_back( type_literal( lit, var_types ) );
		if( set_contains( env.preferred_literals, lit ) ) {
			env.preferred_typed_literals.emplace( tbody.back() );
		}
    }

    for( auto const head : heads ) {
        auto const name = head->apred->name.str();
        if( name == "eval_before" ) {
            command_expects( loc, "eval_before", head, { env.string_type } );
            auto const pred = parse_predicate( 
                loc, head->args[0].get<term_tc::string>().value.str() );
            auto const apred = to_exhaustive_predicate( pred );
            env.eval_before[apred].push_back( tbody );
        }
        else if( name == "eval_after" ) {
            command_expects( loc, "eval_after", head, { env.string_type } );
            auto const pred = parse_predicate( 
                loc, head->args[0].get<term_tc::string>().value.str() );
            auto const apred = to_exhaustive_predicate( pred );
            env.eval_after[apred].push_back( tbody );
        }
        else {
            auto const thead = type_literal( head, var_types );
            auto const apred = thead->apred;

            if( set_contains( env.inline_predicates, apred->pred->decl )
                && !all_free( apred->bp ) 
                && body.empty() )
            {
                inline_rule = true;
            }

            if( !inline_rule ) {
                env.rules.at( apred ).push_back( { loc, thead, tbody } );
            }
            else {
                env.inline_rules[apred].push_back( { loc, thead, tbody } );
            }
        }
    }
}

auto process_fact(
    rref<location_tc> const loc,
    rref<literal_tc> const lit )
    -> void
{
    auto const name = lit->apred->name.str();
    if( name == "include" ) {
        command_expects( loc, "include", lit, { env.string_type } );
        read_file( 
            lit->args[0].get<term_tc::string>().value.to_string(), true );
    }
    else if( name == "trace" ) {
        command_expects( loc, "trace", lit, { env.string_type } );
        auto const pred = parse_predicate( 
            loc, lit->args[0].get<term_tc::string>().value.str() );
        env.traced_preds.insert( pred );
    } 
    else if( name == "print_scc" ) {
        command_expects( loc, "print_scc", lit, { env.string_type } );
        auto const pred = parse_predicate( 
            loc, lit->args[0].get<term_tc::string>().value.str() );
        env.print_scc_preds.insert( pred );
    }
    else {
        process_rule( loc, cons( lit ), nil<literal_tc>(), false );
    }
}

auto declare_builtin_op(
    rref_str const name,
    rref<type_tc> const return_type,
    rref_vector<type_tc> const arg_types )
{
    env.function_decls.insert( {
        { name, (uint32_t)arg_types.size() },
        construct<function_decl_tc>(
            dummy_loc(),
            name,
            return_type,
            concat( arg_types ) ) } );
}

struct dummy_atype_a
{
    static constexpr auto t_name() -> char const* { return "A"; }
    using arguments_type = argument_pack<>;
};

void init_types()
{
    env.i32_type = construct<proper_type_tc>(
        rref_str( "i32" ),
        nil<proper_type_tc>() );
    env.string_type = construct<proper_type_tc>(
        rref_str( "string" ),
        nil<proper_type_tc>() );
    env.void_type = construct<proper_type_tc>(
        rref_str( "void" ),
        nil<proper_type_tc>() );

    auto i32_type = to_atype( env.i32_type );
    auto string_type = to_atype( env.string_type );
    auto void_type = to_atype( env.void_type );
    auto const type_var_t = construct<type_tc::variable>( "T" );
    auto const type_var_t1 = construct<type_tc::variable>( "T1" );

    auto const two_i32_types 
        = rref_vector<type_tc>{ i32_type, i32_type };
    auto const two_string_types 
        = rref_vector<type_tc>{ string_type, string_type };
    auto const two_t_types 
        = rref_vector<type_tc>{ type_var_t, type_var_t };

    declare_builtin_op( "builtin_show_i32",  void_type, { i32_type } );
    declare_builtin_op( "builtin_show_string", void_type, { string_type } );

    rref_vector<type_tc> fprintf_types{ i32_type, string_type };
    rref_vector<type_tc> printf_types{ string_type };
    for( unsigned i = 0; i < 10; ++i ) {
        declare_builtin_op( "fprintf", 
            void_type, fprintf_types );
        declare_builtin_op( "printf", 
            void_type, printf_types );
        auto const type_var = construct<type_tc::variable>( 
            "T" + to_string( i+1 ) );
        printf_types.push_back( type_var );
        fprintf_types.push_back( type_var );
    }

    declare_builtin_op( "reset_timer", void_type, {} );
    declare_builtin_op( "print_timer", void_type, {} );

    declare_builtin_op( "multiply", i32_type, two_i32_types );
    declare_builtin_op( "divide", i32_type, two_i32_types );
    declare_builtin_op( "add", i32_type, two_i32_types );
    declare_builtin_op( "sub", i32_type, two_i32_types );
    declare_builtin_op( "less_than", i32_type, two_i32_types );
    declare_builtin_op( "less_than_eq", i32_type, two_i32_types );
    declare_builtin_op( "equals", i32_type, two_t_types );
    declare_builtin_op( "nequals", i32_type, two_t_types );
    declare_builtin_op( "bitwise_and", i32_type, two_i32_types );
    declare_builtin_op( "bitwise_xor", i32_type, two_i32_types );
    declare_builtin_op( "bitwise_or", i32_type, two_i32_types );
    declare_builtin_op( "and", i32_type, two_i32_types );
    declare_builtin_op( "or", i32_type, two_i32_types );
    declare_builtin_op( "seq", type_var_t, { type_var_t1, type_var_t } );
    declare_builtin_op( "void", void_type, {} );
    declare_builtin_op( "cat", string_type, two_string_types );
    declare_builtin_op( "match", i32_type, two_string_types );

    auto const dloc = dummy_loc();
    process_data_decl(
        dloc, "i32", nil<string_tc>(), nil<dconstr_tc>() );
    process_data_decl(
        dloc, "string", nil<string_tc>(), nil<dconstr_tc>() );

    process_data_decl( data_decl_ref<list_tc<dummy_atype_a>>::ref() );
    process_data_decl( data_decl_ref<index_descriptor_tc>::ref() );

    process_predicate_decl(
        dloc, "print",
        nil<type_tc>(),
        nil<string_tc>(), true, true );
    process_predicate_decl(
        dloc, "equals",
        concat( two_t_types ),
        nil<string_tc>(), false, true );
    process_predicate_decl(
        dloc, "not_zero",
        { i32_type },
        nil<string_tc>(), false, true );
}
