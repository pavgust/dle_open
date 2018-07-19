#pragma once
typedef uint32_t object_t;
typedef uint32_t tagged_object_t;
typedef size_t table_idx_t;
typedef uint32_t dconstr_tag_t;

template< typename... Args >
struct argument_pack
{
    constexpr argument_pack() {};
};

template< typename... Args >
struct members_pack
{
    constexpr members_pack() {};
};

template< typename... Args >
struct dconstrs_pack
{
    constexpr dconstrs_pack() {};
};

template< unsigned I, typename T >
struct pack_at
{
};

template< unsigned I, template< typename... > class Pack, typename Arg, typename... Rest >
struct pack_at<I,Pack<Arg,Rest...>>
{
    using type = typename pack_at<I-1,Pack<Rest...>>::type;
};

template< template< typename... > class Pack, typename Arg, typename... Rest >
struct pack_at<0,Pack<Arg,Rest...>>
{
    using type = Arg;
};

template< typename... Args >
auto constexpr size_of_pack( argument_pack<Args...> const& ) -> size_t
{
    return sizeof...(Args);
}

template< typename... Args >
auto constexpr size_of_pack( members_pack<Args...> const& ) -> size_t
{
    return sizeof...(Args);
}

template< typename T >
auto constexpr pack_contains( 
    argument_pack<> const& )
    -> bool
{
    return false;
}

template< typename T, typename Arg, typename... Rest >
auto constexpr pack_contains( 
    argument_pack<Arg,Rest...> const& )
    -> bool
{
    return is_same<T,Arg>::value || pack_contains<T>( argument_pack<Rest...>() );
}

template< 
    template< typename... > class Ref, 
    typename Parent, 
    typename T, 
    Ref<T> Parent::*p >
struct member
{
    using parent_type = Parent;
    using value_type = T;
    using member_pointer_type = Ref<T> Parent::*;
    static auto constexpr pointer() -> member_pointer_type { return p; };
};

template< template< typename... > class Ref >
struct t_string
{
    auto static constexpr d_name() { return "string"; };
    using members_type = members_pack<>;
    
    static bool const is_single = false;
    using arguments_type = argument_pack<>;
    auto static constexpr t_name() { return "string"; };
    using dconstrs_type = dconstrs_pack<>;
};

template< template< typename... > class Ref >
struct t_i32
{
    auto static constexpr d_name() { return "i32"; };
    using members_type = members_pack<>;
    
    static bool const is_single = false;
    using arguments_type = argument_pack<>;
    auto static constexpr t_name() { return "i32"; };
    using dconstrs_type = dconstrs_pack<>;
};

template< template< typename... > class Ref, typename Ta >
struct t_list
{
    enum {NIL,CONS};
    struct nil
    {
        auto static constexpr d_name() -> char const* { return "nil"; }
        static unsigned const d_tag = NIL;
        using parent_type = t_list;
        using members_type = members_pack<>;
    };
    struct cons
    {
        Ref<Ta,void> head;
        Ref<t_list<Ref,Ta>,void> rest;
        auto static constexpr d_name() -> char const* { return "cons"; }
        static unsigned const d_tag = CONS;
        using parent_type = t_list;
        using members_type = members_pack<
            member<Ref,cons,Ta,&cons::head>,
            member<Ref,cons,t_list<Ref,Ta>,&cons::rest>>;
    };
    static bool const is_single = false;
    auto static constexpr t_name() -> char const* { return "list"; }
    using arguments_type = argument_pack<Ta>;
    using dconstrs_type = dconstrs_pack<nil,cons>;
};

template< typename Func, template< typename... > class Ref, typename Ta, typename... RefArgs >
auto inline type_switch( Func&& f, Ref<t_list<Ref,Ta>,RefArgs...> const& x )
{
    switch( x.get_tag() ) {
    case t_list<Ref,Ta>::NIL:
        return forward<Func>( f )( x.template get<typename t_list<Ref,Ta>::nil>() ); break; 
    case t_list<Ref,Ta>::CONS:
        return forward<Func>( f )( x.template get<typename t_list<Ref,Ta>::cons>() ); break; 
    default: abort();
    }
}
