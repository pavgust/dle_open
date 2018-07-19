#pragma once
#include "rootref.h"
#include "gen/datatypes.hh"

#ifndef RREF_INST
#define RREF_INST( type )
#endif

#define RREF_TYPE( arg ) using arg ## _tc = t_ ## arg<root_reference>; \
    RREF_INST( t_ ## arg<root_reference> );
RREF_TYPE(dconstr)
RREF_TYPE(typed_predicate)
RREF_TYPE(index_descriptor)
RREF_TYPE(type)
RREF_TYPE(data_decl)
RREF_TYPE(predicate_decl)
RREF_TYPE(index_decl)
RREF_TYPE(proper_type)
RREF_TYPE(proper_dconstr)
RREF_TYPE(binding)
RREF_TYPE(location)
RREF_TYPE(term)
RREF_TYPE(literal)
RREF_TYPE(typed_term)
RREF_TYPE(typed_literal)
RREF_TYPE(typed_dconstr)
RREF_TYPE(interned_string)
RREF_TYPE(index_node)
RREF_TYPE(index_structure)
RREF_TYPE(index_info)
RREF_TYPE(predicate_index)
RREF_TYPE(adorned_predicate)
RREF_TYPE(adorned_typed_predicate)
RREF_TYPE(typed_function)
RREF_TYPE(builtin_pred)
RREF_TYPE(function_decl)
RREF_TYPE(function_defn)

RREF_TYPE(mtype)
RREF_TYPE(nliteral)
RREF_TYPE(nterm)
RREF_TYPE(ptype)
