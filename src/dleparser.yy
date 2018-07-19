%pure-parser
%defines
%locations
%error-verbose
%name-prefix "dle"
%debug

%parse-param { yyscan_t yyscanner }
%lex-param   { yyscan_t yyscanner }

%{
#include "dleparser.h"
%}

%token TOK_EOF 0
%token TOK_BEGIN_DIRECTIVE
%token TOK_END_DIRECTIVE
%token TOK_SYMBOL
%token TOK_MACRO
%token TOK_NUMERIC
%token TOK_INTEGER
%token TOK_VARIABLE
%token TOK_TURNSTILE
%token TOK_EQTURNSTILE
%token TOK_ELLIPSIS
%token TOK_DATA
%token TOK_PREDICATE
%token TOK_INDEX
%token TOK_SUBINDEX
%token TOK_COLON_COLON
%token TOK_DUMMY
%token TOK_EQUAL_EQUAL
%token TOK_NEQUAL
%token TOK_AND
%token TOK_OR
%token TOK_LT_EQ
%token TOK_GT_EQ
%token TOK_WEAK_NEGATION
%token TOK_FUNCTION
%token TOK_ARROW
%token TOK_DOUBLE_ARROW
%token TOK_CASE
%token TOK_OF
%token TOK_IF
%token TOK_WHERE
%token TOK_OVERRIDES

// Lowest
%left ';'
%right TOK_THEN TOK_ELSE
%left TOK_OR
%left TOK_AND
%left '|'
%left '^'
%left '&'
%left TOK_EQUAL_EQUAL TOK_NEQUAL
%left '<' TOK_LT_EQ '>' TOK_GT_EQ
%left '+' '-'
%left '*' '/'
%right ':'
%precedence UNARY_NOT
// Highest


%union
{
    urref<string_tc> str;
    urref<list_tc<string_tc>> strs;
    urref<list_tc<list_tc<string_tc>>> strss;
    urref<term_tc> t;
    urref<list_tc<term_tc>> ts;
    urref<literal_tc> lit;
    urref<list_tc<literal_tc>> lits;
    urref<proper_type_tc> ptype;
    urref<list_tc<proper_type_tc>> ptypes;
    urref<type_tc> type;
    urref<list_tc<type_tc>> types;
    urref<dconstr_tc> dconstr;
    urref<list_tc<dconstr_tc>> dconstrs;
    urref<list_tc<binding_tc>> bp;
    urref<list_tc<list_tc<binding_tc>>> bps;

    double dnum;
    int inum;
    bool b;
}

%type <str>
    TOK_SYMBOL
    TOK_MACRO
    TOK_VARIABLE
    overrides_specifier_opt

%type <inum>
    TOK_INTEGER
	integer_seq
	integer_seq_opt
//    integer_exp
//    integer_term
//    integer_unary

%type <b>
	preferred_literal

%type <t>
    term
    primary_term
    compound_term
    compound_term_explicit
    array_term_list
    array_term_list_opt

%type <ts>
    term_list
    term_list_opt
 
%type <lit>
    literal

%type <lits>
    literal_list
  
%type <strs>
    symbol_seq
    symbol_seq_opt
    variable_seq
    variable_seq_opt

%type <ptype>
    proper_atype
    proper_btype

%type <ptypes>
    proper_atype_seq
    proper_atype_seq_opt
//    proper_btype_list
//    proper_btype_list_opt

%type <type>
    btype
    type
    record_specifier

%type <types>
    atype_seq
    atype_seq_opt
    btype_list
    btype_list_opt
    record_specifier_list

%type <dconstr>
    dconstr

%type <dconstrs>
    dconstr_list

%type <bp>
    binding_pattern_specifier
    binding_pattern_specifier_opt

%type <bps>
    binding_pattern_specifier_seq
    binding_pattern_specifier_seq_opt

%%

file
    : sentence_seq
    | %empty
    ;

sentence_seq
    : sentence
    | sentence_seq sentence
    ;

sentence
    : literal_list[lhs] TOK_TURNSTILE literal_list[rhs] '.'
        {
            process_rule( construct( @$ ), $lhs.ref(), $rhs.ref() );
        }
    | literal_list[lhs] TOK_EQTURNSTILE literal_list[rhs] '.'
        {
            process_rule( construct( @$ ), $lhs.ref(), $rhs.ref(), true );
        }
    | literal '.'
        {
            process_fact( construct( @$ ), $literal.ref() );
        }
    | TOK_DATA TOK_SYMBOL variable_seq_opt '=' dconstr_list '.'
        {
            process_data_decl(
                construct( @$ ),
                $TOK_SYMBOL.ref(),
                $variable_seq_opt.ref(),
                $dconstr_list.ref() );
        }
    | TOK_PREDICATE TOK_SYMBOL '(' btype_list_opt ')' symbol_seq_opt '.'
        {
            process_predicate_decl( 
                construct( @$ ),
                $TOK_SYMBOL.ref(),
                $btype_list_opt.ref(),
                $symbol_seq_opt.ref() );
        }
    | TOK_INDEX TOK_SYMBOL binding_pattern_specifier_opt compound_term symbol_seq_opt '.'
        {
            process_index_decl(
                construct( @$ ),
                $TOK_SYMBOL.ref(),
                $binding_pattern_specifier_opt.ref(),
                $compound_term.ref(),
                $symbol_seq_opt.ref() );
        }
    | TOK_FUNCTION TOK_SYMBOL overrides_specifier_opt 
        TOK_COLON_COLON btype_list_opt TOK_ARROW btype '.'
        {
            process_function_declaration(
                construct( @$ ),
                $TOK_SYMBOL.ref(),
                $btype.ref(),
                $btype_list_opt.ref(),
                $overrides_specifier_opt.ref() );
        }
    | TOK_SYMBOL preferred_literal '(' term_list_opt ')' '=' term '.'
        {
            process_function_definition(
                construct( @$ ),
                $TOK_SYMBOL.ref(),
                $term_list_opt.ref(),
                $term.ref() );
        }
    | TOK_DUMMY binding_pattern_specifier_seq_opt 'a'
    | TOK_DUMMY proper_atype_seq_opt 'b'
	| TOK_BEGIN_DIRECTIVE TOK_INTEGER TOK_SYMBOL integer_seq_opt TOK_END_DIRECTIVE
		{
			yylloc.first_line = yylloc.last_line = $2;
			yylloc.first_column = yylloc.last_column = 1;
			switch( $integer_seq_opt ) {
			case 1: 
				ienv.paths.push_back( fs::path( string( $TOK_SYMBOL.ref().str() ) ) ); 
				break;
			case 2: 
				my_assert( !ienv.paths.empty() );
				ienv.paths.pop_back(); 
				break;
			}
		}
    ;

integer_seq
	: TOK_INTEGER
		{ $$ = $1; }		
	| integer_seq TOK_INTEGER
		{ $$ = $1; }
	;

integer_seq_opt
	: %empty
		{ $$ = 0; }
	| integer_seq
		{ $$ = $1; }
	;

overrides_specifier_opt
    : %empty
        { $$ = rref_str{}; }
    | TOK_OVERRIDES TOK_SYMBOL
        { $$ = $2; }
    ;
    

binding_pattern_specifier_seq_opt
    : %empty
        { $$ = nil<list_tc<binding_tc>>(); }
    | binding_pattern_specifier_seq
        { $$ = $1; }
    ;

binding_pattern_specifier_seq
    : '#' TOK_SYMBOL
        { $$ = cons( construct_bp( $TOK_SYMBOL.ref(), construct( @$ ) ) ); }
    | '#' TOK_SYMBOL binding_pattern_specifier_seq
        { 
            $$ = cons( 
                construct_bp( $TOK_SYMBOL.ref(), construct( @$ ) ), 
                $3.ref() ); 
        }
    ;

binding_pattern_specifier
    : '#' TOK_SYMBOL
        { $$ = construct_bp( $TOK_SYMBOL.ref(), construct( @$ ) ); }
    ;

binding_pattern_specifier_opt
    : %empty
        { $$ = rref_list<binding_tc>(); }
    | binding_pattern_specifier
    ;
/*
integer_term
    : TOK_INTEGER
        { $$ = $TOK_INTEGER; }
    ;

integer_unary
    : integer_term
        { $$ = $integer_term; }
    | '-' integer_term
        { $$ = - $integer_term; }

integer_exp
    : integer_exp[lhs] '+' integer_unary[rhs]
        { $$ = $lhs + $rhs; }
    | integer_exp[lhs] '-' integer_unary[rhs]
        { $$ = $lhs - $rhs; }
    | integer_unary
        { $$ = $integer_unary; }
    ;
*/

compound_term
    : TOK_SYMBOL '(' term_list_opt[inner] ')'
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                $TOK_SYMBOL.ref(),
                rref<proper_type_tc>(),
                $inner.ref() ); 
        }
    ;

compound_term_explicit
    : compound_term
    | TOK_SYMBOL '(' term_list_opt[inner] ')' TOK_COLON_COLON proper_btype
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                $TOK_SYMBOL.ref(),
                $proper_btype.ref(),
                $inner.ref() );
        }
    ;

/*
variable_list
    : TOK_VARIABLE
    | TOK_VARIABLE ',' TOK_VARIABLE
    ;
*/

variable_seq
    : TOK_VARIABLE
        { $$ = cons( $1.ref() ); }
    | TOK_VARIABLE variable_seq
        { $$ = cons( $1.ref(), $2.ref() ); }
    ;

variable_seq_opt
    : %empty 
        { $$ = nil<string_tc>(); }
    | variable_seq
        { $$ = $1; }
    ;
/*
index_selection_seq
    : '[' term_list_opt[elem] ']'
        { $$ = cons( $elem.ref() ); }
    | '[' term_list_opt[elem] ']' index_selection_seq[list]
        { $$ = cons( $elem.ref(), $list.ref() ); }
    ;

index_selection_seq_opt
    : %empty
        { $$ = nil<list_tc<term_tc>>(); }
    | index_selection_seq
        { $$ = $1; }
    ;
*/
primary_term
    : TOK_VARIABLE
        {   
            $$ = construct<term_tc::variable>( 
                construct( @$ ), 
                $TOK_VARIABLE.ref(), 
                rref<proper_type_tc>() ); 
        }
    | TOK_VARIABLE TOK_COLON_COLON proper_btype
        {   
            $$ = construct<term_tc::variable>( 
                construct( @$ ), 
                $TOK_VARIABLE.ref(), 
                $proper_btype.ref() ); 
        }
    | '_'
        {
            $$ = construct<term_tc::anonymous>( construct( @$ ) );
        }
    | TOK_SYMBOL
        {
            $$ = construct<term_tc::string>( 
                construct( @$ ), 
                $TOK_SYMBOL.ref() );
        }
    | '@' TOK_SYMBOL
        {
            $$ = construct<term_tc::string>( 
                construct( @$ ), 
                find_macro( $TOK_SYMBOL.ref().str() ) );
        }
    | TOK_INTEGER
        {
            $$ = construct<term_tc::i32>( 
                construct( @$ ), 
                (uint32_t)$TOK_INTEGER ); 
        }
    | compound_term_explicit
        { $$ = $1; }
    | '$' term_list_opt '{' literal_list '}' TOK_ARROW primary_term[output]
        { 
            $$ = construct<term_tc::aggregate>(
                construct( @$ ),
                $term_list_opt, $output, $literal_list );
        }
    | '(' term ')'
        { $$ = $2; }
    ;

term
    : primary_term
    | '[' array_term_list_opt ']'
        { $$ = $array_term_list_opt; }
    | '!' term[t] %prec UNARY_NOT
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "equals",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ 
                    $t.ref(), 
                    construct<term_tc::i32>( 
                        construct( @$ ), 
                        (uint32_t)0 ) } );
        }
    | term[lhs] ':' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "cons",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '*' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "multiply",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '/' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "divide",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '+' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "add",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '-' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "sub",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '<' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "less_than",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] TOK_LT_EQ term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "less_than_eq",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '>' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "less_than",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $rhs.ref(), $lhs.ref() } );
        }
    | term[lhs] TOK_GT_EQ term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "less_than_eq",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $rhs.ref(), $lhs.ref() } );
        }
    | term[lhs] TOK_EQUAL_EQUAL term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "equals",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] TOK_NEQUAL term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "nequals",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '&' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "bitwise_and",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '^' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "bitwise_xor",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] '|' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "bitwise_or",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] TOK_AND term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "and",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | term[lhs] TOK_OR term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "or",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    | TOK_IF term[cond] TOK_THEN term[then]
        {
            $$ = construct<term_tc::conditional>(
                construct( @$ ),
                $cond.ref(),
                $then.ref(),
                rref<term_tc>() );
        }
    | TOK_IF term[cond] TOK_THEN term[then] TOK_ELSE term[else]
        {
            $$ = construct<term_tc::conditional>(
                construct( @$ ),
                $cond.ref(),
                $then.ref(),
                $else.ref() );
        }
    | term[lhs] ';' term[rhs]
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                "seq",
                rref<proper_type_tc>(),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() } );
        }
    ;


term_list
    : term
        { $$ = cons( $term.ref(), nil<term_tc>() ); }
    | term ',' term_list
        { $$ = cons( $term.ref(), $3.ref() ); }
    ;

term_list_opt
    : %empty
        { $$ = nil<term_tc>(); }
    | term_list
        { $$ = $1; }
    ;

array_term_list
    : term
        {   
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                rref_str( "cons" ), 
                rref<proper_type_tc>(),
                rref_list<term_tc>{ 
                    $term.ref(),
                    construct<term_tc::compound>(
                        construct( @$ ),
                        rref_str( "nil" ),
                        rref<proper_type_tc>(),
                        nil<term_tc>() ) } ); 
        }
    | term ',' array_term_list
        {   
            $$ = construct<term_tc::compound>(
                construct( @$ ), 
                rref_str( "cons" ), 
                rref<proper_type_tc>(),
                rref_list<term_tc>{ 
                    $term.ref(),
                    $3.ref() } ); 
        }
    ;

array_term_list_opt
    : %empty
        {
            $$ = construct<term_tc::compound>(
                construct( @$ ),
                rref_str( "nil" ),
                rref<proper_type_tc>(),
                nil<term_tc>() ); 
        }
    | array_term_list
        { $$ = $1; }
    ;

preferred_literal
	: %empty { $$ = false; }
	| '*'	 { $$ = true; }
	;

literal
    : TOK_SYMBOL preferred_literal '(' term_list_opt ')'
        {
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    $TOK_SYMBOL.ref(), 
                    construct_n( $term_list_opt.ref().length(), b_free() ) ),
                $term_list_opt.ref(), 
                0 );
			if( $preferred_literal ) { env.preferred_literals.emplace( $$ ); }
        }
    | TOK_SYMBOL preferred_literal binding_pattern_specifier '(' term_list_opt ')'
        {
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    $TOK_SYMBOL.ref(), 
                    $binding_pattern_specifier.ref() ),
                $term_list_opt.ref(), 
                0 ); 
			if( $preferred_literal ) { env.preferred_literals.emplace( $$ ); }
        }
    | '!' TOK_SYMBOL preferred_literal '(' term_list_opt ')'
        {
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    $TOK_SYMBOL.ref(), 
                    construct_n( $term_list_opt.ref().length(), b_free() ) ),
                $term_list_opt.ref(), 
                1 ); 
			if( $preferred_literal ) { env.preferred_literals.emplace( $$ ); }
        }
    | '!' TOK_SYMBOL preferred_literal binding_pattern_specifier '(' term_list_opt ')'
        {
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    $TOK_SYMBOL.ref(), 
                    $binding_pattern_specifier.ref() ),
                $term_list_opt.ref(), 
                1 ); 
			if( $preferred_literal ) { env.preferred_literals.emplace( $$ ); }
        }
    | TOK_WEAK_NEGATION TOK_SYMBOL preferred_literal '(' term_list_opt ')'
        {
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    $TOK_SYMBOL.ref(), 
                    construct_n( $term_list_opt.ref().length(), b_free() ) ),
                $term_list_opt.ref(), 
                2 ); 
			if( $preferred_literal ) { env.preferred_literals.emplace( $$ ); }
        }
    | TOK_WEAK_NEGATION TOK_SYMBOL preferred_literal binding_pattern_specifier '(' term_list_opt ')'
        {
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    $TOK_SYMBOL.ref(), 
                    $binding_pattern_specifier.ref() ),
                $term_list_opt.ref(), 
                2 ); 
			if( $preferred_literal ) { env.preferred_literals.emplace( $$ ); }
        }
    | '(' term[lhs] '=' term[rhs] ')'
        {   
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    rref_str( "equals" ), 
                    construct_n( 2, b_free() ) ),
                rref_list<term_tc>{ $lhs.ref(), $rhs.ref() },
                0 ); 
        }
    | '(' term[t] ')'
        {   
            $$ = construct<literal_tc>(
                construct( @$ ), 
                construct<adorned_predicate_tc>(
                    rref_str( "not_zero" ), 
                    construct_n( 1, b_free() ) ),
                rref_list<term_tc>{ $t.ref() },
                0 ); 
        }
    ;

literal_list
    : literal
        { $$ = cons( $literal.ref() ); }
    | literal literal_list
        { $$ = cons( $literal.ref(), $2.ref() ); }
    | literal ',' literal_list
        { $$ = cons( $literal.ref(), $3.ref() ); }
    ;

symbol_seq
    : TOK_SYMBOL
        { $$ = cons( $TOK_SYMBOL.ref() ); }
    | TOK_SYMBOL symbol_seq
        { $$ = cons( $TOK_SYMBOL.ref(), $2.ref() ); }
    ;

symbol_seq_opt
    : %empty 
        { $$ = nil<string_tc>(); }
    | symbol_seq
        { $$ = $1; }
    ;

btype
    : type
        { $$ = $1; }
    | TOK_SYMBOL atype_seq
        {
            $$ = construct<type_tc::atype>(
                $TOK_SYMBOL.ref(),
                $atype_seq.ref() );
        }
    ;

type
    : TOK_VARIABLE
        {   
            $$ = construct<type_tc::variable>(
                $TOK_VARIABLE.ref() );
        }
    | TOK_SYMBOL
        {   
            $$ = construct<type_tc::atype>(
                $TOK_SYMBOL.ref(),
                nil<type_tc>() );
        }
    | '[' btype ']'
        {   
            $$ = construct<type_tc::atype>(
                rref_str( "list" ),
                cons( $btype.ref() ) );
        }
    | '(' btype ')'
        { $$ = $btype; }
    ;

atype_seq
    : type
        { $$ = cons( $1.ref() ); }
    | type atype_seq
        { $$ = cons( $1.ref(), $2.ref() ); }
    ;

atype_seq_opt
    : %empty
        { $$ = nil<type_tc>(); }
    | atype_seq
        { $$ = $1; }
    ;


btype_list
    : btype
        { $$ = cons( $1.ref() ); }
    | btype ',' btype_list
        { $$ = cons( $1.ref(), $3.ref() ); }
    ;

btype_list_opt
    : %empty
        { $$ = nil<type_tc>(); }
    | btype_list
        { $$ = $1; }
    ;

proper_btype
    : TOK_SYMBOL proper_atype_seq
        {   
            $$ = construct<proper_type_tc>(
                $TOK_SYMBOL.ref(), 
                $proper_atype_seq.ref() );
        }
    | proper_atype
    ;

proper_atype
    : TOK_SYMBOL
        {   
            $$ = construct<proper_type_tc>(
                $TOK_SYMBOL.ref(), 
                nil<proper_type_tc>() );
        }
    | '(' proper_btype ')'
        { $$ = $proper_btype; }
    | '[' proper_btype ']'
        {   
            $$ = construct<proper_type_tc>(
                rref_str( "list" ),
                cons( $proper_btype.ref() ) );
        }
                
    ;

proper_atype_seq
    : proper_atype
        { $$ = cons( $proper_atype.ref() ); }
    | proper_atype proper_atype_seq
        { $$ = cons( $proper_atype.ref(), $2.ref() ); }
    ;

proper_atype_seq_opt
    : %empty
        { $$ = nil<proper_type_tc>(); }
    | proper_atype_seq
        { $$ = $1; }
    ;

record_specifier
    : TOK_SYMBOL TOK_COLON_COLON btype
        { $$ = $btype; }
    ;

record_specifier_list
    : record_specifier
        { $$ = cons( $record_specifier.ref() ); }
    | record_specifier ',' record_specifier_list
        { $$ = cons( $record_specifier.ref(), $3.ref() ); }
    ;

dconstr
    : TOK_SYMBOL atype_seq_opt
        { $$ = construct<dconstr_tc>( $TOK_SYMBOL.ref(), $atype_seq_opt.ref() ); }
    | TOK_SYMBOL '{' record_specifier_list '}'
        { $$ = construct<dconstr_tc>( $TOK_SYMBOL.ref(), $record_specifier_list.ref() ); }
    ;

dconstr_list
    : dconstr
        { $$ = cons( $dconstr.ref() ); }
    | dconstr '|' dconstr_list
        { $$ = cons( $dconstr.ref(), $3.ref() ); }
    ;
