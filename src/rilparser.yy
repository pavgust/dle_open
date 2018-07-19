%pure-parser
%defines
%locations
%error-verbose
%name-prefix "ril"

%parse-param { yyscan_t yyscanner }
%lex-param   { yyscan_t yyscanner }

%{
#include "rilparser.h"
%}

%token TOK_EOF 0
%token TOK_BRANCH
%token TOK_IDENTIFIER
%token TOK_INTEGER_CONSTANT
%token TOK_RETURN
%token TOK_GOTO
%token TOK_UNKNOWN
%token TOK_ALLOC
%token TOK_IF
%token TOK_ELSE
%token TOK_WHILE
%token TOK_DO
%token TOK_TYPE
%token TOK_PRAGMA

// lowest
%right '='
%left '+' '-'
%left '*'
%precedence DEREF ADDROF
%precedence '(' '.'
// Highest

%precedence NOT_ELSE
%precedence TOK_ELSE

%union
{
   urref<string_tc> str;
   urref<list_tc<string_tc>> strs;
   urref<i32_tc> ival;
   urref<expression_tc> expr;
   urref<list_tc<expression_tc>> exprs;
   urref<statement_tc> stmt;
   urref<list_tc<statement_tc>> stmts;
}

%type <str>
    TOK_IDENTIFIER 
    type

%type <strs>
    variable_decl
    identifier_list
    identifier_list_opt

%type <ival>        
    TOK_INTEGER_CONSTANT

%type <expr>
    expression
    primary_expression
    
%type <exprs>
    expression_list
    expression_list_opt

%type <stmt>
    statement

%type <stmts>
    statement_seq
    statement_seq_opt

%start file

%%

file
    : decl_seq
    | %empty
    ;

decl_seq
    : function_decl
    | decl_seq function_decl
    | variable_decl
    | decl_seq variable_decl
    ;

type
    : TOK_IDENTIFIER
    | TOK_TYPE
        { $$ = rref_str( "type" ); }
    ;

variable_decl
    : type identifier_list ';'
        { $$ = $identifier_list; }
    ;

function_decl
    : type TOK_IDENTIFIER '(' identifier_list_opt ')' '{' statement_seq_opt '}'
        {
            functions.push_back(
                construct<function_tc>(
                    construct( @$ ),
                    $TOK_IDENTIFIER,
                    to_formal_list( $TOK_IDENTIFIER, $identifier_list_opt ),
                    $statement_seq_opt,
                    construct_global( $TOK_IDENTIFIER ) ) );
        }
    ;

identifier_list
    : TOK_IDENTIFIER
        { $$ = rref_list<string_tc>{ $1 }; }
    | TOK_IDENTIFIER ',' identifier_list
        { $$ = cons<string_tc>( $1, $3 ); }
    ;

identifier_list_opt
    : %empty
        { $$ = nil<string_tc>(); }
    | identifier_list
        { $$ = $1; }
    ;

statement_seq
    : statement
        { $$ = rref_list<statement_tc>{ $1 }; }
    | statement statement_seq
        { $$ = cons<statement_tc>( $1, $2 ); }
    ;

statement_seq_opt
    : %empty
        { $$ = nil<statement_tc>(); }
    | statement_seq
        { $$ = $1; }
    ;

statement
    : variable_decl
        { 
            $$ = construct<statement_tc::variable_decl>(
                construct( @$ ),
                $variable_decl );
        }
    | TOK_IDENTIFIER ':'
        { $$ = construct<statement_tc::label>( construct( @$ ), $TOK_IDENTIFIER ); }
    | TOK_BRANCH TOK_IDENTIFIER ';'
        { die(); }
    | TOK_GOTO TOK_IDENTIFIER ';'
        { die(); }
    | TOK_IF statement[true] %prec NOT_ELSE
        { 
            $$ = construct<statement_tc::if_>( 
                construct( @$ ), 
                $true,
                construct<statement_tc::empty>() ); 
        }
    | TOK_IF statement[true] TOK_ELSE statement[false]
        { 
            $$ = construct<statement_tc::if_>( 
                construct( @$ ), 
                $true,
                $false ); 
        }
    | TOK_WHILE statement[body]
        {
            $$ = construct<statement_tc::while_>(
                construct( @$ ),
                $body );
        }
    | TOK_DO statement[body] TOK_WHILE ';'
        {
            $$ = construct<statement_tc::do_>(
                construct( @$ ),
                $body );
        }
    | expression ';'
        { $$ = construct<statement_tc::expression>( construct( @$ ), $expression ); }
    | '{' statement_seq '}'
        { $$ = construct<statement_tc::block>( construct( @$ ), $statement_seq ); }
    | TOK_RETURN expression ';'
        { $$ = construct<statement_tc::return_>( construct( @$ ), $expression ); }
    | TOK_PRAGMA TOK_IDENTIFIER '(' expression_list_opt ')' ';'
        { die(); }
    | ';'
        { $$ = construct<statement_tc::empty>(); }
    ;

primary_expression
    : TOK_IDENTIFIER
        { $$ = construct<expression_tc::identifier>( construct( @$ ), $1 ); }
    | TOK_INTEGER_CONSTANT
        { $$ = construct<expression_tc::integer>( construct( @$ ), $1 ); }
    | TOK_UNKNOWN
        { die(); }
    | TOK_ALLOC type
        { die(); }
    | TOK_ALLOC
        { $$ = construct<expression_tc::allocation>( construct( @$ ) ); }
    | '(' expression ')'
        { $$ = $expression; }
    ;

expression
    : primary_expression
        { $$ = $1; }
    | expression[ptr] '.' TOK_INTEGER_CONSTANT[offset]
        { $$ = construct<expression_tc::pointer_offset>( construct( @$ ), $ptr, $offset ); }
    | expression[tgt] '(' expression_list_opt[args] ')'
        { $$ = construct<expression_tc::invoke>( construct( @$ ), $tgt, $args );
        }
    | '*' expression[inner] %prec DEREF
        { $$ = construct<expression_tc::dereference>( construct( @$ ), $inner ); }
    | '&' expression[inner] %prec ADDROF
        { $$ = construct<expression_tc::address_of>( construct( @$ ), $inner ); }
    | expression '*' expression
    | expression '+' expression
    | expression '-' expression
    | expression[lhs] '=' expression[rhs]
        { $$ = construct<expression_tc::assignment>( construct( @$ ), $lhs, $rhs ); }
    ;

expression_list
    : expression
        { $$ = rref_list<expression_tc>{ $1 }; }
    | expression ',' expression_list
        { $$ = cons<expression_tc>( $1, $3 ); }
    ;

expression_list_opt
    : %empty
        { $$ = rref_list<expression_tc>{}; }
    | expression_list
        { $$ = $1; }
    ;
