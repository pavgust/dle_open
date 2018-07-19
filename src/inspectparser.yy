%pure-parser
%defines
%locations
%error-verbose
%debug

%parse-param { yyscan_t yyscanner }
%lex-param   { yyscan_t yyscanner }

%token TOK_EOF 0
%token TOK_SYMBOL
%token TOK_NUMERIC
%token TOK_INTEGER
%token TOK_VARIABLE
%token TOK_DATA
%token TOK_COLON_COLON

%union
{
    string* str;
    vector<string>* strs;
    atype* at;
    vector<atype>* ats;
    field* f;
    vector<field>* fs;
    dconstr* dc;
    vector<dconstr>* dcs;
    double dnum;
    int inum;
    bool b;
}

%type <str>
    TOK_SYMBOL

%type <strs>
    symbol_seq
    symbol_seq_opt

%type <at>
    btype
    atype

%type <f>
    record_specifier

%type <ats>
    type_seq
    type_seq_opt

%type <fs>
    record_specifier_list

%type <dc>
    dconstr

%type <dcs>
    dconstr_list

%%

start 
    : statement_seq 
    | %empty ;

statement_seq
    : statement
    | statement_seq statement
    ;

statement 
    : TOK_DATA TOK_SYMBOL symbol_seq_opt '=' dconstr_list '.'
        { 
            type_constructors.push_back( {
                *make( $TOK_SYMBOL ),
                *make( $symbol_seq_opt ),
                *make( $dconstr_list ) } );
        }
    | TOK_DATA TOK_SYMBOL symbol_seq_opt '.'
        { 
            type_constructors.push_back( {
                *make( $TOK_SYMBOL ),
                *make( $symbol_seq_opt ),
                *make( new vector<dconstr>() ) } );
        }
    ;

symbol_seq
    : TOK_SYMBOL
        { $$ = new vector<string>{ *make( $TOK_SYMBOL ) }; }
    | symbol_seq TOK_SYMBOL
        { $$ = $1; $$->push_back( *make( $TOK_SYMBOL ) ); }
    ;

symbol_seq_opt
    : %empty 
        { $$ = new vector<string>; }
    | symbol_seq
        { $$ = $1; }
    ;

dconstr
    : TOK_SYMBOL type_seq_opt
        { $$ = new dconstr{ *make( $TOK_SYMBOL ), to_fields( *make( $type_seq_opt ) ) }; }
    | TOK_SYMBOL '{' record_specifier_list '}'
        { $$ = new dconstr{ *make( $TOK_SYMBOL ), *make( $record_specifier_list ) }; }
    ;

dconstr_list
    : dconstr
        { $$ = new vector<dconstr>{ *make( $dconstr ) }; }
    | dconstr_list '|' dconstr
        { $$ = $1; $$->push_back( *make( $dconstr ) ); }
    ;

btype
    : atype
        { $$ = $1; }
    | TOK_SYMBOL type_seq
        { $$ = new atype{ *make( $TOK_SYMBOL ), *make( $type_seq ) }; }
    ;

atype
    : TOK_SYMBOL
        { $$ = new atype{ *make( $TOK_SYMBOL ), {} }; }
    | '[' btype ']'
        { $$ = new atype{ "list", { *make( $btype ) } }; }
    | '(' btype ')'
        { $$ = $btype; }
    ;

type_seq
    : atype
        { $$ = new vector<atype>{ *make( $atype ) }; }
    | type_seq atype
        { $$ = $1; $$->push_back( *make( $atype ) ); }
    ;

type_seq_opt
    : %empty 
        { $$ = new vector<atype>; }
    | type_seq
        { $$ = $1; }
    ;

record_specifier
    : TOK_SYMBOL TOK_COLON_COLON btype
        { $$ = new field{ *make( $TOK_SYMBOL ), *make( $btype ) }; }
    ;

record_specifier_list
    : record_specifier
        { $$ = new vector<field>{ *make( $1 ) }; }
    | record_specifier_list ',' record_specifier
        { $$ = $1; $$->push_back( *make( $3 ) ); }
    ;

