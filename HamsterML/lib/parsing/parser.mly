%{
    open Ast    
%}

// --- Tokens ---

// Data Types
%token <int> TYPE_INT
%token <bool> TYPE_BOOL
%token <string> TYPE_STRING
%token TYPE_UNIT

// Explicit Types
%token INT
%token BOOL
%token STRING

%token <string> IDENTIFIER

// Statements 
%token IF
%token THEN
%token ELSE
%token FUN          // "fun _ -> _"
%token LET
%token IN
%token REC
%token MATCH
%token WITH
%token WILDCARD     // "_"

// Separators
%token ARROW        // "->"
%token COMMA        // "."
%token SEMICOLON    // ";"
%token COLON        // ":"
%token DOUBLE_COLON // "::"
%token BAR          // "|"

// braces
%token LEFT_PARENTHESIS       // "("
%token RIGHT_PARENTHESIS      // ")"
%token LEFT_SQ_BRACKET        // "["
%token RIGHT_SQ_BRACKET       // "]"

// Operators
%token PLUS                 // "+"
%token MINUS                // "-"
%token ASTERISK             // "*"
%token SLASH                // "/"
%token CARET                // "^"
%token EQUAL                // "="
%token IDENTICAL_EQ         // "=="
%token NOT_EQUAL            // "!=" || "<>"
%token GREATER_THAN         // ">"
%token GREATER_THAN_EQUAL   // ">="
%token LESS_THAN            // "<"
%token LESS_THAN_EQUAL      // "<"
%token LET_AND              // "let x = 1 and y = 2" 
%token AND                  // "&&"
%token OR                   // "||"
%token NOT                  // "not"

// End Of File
%token EOF

// --- Priorities ---
%left OR
%left AND
%left NOT

%left GREATER_THAN_EQUAL
%left LESS_THAN_EQUAL
%left GREATER_THAN
%left LESS_THAN
%left NOT_EQUAL
%left EQUAL
%left IDENTICAL_EQ

%left CARET
%left PLUS, MINUS
%left ASTERISK, SLASH

// --- Parsing ---
%start <expr list> prog
%start <expr> prog_expr
%start <pattern> prog_pattern
%%

// --- Subs ---

%inline paramType:
    | INT                   { PInt }
    | BOOL                  { PBool }
    | STRING                { PString }

%inline bop:
    | PLUS                  { ADD }                 
    | MINUS                 { SUB }        
    | ASTERISK              { MUL }                  
    | SLASH                 { DIV }                  
    | CARET                 { CONCAT }               
    | EQUAL                 { EQ }       
    | IDENTICAL_EQ          { ID_EQ }         
    | NOT_EQUAL             { NEQ }           
    | GREATER_THAN          { GT }       
    | GREATER_THAN_EQUAL    { GTE }  
    | LESS_THAN             { LT }      
    | LESS_THAN_EQUAL       { LTE }     
    | AND                   { AND }            
    | OR                    { OR }            

%inline uop:
    | NOT   { NOT }
    | MINUS {UMINUS}
    | PLUS  {UPLUS}

value:
    | TYPE_INT          { Int $1 }
    | TYPE_BOOL         { Bool $1 }
    | TYPE_STRING       { String $1 }
    | TYPE_UNIT         { Unit }

// --- Parser rules ---

prog : list (declare) EOF { $1 }

(* for testing purposes *)
prog_expr : expr EOF { $1 }
prog_pattern : pattern EOF { $1 }

(* special expr for resolving tuple conflicts *)

declare:
    | _let  { $1 }

list_expr:
    | value                     { EConst $1 }
    | id                        { EVar $1 }
    | operation                 { $1 }
    | prefix_bop                { EOperation $1 }
    | _fun                      { $1 }
    | _if                       { $1 }
    | _match                    { $1 }
    | _list(list_expr)          { EList $1 }
    | _tuple(tuple_expr)        { ETuple $1 }
    | application               { $1 }
    | concat(concat_expr)       { let a,b = $1 in EListConcat (a,b) }

tuple_expr:
    | value                                                         { EConst $1 }
    | id                                                            { EVar $1 }
    | operation                                                     { $1 }
    | prefix_bop                                                    { EOperation $1 }
    | _fun                                                          { $1 }
    | _match                                                        { $1 }
    | _list(list_expr)                                              { EList $1 }
    | application                                                   { $1 }
    | concat(concat_expr)                                           { let a,b = $1 in EListConcat (a,b) }
    | LEFT_PARENTHESIS; _tuple(tuple_expr); RIGHT_PARENTHESIS       { ETuple $2 }

l_app_expr: 
    | id                                                    { EVar $1 } 
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS             { $2 }
    | LEFT_PARENTHESIS; prefix_bop; RIGHT_PARENTHESIS       { EOperation $2 }
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS              { $2 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS           { $2 }
    | application                                           { $1 }

r_app_expr:
    | value                                                         { EConst $1 }
    | id                                                            { EVar $1 }
    | LEFT_PARENTHESIS; operation; RIGHT_PARENTHESIS                { $2 }
    | LEFT_PARENTHESIS; concat(concat_expr); RIGHT_PARENTHESIS      { let a,b = $2 in EListConcat (a,b) }
    | LEFT_PARENTHESIS; prefix_bop; RIGHT_PARENTHESIS               { EOperation $2 }
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS                     { $2 }
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS                      { $2 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS                   { $2 }
    | LEFT_PARENTHESIS; _list(list_expr); RIGHT_PARENTHESIS         { EList $2 }
    | LEFT_PARENTHESIS; _tuple(tuple_expr); RIGHT_PARENTHESIS       { ETuple $2 }
    | LEFT_PARENTHESIS; application; RIGHT_PARENTHESIS              { $2 }

op_expr: 
    | value                                             { EConst $1 }
    | id                                                { EVar $1 }
    | application                                       { $1 }
    | operation                                         { $1 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS       { $2 }   
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS          { $2 }

concat_expr:
    | value                                                         { EConst $1 }
    | id                                                            { EVar $1 }
    | operation                                                     { $1 }
    | prefix_bop                                                    { EOperation $1 }
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS                     { $2 }
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS                      { $2 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS                   { $2 }   
    | _list(list_expr)                                              { EList $1 }
    | LEFT_PARENTHESIS; _tuple(tuple_expr); RIGHT_PARENTHESIS       { ETuple $2 }
    | application                                                   { $1 }
    | concat(concat_expr)                                           { let a,b = $1 in EListConcat (a,b) }

expr:
    | LEFT_PARENTHESIS; e = expr; RIGHT_PARENTHESIS     { e }
    | value                                             { EConst $1 }
    | id                                                { EVar $1 }
    | operation                                         { $1 }
    | prefix_bop                                        { EOperation $1 }
    | _fun                                              { $1 }
    | _if                                               { $1 }
    | _match                                            { $1 }   
    | _list(list_expr)                                  { EList $1 }
    | _tuple(tuple_expr)                                { ETuple $1 }
    | concat(concat_expr)                               { let a,b = $1 in EListConcat (a,b) }
    | application                                       { $1 }
    | _let                                              { $1 }

tuple_pattern:
    | value                                                         { Const $1 } 
    | id                                                            { Var $1 }
    | WILDCARD                                                      { Wildcard }
    | prefix_bop                                                    { Operation $1 }
    | _list(list_pattern)                                           { List $1 }
    | LEFT_PARENTHESIS; _tuple(tuple_pattern); RIGHT_PARENTHESIS    { Tuple $2 }

list_pattern:
    | LEFT_PARENTHESIS; p = pattern; RIGHT_PARENTHESIS      { p }
    | value                                                 { Const $1 } 
    | id                                                    { Var $1 }
    | WILDCARD                                              { Wildcard }
    | prefix_bop                                            { Operation $1 }
    | _list(list_pattern)                                   { List $1 }
    | _tuple(tuple_pattern)                                 { Tuple $1 }

concat_pattern:
    | value                                                         { Const $1 }
    | id                                                            { Var $1 }
    | prefix_bop                                                    { Operation $1 }
    | _list(list_pattern)                                           { List $1 }
    | LEFT_PARENTHESIS; _tuple(tuple_pattern); RIGHT_PARENTHESIS    { Tuple $2 }
    | concat(concat_pattern)                                        { let a,b = $1 in ListConcat (a,b) }
    | WILDCARD                                                      { Wildcard }

pattern:
    | LEFT_PARENTHESIS; p = pattern; RIGHT_PARENTHESIS      { p }
    | value                                                 { Const $1 } 
    | id                                                    { Var $1 }
    | WILDCARD                                              { Wildcard }
    | prefix_bop                                            { Operation $1 }
    | _tuple(tuple_pattern)                                 { Tuple $1 }
    | _list(list_pattern)                                   { List $1 }
    | concat(concat_pattern)                                { let a,b = $1 in ListConcat (a,b) }


concat (rule):
    | rule; DOUBLE_COLON; rule                                  { $1, $3 }
    | LEFT_PARENTHESIS; concat (rule); RIGHT_PARENTHESIS        { $2 }

(* default operations like "1 + 2" *)
operation:
    | LEFT_PARENTHESIS; operation; RIGHT_PARENTHESIS           { $2 }
    | e1 = op_expr; op = bop; e2 = op_expr                     { Application ( Application (EOperation (Binary op), e1), e2 ) }
    | op = uop; e = op_expr                                    { Application (EOperation (Unary op), e) } 

application:
    | LEFT_PARENTHESIS; a = application; RIGHT_PARENTHESIS      { a }
    | f = l_app_expr; a = r_app_expr                            { Application (f, a) }

_match:
    | MATCH; e = expr; WITH; cases = separated_nonempty_list(BAR, match_case)           { Match (e, cases) }
    | MATCH; e = expr; WITH; BAR; cases = separated_nonempty_list(BAR, match_case)      { Match (e, cases) }
    | LEFT_PARENTHESIS; m = _match; RIGHT_PARENTHESIS                                   { m }

%inline match_case: p = pattern; ARROW; e = expr    { (p, e) }

%inline _if:
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr       { If (e1, e2, Some e3) }
    | IF; e1 = expr; THEN; e2 = expr                        { If (e1, e2, None) }

(* let (+) x y = ... *)
%inline prefix_bop:
    | LEFT_PARENTHESIS; op = bop; RIGHT_PARENTHESIS     { Binary op }
    
%inline _fun:
    | FUN; vls = nonempty_list(pattern); ARROW; e = expr    { Fun (vls, e) }

%inline id:
    | IDENTIFIER    { $1 }

%inline _list(rule):
  LEFT_SQ_BRACKET; elements = separated_list(SEMICOLON, rule); RIGHT_SQ_BRACKET     { elements }

_tuple (rule):
    | lst = tuple_simple (rule)                                     { lst }
    | LEFT_PARENTHESIS; lst = _tuple (rule); RIGHT_PARENTHESIS      { lst }    

tuple_simple (rule):
    | e1 = rule; COMMA; e2 = rule                   { [e1;e2] }
    | e = rule; COMMA; t = tuple_simple(rule)       { e :: t }

%inline rec_flag:
    | REC   { Recursive } 
    |       { Nonrecursive }

%inline _let:
    | LET; rec_flag; separated_nonempty_list(LET_AND, _bind); IN; expr  { Let($2, $3, Some $5) }
    | LET; rec_flag; separated_nonempty_list(LET_AND, _bind)            { Let($2, $3, None) }

%inline _bind:
    | pattern; list(pattern); EQUAL; expr { ($1, $2, $4) } (* f x y = x + y *)
