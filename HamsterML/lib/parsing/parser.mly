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
%token COMMA        // ","
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
%left ARROW
%right COMMA DOUBLE_COLON 

%nonassoc BAR

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
    | MINUS { UMINUS }
    | PLUS  { UPLUS }

value:
    | TYPE_INT          { Int $1 }
    | TYPE_BOOL         { Bool $1 }
    | TYPE_STRING       { String $1 }
    | TYPE_UNIT         { Unit }

// --- Parser rules ---

prog : list (declare) EOF { $1 }

(* for testing purposes *)
prog_expr : expr EOF        { $1 }
prog_pattern : pattern EOF  { $1 }

declare:
    | _let  { $1 }

(* possible elements in list *)
list_expr:
    | value                                 { EConst $1 }
    | id                                    { EVar $1 }
    | operation                             { $1 }
    | prefix_bop                            { EOperation $1 }
    | _fun                                  { $1 }
    | _if                                   { $1 }
    | _match                                { $1 }
    | _list(list_expr)                      { EList $1 }
    | _tuple(tuple_expr)                    { let a, b, tl = $1 in ETuple (a, b, tl)  }
    | application                           { $1 }
    | concat(concat_expr)                   { let a,b = $1 in EListConcat (a, b) }
    | _constraint (constraint_expr)         { let a,b = $1 in EConstraint (a,b)  }

(* possible elements in tuple *)
tuple_expr:
    | value                                                         { EConst $1 }
    | id                                                            { EVar $1 }
    | operation                                                     { $1 }
    | prefix_bop                                                    { EOperation $1 }
    | _fun                                                          { $1 }
    | _match                                                        { $1 }
    | _list(list_expr)                                              { EList $1 }
    | application                                                   { $1 }
    | concat(concat_expr)                                           { let a,b = $1 in EListConcat (a, b) }
    | LEFT_PARENTHESIS; _tuple(tuple_expr); RIGHT_PARENTHESIS       { let a, b, tl = $2 in ETuple (a, b, tl) }
    | _constraint (constraint_expr)                                 { let a,b = $1 in EConstraint (a,b)  }

(* possible left parts of application *)
l_app_expr: 
    | id                                                                    { EVar $1 }
    | prefix_bop                                                            { EOperation $1 }
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS                             { $2 }
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS                              { $2 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS                           { $2 }
    | application                                                           { $1 }

(* possible rights parts of application *)
r_app_expr:
    | value                                                         { EConst $1 }
    | id                                                            { EVar $1 }
    | _list(list_expr)                                              { EList $1 }
    | prefix_bop;                                                   { EOperation $1 }
    | LEFT_PARENTHESIS; operation; RIGHT_PARENTHESIS                { $2 }
    | LEFT_PARENTHESIS; concat(concat_expr); RIGHT_PARENTHESIS      { let a,b = $2 in EListConcat (a, b) }
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS                     { $2 }
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS                      { $2 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS                   { $2 }
    | LEFT_PARENTHESIS; _tuple(tuple_expr); RIGHT_PARENTHESIS       { let a, b, tl = $2 in ETuple (a, b, tl) }
    | LEFT_PARENTHESIS; application; RIGHT_PARENTHESIS              { $2 }
    | _constraint (constraint_expr)                                 { let a,b = $1 in EConstraint (a,b)  }

(* all possible operands in arithmetic *)
op_expr: 
    | value                                             { EConst $1 }
    | id                                                { EVar $1 }
    | application                                       { $1 }
    | operation                                         { $1 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS       { $2 }   
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS          { $2 }

(* possible elements in constructions like '1 :: [2; 3]' *)
concat_expr:
    | value                                                         { EConst $1 }
    | id                                                            { EVar $1 }
    | operation                                                     { $1 }
    | prefix_bop                                                    { EOperation $1 }
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS                     { $2 }
    | LEFT_PARENTHESIS; _if; RIGHT_PARENTHESIS                      { $2 }
    | LEFT_PARENTHESIS; _match; RIGHT_PARENTHESIS                   { $2 }   
    | _list(list_expr)                                              { EList $1 }
    | LEFT_PARENTHESIS; _tuple(tuple_expr); RIGHT_PARENTHESIS       { let a, b, tl = $2 in ETuple (a, b, tl) }
    | application                                                   { $1 }
    | concat(concat_expr)                                           { let a,b = $1 in EListConcat (a, b) }
    | _constraint (constraint_expr)                                 { let a,b = $1 in EConstraint (a,b)  }

(* all possible elements to which we can apply type constraints *)
(* there is no 'fun' or 'let' because we don't support explicit arrow types *)
constraint_expr:
    | value                                             { EConst $1 }
    | id                                                { EVar $1 }
    | operation                                         { $1 }
    | prefix_bop                                        { EOperation $1 }
    | _if                                               { $1 }
    | _match                                            { $1 }   
    | application                                       { $1 }

(* union of all rules that can be called an expression *)
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
    | _tuple(tuple_expr)                                { let a, b, tl = $1 in ETuple (a, b, tl) }
    | concat(concat_expr)                               { let a,b = $1 in EListConcat (a, b) }
    | application                                       { $1 }
    | _let                                              { $1 }
    | _constraint (constraint_expr)                     { let a,b = $1 in EConstraint (a,b)  }

(* -- same for patterns, but a little simpler -- *)

(* possible elements in list *)
list_pattern:
    | value                                                 { Const $1 } 
    | id                                                    { Var $1 }
    | WILDCARD                                              { Wildcard }
    | prefix_bop                                            { Operation $1 }
    | _list(list_pattern)                                   { List $1 }
    | _tuple(tuple_pattern)                                 { let a, b, tl = $1 in Tuple (a, b, tl) }
    | _constraint (constraint_pattern)                      { let a,b = $1 in Constraint (a,b)  }

(* possible elements in tuple *)
tuple_pattern:
    | value                                                         { Const $1 } 
    | id                                                            { Var $1 }
    | WILDCARD                                                      { Wildcard }
    | prefix_bop                                                    { Operation $1 }
    | _list(list_pattern)                                           { List $1 }
    | LEFT_PARENTHESIS; _tuple(tuple_pattern); RIGHT_PARENTHESIS    { let a, b, tl = $2 in Tuple (a, b, tl) }
    | _constraint (constraint_pattern)                              { let a,b = $1 in Constraint (a,b)  }

(* possible elements in constructions like '1 :: [2; 3]' *)
concat_pattern:
    | value                                                         { Const $1 }
    | id                                                            { Var $1 }
    | prefix_bop                                                    { Operation $1 }
    | _list(list_pattern)                                           { List $1 }
    | LEFT_PARENTHESIS; _tuple(tuple_pattern); RIGHT_PARENTHESIS    { let a, b, tl = $2 in Tuple (a, b, tl) }
    | concat(concat_pattern)                                        { let a,b = $1 in ListConcat (a, b) }
    | WILDCARD                                                      { Wildcard }
    | _constraint (constraint_pattern)                              { let a,b = $1 in Constraint (a,b)  }

(* all possible elements to which we can apply type constraints *)
(* we can't apply type constraints to functions 'fun x y z : int -> ...' *)
constraint_pattern:
    | value                                                         { Const $1 } 
    | id                                                            { Var $1 }
    | WILDCARD                                                      { Wildcard }
    | prefix_bop                                                    { Operation $1 }

(* union of all rules that can be called a pattern *)
pattern:
    | LEFT_PARENTHESIS; p = pattern; RIGHT_PARENTHESIS              { p }
    | value                                                         { Const $1 } 
    | id                                                            { Var $1 }
    | WILDCARD                                                      { Wildcard }
    | prefix_bop                                                    { Operation $1 }
    | _tuple(tuple_pattern)                                         { let a, b, tl = $1 in Tuple (a, b, tl) }
    | _list(list_pattern)                                           { List $1 }
    | concat(concat_pattern)                                        { let a,b = $1 in ListConcat (a, b) }
    | _constraint (constraint_pattern)                              { let a,b = $1 in Constraint (a,b)  }

(* --- other rules --- *)

concat (rule):
    | rule; DOUBLE_COLON; rule                                  { $1, $3 }
    | LEFT_PARENTHESIS; concat (rule); RIGHT_PARENTHESIS  { $2 }

(* default operations like "1 + 2" *)
operation:
    | LEFT_PARENTHESIS; operation; RIGHT_PARENTHESIS                { $2 }
    | e1 = op_expr; op = bop; e2 = op_expr                          { Application ( Application (EOperation (Binary op), e1), e2 ) }
    | op = uop; e = op_expr                                         { Application (EOperation (Unary op), e) } 

application:
    | LEFT_PARENTHESIS; a = application; RIGHT_PARENTHESIS          { a }
    | f = l_app_expr; a = r_app_expr                                { Application (f, a) }

_match:
    | MATCH; e = expr; WITH; cases = separated_nonempty_list(BAR, match_case)           { Match (e, cases) }
    | MATCH; e = expr; WITH; BAR; cases = separated_nonempty_list(BAR, match_case)      { Match (e, cases) }
    | LEFT_PARENTHESIS; m = _match; RIGHT_PARENTHESIS                                   { m }

%inline match_case: p = pattern; ARROW; e = expr            { (p, e) }

%inline _if:
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr       { If (e1, e2, Some e3) }
    | IF; e1 = expr; THEN; e2 = expr                        { If (e1, e2, None) }

(* let (+) x y = ... *)
%inline prefix_bop:
    | LEFT_PARENTHESIS; op = bop; RIGHT_PARENTHESIS         { Binary op }
    
_fun:
    | FUN; vls = nonempty_list(pattern); ARROW; e = expr    { Fun (vls, e) }
    | LEFT_PARENTHESIS; _fun; RIGHT_PARENTHESIS             { $2 }

%inline id:
    | IDENTIFIER    { $1 }

_list(rule):
    | LEFT_SQ_BRACKET; elements = separated_list(SEMICOLON, rule); RIGHT_SQ_BRACKET     { elements }

_tuple (rule):
    | rule; COMMA; rule                                         { $1, $3, [] }
    | rule; COMMA; rule; COMMA; tuple_simple (rule)             { $1, $3, $5 }
    | LEFT_PARENTHESIS; _tuple (rule); RIGHT_PARENTHESIS        { $2 }    

tuple_simple (rule):
    | rule                                  { [$1] }
    | rule; COMMA; tuple_simple(rule)       { $1 :: $3 }

%inline rec_flag:
    | REC   { Recursive } 
    |       { Nonrecursive }

%inline _let:
    | LET; rec_flag; separated_nonempty_list(LET_AND, _bind); IN; expr  { Let($2, $3, Some $5) }
    | LET; rec_flag; separated_nonempty_list(LET_AND, _bind)            { Let($2, $3, None) }

%inline _bind:
    | pattern; list(pattern); EQUAL; expr { ($1, $2, $4) } (* f x y = x + y *)

%inline _constraint (rule): 
    | LEFT_PARENTHESIS; rule; COLON; paramType; RIGHT_PARENTHESIS { ($2, $4) }
