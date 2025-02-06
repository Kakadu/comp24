%{
    open Ast    
%}

// --- Tokens ---

// Data Types
%token <int> TYPE_INT
%token <float> TYPE_FLOAT
%token <bool> TYPE_BOOL
%token <char> TYPE_CHAR
%token <string> TYPE_STRING
%token TYPE_UNIT

// Explicit Types
%token INT
%token FLOAT
%token BOOL
%token CHAR
%token STRING
%token <string> POLYMORPHIC_NAME // (x: `a) (y: `b)

%token <char>   OP_IDENTIFIER // let (+) = ...
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
%token NOT_EQUAL            // "!=" || "<>" TODO: check semantics
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

%left CARET
%left PLUS, MINUS
%left ASTERISK, SLASH

// --- Parsing ---
%type <expr list> prog
%start prog
%%

prog: p = list(declaration) EOF { p }

declaration:
    | LET; l = let_def          { l }
    | LET; l = let_and_in_def   { l } 

expr:
    | LEFT_PARENTHESIS; e = expr; RIGHT_PARENTHESIS                         { e }
    | v = patternValue                                                      { Pattern v }
    | uop = uop; e = expr                                                   { UnOp (uop, e) }
    | le = expr; bop = bop; re = expr                                       { BinOp (bop, le, re) }
    | MATCH; expr = expr; WITH; match_cases = match_cases                   { Match (expr, match_cases) }
    | d = declaration                                                       { d }
    | FUN; vls = nonempty_list(patternValue); ARROW; e = expr               { Fun (vls, e) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr                       { If (e1, e2, Some e3) }
    | IF; e1 = expr; THEN; e2 = expr                                        { If (e1, e2, None) }
    | le = expr; re = expr                                                  { Application (le,re) }

dataType:
    | f = float             { f }
    | i = int               { i }
    | b = TYPE_BOOL         { Bool b }
    | c = TYPE_CHAR         { Char c }
    | s = TYPE_STRING       { String s }
    | TYPE_UNIT             { Unit }

patternValue:
    | LEFT_PARENTHESIS; v = patternValue; RIGHT_PARENTHESIS     { v }
    | WILDCARD                                                  { Wildcard }
    | t_v = typed_var                                           { t_v }
    | v = const_or_var                                          { v }
    | p = pattern                                               { p }

pattern:
    | tpl = tuple_dt                                        { Tuple tpl } // (1, 2, 3, ...) 
    | lst = list_dt                                         { List lst }  // [1; 2; 3; ...] 
    | lv = const_or_var; DOUBLE_COLON; rv = const_or_var    { ListConcat (lv, rv) } // hd :: tl 
    | v = const_or_var ; DOUBLE_COLON; l = list_dt          { ListConcat (v, List l) } // 1 :: [2; 3] 

int:
    | i = TYPE_INT              {Int i}
    | MINUS; i = TYPE_INT       {Int (-i)}
    | PLUS; i = TYPE_INT        {Int (i)}

float:
    | f = TYPE_FLOAT            {Float f}
    | MINUS; f = TYPE_FLOAT     {Float (-.f)}
    | PLUS; f = TYPE_FLOAT      {Float (f)}

%inline paramType:
    | INT       { PInt }
    | FLOAT     { PFloat }
    | BOOL      { PBool }
    | CHAR      { PChar }
    | STRING    { PString }

%inline bop:
    | PLUS                  { ADD }                 
    | MINUS                 { SUB }        
    | ASTERISK              { MUL }                  
    | SLASH                 { DIV }                  
    | CARET                 { CONCAT }               
    | EQUAL                 { EQ }                
    | NOT_EQUAL             { NEQ }           
    | GREATER_THAN          { GT }       
    | GREATER_THAN_EQUAL    { GTE }  
    | LESS_THAN             { LT }      
    | LESS_THAN_EQUAL       { LTE }     
    | AND                   { AND }            
    | OR                    { OR }            

%inline uop:
    | NOT { NOT }

%inline func_id: 
    // let (+) x y = x - y  (operators as names)
    | op = OP_IDENTIFIER    { VarId ( String.make 1 op ) }
    // let f x = let (k, j) = x in j in f (1, 2) (regular names and patterns like (k,j))
    | v = patternValue      { v }

const_or_var: // Const or variable 
    | const = dataType      { Const const } 
    | varId = IDENTIFIER    { VarId varId }

typed_var: typedVarId = IDENTIFIER; COLON; varType = paramType {TypedVarID (typedVarId, varType) }

%inline tuple_dt: LEFT_PARENTHESIS; arg_list = separated_nonempty_list(COMMA, patternValue); RIGHT_PARENTHESIS      { arg_list }

%inline list_dt: LEFT_SQ_BRACKET; arg_list = separated_list(SEMICOLON, patternValue); RIGHT_SQ_BRACKET              { arg_list }

match_cases: first_match_case = match_case_first; rest_cases = list(match_case)     { first_match_case :: rest_cases }

match_case_first: bar_opt; v = patternValue; ARROW; e = expr     { (v, e) } 

bar_opt: 
    | BAR   { () }
    |       { () }

match_case: BAR; v = patternValue; ARROW; e = expr      { (v, e) }

%inline rec_flag:
    | REC   { Recursive }
    |       { Nonrecursive }

%inline let_def:
    // () = print_endline "123" 
    | TYPE_UNIT; EQUAL; e = expr                                                            { Let(Nonrecursive, Const(Unit), [], e)}
    // [rec] f x = x 
    | rec_opt = rec_flag; fun_name = func_id; args = list(patternValue); EQUAL; e = expr    { Let(rec_opt, fun_name, args, e) }

// a = 10 and b = 20 and ... 
and_bind:
    | l = let_def                               { [l] }
    | h = let_def; LET_AND; tl = and_bind       { h :: tl }

// a = 10 and b = 20 and ... in a + b + ... 
let_and_in_def: 
    | e1 = let_def; IN; e2 = expr       { LetAndIn ([e1], Some e2) }    // without and 
    | exs = and_bind; IN; e = expr      { LetAndIn (exs, Some e) }      // with and 
    | exs = and_bind;                   { LetAndIn (exs, None) }        // and .. and .. without IN 
