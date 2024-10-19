%{
    open Ast    
%}

// --- Tokens ---

// Data Types
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING

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
%token ASSIGN               // "=" (let a = 1)
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
%start <Ast.expr> prog
%%

prog : p = expr EOF { p }

expr:
    | v = value { Value v }
    | le = expr; bop = bop; re = expr { BinOp (bop, le, re) }
    | le = expr; re = expr { Application (le,re) }
    | uop = uop; e = expr { UnOp (uop, e) }
    | LET; exprs = separated_nonempty_list(LET_AND, expr); IN; e = expr { LetIn (exprs, e) }
    | MATCH; input_e = expr; WITH; match_cases = nonempty_list(match_case) { Match (input_e, match_cases) }
    | LET; REC; id = IDENTIFIER; vls = nonempty_list(value); EQUAL; e = expr { Let (Recursive, id, vls, e) }
    | LET; id = IDENTIFIER; vls = nonempty_list(value); EQUAL; e = expr { Let (Nonrecursive, id, vls, e) }
    | FUN; vls = nonempty_list(value); ARROW; e = expr { Fun (vls, e) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }

%inline match_case: 
    | BAR; v = value; ARROW; e = expr { (v, e) }

dataType:
    | i = INT {Int i}
    | f = FLOAT {Float f}
    | b = BOOL {Bool b}
    | c = CHAR {Char c}
    | s = STRING {String s}
    | LEFT_PARENTHESIS; data = dataType; RIGHT_PARENTHESIS { data } // (10), ("10"), etc...

identifier:
    | name = IDENTIFIER { name }
    | LEFT_PARENTHESIS; name = identifier; RIGHT_PARENTHESIS { name }

value:
    | const = dataType {Const const} 
    | varId = identifier { VarId varId }
    | typedVarId = identifier; COLON; varType = dataType  {TypedVarID (typedVarId, varType)}
    | WILDCARD {Wildcard}
    | lst = list_dt {List lst}
    | v1 = value ; DOUBLE_COLON; v2 = value  { ListConcat (v1, v2) }
    | LEFT_PARENTHESIS; tuple = separated_nonempty_list(COMMA, value); RIGHT_PARENTHESIS {Tuple tuple}
    | LEFT_PARENTHESIS; v = value; RIGHT_PARENTHESIS { v }

%inline list_dt: LEFT_SQ_BRACKET; val_list = separated_nonempty_list(SEMICOLON, value); RIGHT_SQ_BRACKET { val_list }


%inline bop:
    | PLUS { ADD }                 
    | MINUS { SUB }        
    | ASTERISK { MUL }                  
    | SLASH{ DIV }                  
    | CARET { CONCAT }               
    | EQUAL { EQ }                
    | NOT_EQUAL { NEQ }           
    | GREATER_THAN { GT }       
    | GREATER_THAN_EQUAL { GTE }  
    | LESS_THAN { LT }      
    | LESS_THAN_EQUAL { LTE }     
    | AND { AND }            
    | OR { OR }            
    | ASSIGN { ASSIGN }

%inline uop:
    | MINUS { MINUS }
    | NOT { NOT }   

/** 
    OCaml:

    let a = 1 in
        let x b = a + b in
            let () = print_int x 1

    AST:

    LetIn(
        [BinOp
            (
                ASSIGN, 
                Value(VarID("a")), 
                Value(Const(Int(1))
            )   
        ],
        LetIn(...) 
    )
*/
