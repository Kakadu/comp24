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

// Param Types
%token INT
%token FLOAT
%token BOOL
%token CHAR
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
    | LET; REC; id = IDENTIFIER; vls = nonempty_list(value); EQUAL; e = expr { Let (Recursive, id, vls, e) }
    | LET; id = IDENTIFIER; vls = nonempty_list(value); EQUAL; e = expr { Let (Nonrecursive, id, vls, e) }
    | LET; id = IDENTIFIER; EQUAL; e = expr {BinOp(ASSIGN, Value(VarId id), e) }
    | LET; exprs = separated_nonempty_list(LET_AND, assign); IN; e = expr { LetIn (exprs, e) }
    | MATCH; expr = expr; WITH; match_cases = nonempty_list(match_case) { Match (expr, match_cases) }
    | FUN; vls = nonempty_list(value); ARROW; e = expr { Fun (vls, e) }
    // TODO: change to make possible to omit else clause
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
    | LEFT_PARENTHESIS; e = expr; RIGHT_PARENTHESIS { e }

assign: 
    | id = IDENTIFIER; EQUAL; e = expr  {BinOp (ASSIGN, Value(VarId id), e)}

%inline match_case: 
    | BAR; v = value; ARROW; e = expr { (v, e) }

dataType:
    | i = TYPE_INT {Int i}
    | f = TYPE_FLOAT {Float f}
    | b = TYPE_BOOL {Bool b}
    | c = TYPE_CHAR {Char c}
    | s = TYPE_STRING {String s}
    | LEFT_PARENTHESIS; data = dataType; RIGHT_PARENTHESIS { data } // (10), ("10"), etc...

identifier:
    | name = IDENTIFIER { name }
    | LEFT_PARENTHESIS; name = identifier; RIGHT_PARENTHESIS { name }

value:
    | const = dataType {Const const} 
    | typedVarId = identifier; COLON; varType = paramType {TypedVarID (typedVarId, varType)}
    | varId = identifier { VarId varId }
    | WILDCARD {Wildcard}
    | tpl = tuple_dt {Tuple tpl}
    | lst = list_dt {List lst}
    | v1 = value ; DOUBLE_COLON; v2 = value  { ListConcat (v1, v2) }
    | LEFT_PARENTHESIS; v = value; RIGHT_PARENTHESIS { v }

%inline tuple_dt: LEFT_PARENTHESIS; val_list = separated_nonempty_list(COMMA, value); RIGHT_PARENTHESIS {val_list}
%inline list_dt: LEFT_SQ_BRACKET; val_list = separated_nonempty_list(SEMICOLON, value); RIGHT_SQ_BRACKET { val_list }


%inline paramType:
    | INT { Int }
    | FLOAT { Float }
    | BOOL { Bool }
    | CHAR { Char }
    | STRING { String }

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
