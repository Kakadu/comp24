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
%token MUL                  // "*"
%token DIV                  // "/"
%token CONCAT               // "^"
%token EQUAL                // "="
%token NOT_EQUAL            // "!=" || "<>"
%token GREATER_THAN         // ">"
%token GREATER_THAN_EQUAL   // ">="
%token LESS_THAN            // "<"
%token LESS_THAN_EQUAL      // "<"
%token LET_ASSIGNMENT       // "let x = 1"
%token LET_AND              // "let x = 1 and y = 2"
%token AND                  // "AND"
%token OR                   // "OR"
%token NOT                  // "NOT"

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

%token CONCAT
%left PLUS, MINUS
%left MUL, DIV

// --- Parsing ---
%start <expr option> prog
%%

prog : p = expr EOF { p }

expr:
    | op = bop; 

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

tuple: LEFT_PARENTHESIS; els = list(value); RIGHT_PARENTHESIS { els }

list: LEFT_SQ_BRACKET; els = list(value); RIGHT_SQ_BRACKET { els }

value:
    | const = dataType {Const const} 
    | varId = identifier { VarId var }
    | typedVarId = identifier; varType = dataType  {TypedVarID (typedVarId, varType)}
    | WILDCARD {Wildcard}
    | list = list {List list}
    | v1 = value; DOUBLE_COLON; v2 = value { ListConcat (v1, v2) }
    | tuple = tuple {Tuple tuple}
    
%inline bop:
    | PLUS { ADD }                 
    | MINUS { Sub }        
    | MUL { MUL }                  
    | DIV { DIV }                  
    | CONCAT { CONCAT }               
    | EQUAL { EQ }                
    | NOT_EQUAL { NEQ }           
    | GREATER_THAN { GT }       
    | GREATER_THAN_EQUAL { GTE }  
    | LESS_THAN {LT }      
    | LESS_THAN_EQUAL { LTE }     
    | AND { AND }            
    | OR { OR }            
    | LET_ASSIGNMENT { ASSIGN }

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
