%{
    open Ast
%}

// -=-=- Tokens -=-=-

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
%token BAR          // "|"

// braces
%token LEFT_BRACE       // "("
%token RIGHT_BRACE      // ")"
%token LEFT_SQ_BRACE    // "["
%token RIGHT_SQ_BRACE   // "]"

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

// -=-= Priorities -=-=
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

%start <expr option> prog
%%
