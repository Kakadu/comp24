{
open Lexing
open Parser

exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+

let digit = ['0'-'9']
let frac = '.' digit*
let float = digit* frac
let int = digit+

let sym = ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '?' '/' '[' ']' '{' '}' ',' '.' ' ']
let comment_sym = ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '?' '/' '[' ']' '{' '}' ',' '.' ' ']
let char = ''' sym '''
let string  = '\"' sym* '\"'

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let comment = "(*" comment_sym* "*)"

let unit = '(' ' '* ')'

rule read =
    parse
    | white     { read lexbuf }
    | newline   { new_line lexbuf; read lexbuf }
    | "int"     { INT }
    | "float"   { FLOAT }
    | "char"    { CHAR }
    | "string"  { STRING }
    | "bool"    { BOOL }
    | "if"      { IF } 
    | "then"    { THEN }
    | "else"    { ELSE }
    | "fun"     { FUN } 
    | "let"     { LET }
    | "in"      { IN }
    | "rec"     { REC }
    | "match"   { MATCH }
    | "with"    { WITH }
    | "and"     { LET_AND }              
    | "not"     { NOT }
    | "&&"      { AND }                 
    | "||"      { OR  }
    | "|"       { BAR }              
    | '_'       { WILDCARD }
    | '('       { LEFT_PARENTHESIS }
    | ')'       { RIGHT_PARENTHESIS }
    | '['       { LEFT_SQ_BRACKET }
    | ']'       { RIGHT_SQ_BRACKET }
    | "::"      { DOUBLE_COLON }
    | ':'       { COLON }
    | ';'       { SEMICOLON }
    | ','       { COMMA }
    | '+'       { PLUS }
    | "->"      { ARROW }
    | '-'       { MINUS }
    | '*'       { ASTERISK }
    | '/'       { SLASH }
    | '^'       { CARET }
    | '='       { EQUAL }
    | "=="      { IDENTICAL_EQ }
    | "!="      { NOT_EQUAL}  
    | '>'       { GREATER_THAN }
    | ">="      { GREATER_THAN_EQUAL }
    | '<'       { LESS_THAN }
    | "<="      { LESS_THAN_EQUAL }
    | comment   { read lexbuf }
    | unit      { TYPE_UNIT }
    | int       { TYPE_INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float     { TYPE_FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | char      { TYPE_CHAR (String.get (Lexing.lexeme lexbuf) 1)}
    | string    { let str = Lexing.lexeme lexbuf in
                    TYPE_STRING (String.sub str 1 (String.length str - 2))}
    | "true"    { TYPE_BOOL (true) }
    | "false"   { TYPE_BOOL (false) }
    | id        { IDENTIFIER (Lexing.lexeme lexbuf)}
    | eof       { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }