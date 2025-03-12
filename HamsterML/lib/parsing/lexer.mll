{
open Lexing
open Parser

exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+

let digit = ['0'-'9']
let int = digit+

let sym = ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '?' '/' '[' ']' '{' '}' ',' '.' ' ']
let comment_sym = ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '?' '/' '[' ']' '{' '}' ',' '.' ' ']
let string  = '\"' sym* '\"'

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let poly_name = '\'' id

let comment = "(*" comment_sym* "*)"

let unit = '(' ' '* ')'

let sep = ";;"

rule read =
    parse
    | white     { read lexbuf }
    | newline   { new_line lexbuf; read lexbuf }
    | "int"     { INT }
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
    | "list"    { LIST }
    | poly_name { POLY (Lexing.lexeme lexbuf) }
    | comment   { read lexbuf }
    | sep       { read lexbuf }
    | unit      { TYPE_UNIT }
    | int       { TYPE_INT (int_of_string (Lexing.lexeme lexbuf)) }
    | string    { let str = Lexing.lexeme lexbuf in
                    TYPE_STRING (String.sub str 1 (String.length str - 2))}
    | "true"    { TYPE_BOOL (true) }
    | "false"   { TYPE_BOOL (false) }
    | id        { IDENTIFIER (Lexing.lexeme lexbuf)}
    | eof       { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }