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
let int = ('-' | '+')? digit+
let char = ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '?' '/' '\' '[' ']' '{' '}' ',' '.']
let string  = '"' char* '"'

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
    parse
    | white     { read lexbuf }
    | newline   { next_line lexbuf; read lexbuf }
    | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | id        { IDENTIFIER (Lexing.lexeme lexbuf)}
    | char      { CHAR (get (Lexing.lexeme lexbuf) 0)}
    | string    { STRING (Lexing.lexeme lexbuf)}
    | "true"    { BOOL (true) }
    | "false"   { BOOL (false) }
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
    | "&&"      { AND }                 
    | "||"      { OR  }              
    | "not"     { NOT }
    | '_'       { WILDCARD }
    | '('       { LEFT_PARENTHESIS }
    | ')'       { LEFT_PARENTHESIS }
    | '['       { LEFT_SQ_BRACKET }
    | ']'       { RIGHT_SQ_BRACKET }
    | ':'       { COLON }
    | "::"      { DOUBLE_COLON }
    | ','       { COMMA }
    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { ASTERISK }
    | '/'       { SLASH }
    | '^'       { CARET }
    | '='       { EQUAL }
    | "!="      { NOT_EQUAL}  
    | '>'       { GREATER_THAN }
    | ">="      { GREATER_THAN_EQUAL }
    | '<'       { LESS_THAN }
    | "<="      { LESS_THAN_EQUAL }
    | eof      { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }