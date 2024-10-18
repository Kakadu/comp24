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

let sym = ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '?' '/' '[' ']' '{' '}' ',' '.' ' ']
let char = ''' sym '''
let string  = '\"' sym* '\"'

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
    parse
    | white     { read lexbuf }
    | newline   { new_line lexbuf; read lexbuf }
    | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | char      { CHAR (String.get (Lexing.lexeme lexbuf) 1)}
    | string    { let str = Lexing.lexeme lexbuf in
                    STRING (String.sub str 1 (String.length str - 2))}
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
    | "not"     { NOT }
    | "&&"      { AND }                 
    | "||"      { OR  }
    | "|"       { BAR }              
    | '_'       { WILDCARD }
    | '('       { LEFT_PARENTHESIS }
    | ')'       { LEFT_PARENTHESIS }
    | '['       { LEFT_SQ_BRACKET }
    | ']'       { RIGHT_SQ_BRACKET }
    | "::"      { DOUBLE_COLON }
    | ':'       { COLON }
    | ','       { COMMA }
    | '+'       { PLUS }
    | "->"      { ARROW }
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
    | id        { IDENTIFIER (Lexing.lexeme lexbuf)}
    | eof      { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }