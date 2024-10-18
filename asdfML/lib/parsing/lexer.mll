{
  open Menhir
  exception SyntaxError of string
}

let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let digit_nz = ['1'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let id = (alpha | '_') (alpha | digit | '_')* 
let int = digit*  
let bool = "true" | "false"


rule token = parse
  | whitespace { token lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | "()" { UNIT }
  | "let rec" { LETREC }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | "=" { EQ }
  | "==" { EQEQ }
  | "!=" { NE }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GE }
  | "<=" { LE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { IDENT (Lexing.lexeme lexbuf) }
  | ";;" { SS }
  | eof { EOF }
  | _ { raise (SyntaxError ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
