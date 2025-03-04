(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

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
let bool = ("not" whitespace+)? ("true" | "false")

rule comment = parse
| "*)" { () }
| "(*" { comment lexbuf; comment lexbuf }
| _ { comment lexbuf }

and token = parse
  | "(*" { comment lexbuf; token lexbuf } 
  | "()" { UNIT }
  | "[]" { NIL }
  | "_" { WILDCARD }
  | "let rec" { LETREC }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { BAR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | "=" { EQ }
  | "<>" { LTGT }
  | "==" { EQEQ }
  | "!=" { NE }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GE }
  | "<=" { LE }
  | "&&" { AND }
  | "||" { OR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | whitespace { token lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | bool { 
    BOOL (String.split_on_char ' ' (Lexing.lexeme lexbuf) |> 
    function
    | [ "not"; b ] -> not (bool_of_string b)
    | [ b ] -> bool_of_string b
    | _ -> raise (SyntaxError ("bool: " ^ Lexing.lexeme lexbuf))) 
  }
  (* | "not" { NOT } *)
  | id { IDENT (Lexing.lexeme lexbuf) }
  | "::" { CONS }
  | ":" { COLON }
  | "," { COMMA }
  | ";;" { SS }
  | ";" { SEMI }
  | eof { EOF }
  | _ { raise (SyntaxError ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
