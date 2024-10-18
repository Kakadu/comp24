(* open Lexer
   open Lexing *)

let token_to_string = function
  | Parser.IF -> "IF"
  | Parser.THEN -> "THEN"
  | Parser.ELSE -> "ELSE"
  | Parser.FUN -> "FUN"
  | Parser.LET -> "LET"
  | Parser.IN -> "IN"
  | Parser.REC -> "REC"
  | Parser.MATCH -> "MATCH"
  | Parser.WITH -> "WITH"
  | Parser.WILDCARD -> "WILDCARD"
  | Parser.ARROW -> "ARROW"
  | Parser.COMMA -> "COMMA"
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.COLON -> "COLON"
  | Parser.DOUBLE_COLON -> "DOUBLE_COLON"
  | Parser.BAR -> "BAR"
  | Parser.LEFT_PARENTHESIS -> "LEFT_PARENTHESIS"
  | Parser.RIGHT_PARENTHESIS -> "RIGHT_PARENTHESIS"
  | Parser.LEFT_SQ_BRACKET -> "LEFT_SQ_BRACKET"
  | Parser.RIGHT_SQ_BRACKET -> "RIGHT_SQ_BRACKET"
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.ASTERISK -> "ASTERISK"
  | Parser.SLASH -> "SLASH"
  | Parser.CARET -> "CARET"
  | Parser.EQUAL -> "EQUAL"
  | Parser.NOT_EQUAL -> "NOT_EQUAL"
  | Parser.GREATER_THAN -> "GREATER_THAN"
  | Parser.GREATER_THAN_EQUAL -> "GREATER_THAN_EQUAL"
  | Parser.LESS_THAN -> "LESS_THAN"
  | Parser.LESS_THAN_EQUAL -> "LESS_THAN_EQUAL"
  | Parser.LET_AND -> "LET_AND"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.IDENTIFIER id -> "IDENTIFIER <" ^ id ^ ">"
  | Parser.STRING str -> "STRING <" ^ str ^ ">"
  | Parser.INT i -> "INT <" ^ string_of_int i ^ ">"
  | Parser.FLOAT f -> "FLOAT <" ^ string_of_float f ^ ">"
  | Parser.CHAR c -> "CHAR <" ^ String.make 1 c ^ ">"
  | Parser.BOOL b -> "BOOL <" ^ string_of_bool b ^ ">"
  | Parser.EOF -> "EOF"
;;

let get_tokens code =
  let rec build_list lexbuf =
    match Lexer.read lexbuf with
    | Parser.EOF -> []
    | token -> token :: build_list lexbuf
  in
  build_list (Lexing.from_string code)
;;

let rec print_list f lst =
  match lst with
  | [] -> ()
  | [ x ] -> Printf.printf "%s" (f x)
  | x :: xs ->
    Printf.printf "%s, " (f x);
    print_list f xs
;;

(* let%test _ =
   let real =
   get_tokens
   "let rec sum list =\n\
   \   match list with\n\
   \   | [] -> 0\n\
   \   | head :: tail -> head + sum tail"
   in
   print_list token_to_string real;
   true
   ;; *)

let%test _ = get_tokens "\"rofl\"" = [ Parser.STRING "rofl" ]
let%test _ = get_tokens "228" = [ Parser.INT 228 ]
let%test _ = get_tokens "'a'" = [ Parser.CHAR 'a' ]
let%test _ = get_tokens "228.337" = [ Parser.FLOAT 228.337 ]
let%test _ = get_tokens "true" = [ Parser.BOOL true ]
let%test _ = get_tokens "false" = [ Parser.BOOL false ]
let%test _ = get_tokens "add" = [ Parser.IDENTIFIER "add" ]

let%test _ =
  get_tokens
    "let rec sum list =\n\
    \   match list with\n\
    \   | [] -> 0\n\
    \   | head :: tail -> head + sum tail"
  = [ Parser.LET
    ; Parser.REC
    ; Parser.IDENTIFIER "sum"
    ; Parser.IDENTIFIER "list"
    ; Parser.EQUAL
    ; Parser.MATCH
    ; Parser.IDENTIFIER "list"
    ; Parser.WITH
    ; Parser.BAR
    ; Parser.LEFT_SQ_BRACKET
    ; Parser.RIGHT_SQ_BRACKET
    ; Parser.ARROW
    ; Parser.INT 0
    ; Parser.BAR
    ; Parser.IDENTIFIER "head"
    ; Parser.DOUBLE_COLON
    ; Parser.IDENTIFIER "tail"
    ; Parser.ARROW
    ; Parser.IDENTIFIER "head"
    ; Parser.PLUS
    ; Parser.IDENTIFIER "sum"
    ; Parser.IDENTIFIER "tail"
    ]
;;
