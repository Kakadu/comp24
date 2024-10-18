open! Base
open Angstrom

let skip_ws = skip_while Char.is_whitespace *> return ()

let skip_comments =
  skip_ws *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let ws = many skip_comments *> skip_ws

let ws1 =
  let skip_ws1 = take_while1 Char.is_whitespace *> return () in
  (skip_ws1 *> many skip_comments <|> many1 skip_comments) *> return ()
;;

let is_keyword = function
  | "and"
  | "else"
  | "exception"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "try"
  | "rec"
  | "then"
  | "true"
  | "with"
  | "type"
  | "|" -> true
  | _ -> false
;;

let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' -> true
  | _ -> false
;;

let is_operator_char = function
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' -> true
  | _ as x when is_core_operator_char x -> true
  | _ -> false
;;
