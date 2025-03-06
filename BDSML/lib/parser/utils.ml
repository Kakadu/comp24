(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom

let skip_ws = skip_while Char.is_whitespace *> return ()

let skip_comments =
  skip_ws *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let ws = many skip_comments *> skip_ws
let check_char c = ws *> char c
let check_string s = ws *> string s

let ws1 =
  let skip_ws1 = take_while1 Char.is_whitespace *> return () in
  (skip_ws1 *> many skip_comments <|> many1 skip_comments) *> return ()
;;

let remove_parents x = check_char '(' *> ws *> x <* check_char ')'
let remove_square_brackets x = check_char '[' *> x <* check_char ']'

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with"
  | "|" -> true
  | _ -> false
;;

let parse_lowercase_ident =
  let* parse_first =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let parse_rest =
    let is_rest = function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false
    in
    match parse_first with
    | '_' -> take_while1 is_rest
    | _ -> take_while is_rest
  in
  lift2 String.( ^ ) (return (Char.escaped parse_first)) parse_rest
  >>= fun name ->
  if not (is_keyword name)
  then return name
  else fail (name ^ " keyword can't be used as ident")
;;

let parse_capitalized_ident =
  let parse_first =
    satisfy (function
      | 'A' .. 'Z' -> true
      | _ -> false)
    >>| String.of_char
  in
  let parse_rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  lift2 String.( ^ ) parse_first parse_rest
;;

let parse_ident_name = ws *> parse_lowercase_ident
let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let choice_pass_prev parsers_list prev =
  choice @@ List.map ~f:(fun el -> el prev) parsers_list
;;

let parse_by_priority priority init =
  let rec parse_priority prev = function
    | h :: tl -> parse_priority (h prev) tl
    | [] -> prev
  in
  parse_priority init priority
;;
