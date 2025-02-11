(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* Expression parsers description *)

type dispatch =
  { parse_tuple : dispatch -> expression Angstrom.t
  ; parse_list : dispatch -> expression Angstrom.t
  ; parse_list_constructor : dispatch -> expression Angstrom.t
  ; parse_fun : dispatch -> expression Angstrom.t
  ; parse_function : dispatch -> expression Angstrom.t
  ; parse_application : dispatch -> expression Angstrom.t
  ; parse_binary_operation : dispatch -> expression Angstrom.t
  ; parse_match_with : dispatch -> expression Angstrom.t
  ; parse_let_in : dispatch -> expression Angstrom.t
  ; parse_if_then_else : dispatch -> expression Angstrom.t
  ; parse_type_defition : dispatch -> expression Angstrom.t
  ; parse_constant_expr: expression Angstrom.t
  ; parse_identifier_expr: expression Angstrom.t
  ; parse_empty_list_expr: expression Angstrom.t
  }

(* ---------------- *)

(* Helper functions *)

let is_whitespace = function
  | ' ' | '\n' -> true
  | _ -> false
;;

let is_upper_char = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lower_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_digit_char = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_acceptable_service_char = function
  | '_' | '\'' -> true
  | _ -> false
;; 

let is_operator_char = function
  | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '&' | '|' | '^' | '%' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "if"
  | "then"
  | "else"
  | "int"
  | "string"
  | "bool"
  | "rec"
  | "fun"
  | "char"
  | "function"
  | "false"
  | "true"
  | "match"
  | "with"
  | "and"
  | "in" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
;;

(* ---------------- *)

(* Helper parsers *)

let skip_wspace = skip_while is_whitespace
let skip_wspace1 = take_while1 is_whitespace

let parens p = skip_wspace *> char '(' *> skip_wspace *> p <* skip_wspace <* char ')'
let brackets p = skip_wspace *> char '[' *> p <* skip_wspace <* char ']'

let arrow = skip_wspace *> string "->"
let trait = skip_wspace *> string "|"
let tying = skip_wspace *> string "="

(* ---------------- *)

(* Constant parsers *)

let parse_constant =
  fix
  @@ fun self ->
  skip_wspace 
  *> 
  let parse_cint = take_while1 is_digit_char >>| int_of_string >>| fun x -> CInt x in
  let parse_cstring = char '"' *> take_while (( != ) '"') <* char '"' >>| fun x -> CString x in
  let parse_cchar = char '\'' *> any_char <* char '\'' >>| fun x -> CChar x in
  let parse_cbool = string "true" <|> string "false" >>| bool_of_string >>| fun x -> CBool x in
  let parse_cunit = string "()" >>| fun _ -> CUnit in
  parens self <|> choice [ parse_cint; parse_cstring; parse_cchar; parse_cbool; parse_cunit ]
;;

(* ---------------- *)

(* Identifiers *)

let parse_name =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let is_acceptable x = is_lower_char x || is_upper_char x || is_digit_char x || is_acceptable_service_char x in
  parens self <|> take_while1 is_acceptable
;;

let parse_uncapitalized_name =
  let* name = parse_name in
  if (is_lower_char name.[0])
  then return name
  else fail "Syntax error: the some name started with a capital letter when a small letter was expected"
;;

let parse_capitalized_name =
  let* name = parse_name in
  if (is_upper_char name.[0])
  then return name
  else fail "Syntax error: the some name started with a small letter when a capital letter was expected"
;;

let parse_name_started_with_underscore =
  let* name = parse_name in
  if (name.[0] = '_' && String.length name > 1)
  then return name
  else fail "Syntax error: expected '_' at the beggining of the identifier"
;;

let parse_identifier =
  let* name = parse_uncapitalized_name <|> parse_name_started_with_underscore in
  if is_keyword name
  then fail "Syntax error: keywords cannot be used as identifiers"
  else return @@ Id name
;;

let parse_operator =
  let* operator = parens (take_while1 is_operator_char) in
  return @@ Id ("( " ^ operator ^ " )")
;;

(* ---------------- *)
