(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

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

let parens p = skip_wspace *> char '(' *> p <* skip_wspace <* char ')'
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
