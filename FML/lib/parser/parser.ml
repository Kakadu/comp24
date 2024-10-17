(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Angstrom
open Ast

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_keyword = function
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "match"
  | "rec"
  | "then"
  | "true"
  | "with" -> true
  | _ -> false
;;

let skip_wspace = skip_while is_whitespace
let parens p = skip_wspace *> char '(' *> skip_wspace *> p <* skip_wspace <* char ')'
let sqr_br p = skip_wspace *> char '[' *> skip_wspace *> p <* skip_wspace <* char ']'

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let parse_name =
  skip_wspace *> take_while1 (fun c -> is_upper c || is_lower c || c = '_' || c = '\'')
;;

let parse_identifier constr =
  parse_name
  >>= fun name ->
  if ((not @@ is_keyword name) && is_lower name.[0]) || name.[0] = '_'
  then return @@ constr name
  else fail "Syntax error: invalid identifier name"
;;

let parse_int =
  skip_wspace *> take_while1 is_digit <* skip_wspace >>| int_of_string >>| cint
;;

let parse_bool =
  skip_wspace *> string "true" <|> string "false" >>| bool_of_string >>| cbool
;;

let parse_const constr = choice [ parse_int; parse_bool ] >>| constr

(* Type annotations parsers *)
let parse_primitive_type =
  skip_wspace
  *> choice
       [ string "int" *> return AInt
       ; string "bool" *> return ABool
       ; char '(' *> skip_wspace *> char ')' *> return AUnit
       ]
;;

let parse_list_type p_type =
  p_type <* skip_wspace <* string "list" >>= fun l -> return @@ AList l
;;

let parse_tuple_type p_type =
  lift2
    (fun h tl -> ATuple (h :: tl))
    p_type
    (many1 (skip_wspace *> string "*" *> p_type))
;;

let parse_function_type p_type =
  let fun_helper =
    skip_wspace *> string "->" *> return (fun arg ret -> AFunction (arg, ret))
  in
  chainr1 p_type fun_helper
;;

let parse_type =
  let typ =
    fix @@ fun self -> choice [ parens self; parse_primitive_type; parse_list_type self ]
  in
  let typ = parse_tuple_type typ <|> typ in
  parse_function_type typ <|> typ
;;

(* ------------------------ *)

(* Pattern parsers*)
let parse_pany = skip_wspace *> char '_' >>| pany
let parse_pidentifier = parse_identifier pident
let parse_pconst = parse_const pconst
let parse_pnill = sqr_br skip_wspace >>| pnill

let parse_pcons p_pattern =
  let cons_helper = skip_wspace *> string "::" *> return pcons in
  chainl1 p_pattern cons_helper
;;

let parse_ptuple p_pattern =
  parens
  @@ lift2
       (fun h tl -> ptuple (h :: tl))
       p_pattern
       (many1 (skip_wspace *> string "," *> p_pattern))
;;

let parse_pattern_wout_type =
  fix
  @@ fun self ->
  let patt =
    choice
      [ parens self
      ; parse_pidentifier
      ; parse_pany
      ; parse_pconst
      ; parse_pnill
      ; parse_ptuple self
      ]
  in
  parse_pcons patt <|> patt
;;

let parse_pattern_with_type =
  lift2
    pconstraint
    (skip_wspace *> char '(' *> parse_pattern_wout_type)
    (skip_wspace *> char ':' *> parse_type <* char ')')
;;

let parse_pattern = parse_pattern_with_type <|> parse_pattern_wout_type

(* ------------------------- *)

(* Expressions parsers *)

let parse_econst = parse_const econst
let parse_identifier = parse_identifier eidentifier

let parse_etuple p_expr =
  parens
  @@ lift2
       (fun h tl -> etuple (h :: tl))
       p_expr
       (many1 (skip_wspace *> string "," *> p_expr))
;;
