(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

(** Start parse func *)

let start_parsing parser string = parse_string ~consume:All parser string

(* Base *)

let is_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_bchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "val"
  | "in" -> true
  | _ -> false
;;

let is_op = function
  | ">=" | "<=" | ">" | "<" | "<>" | "=" | "||" | "&&" | "%" | "/" | "*" | "-" | "+" ->
    true
  | _ -> false
;;

let is_type = function
  | "int" | "bool" | "string" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

(* S1mple parsers *)

let parse_white_space = take_while is_whitespace
let parse_white_space1 = take_while1 is_whitespace
let parse_empty e = parse_white_space *> e <* parse_white_space
let wspaces_str str = parse_empty @@ string_ci str
let token s = parse_white_space *> s
let token1 s = parse_white_space1 *> s
let stoken s = parse_white_space *> string s
let stoken1 s = parse_white_space1 *> string s
let brackets p = stoken "(" *> p <* stoken ")"
let square_brackets p = stoken "[" *> p <* stoken "]"
let angle_brackets p = char '<' *> p <* char '>'
let quotes p = stoken "\"" *> p <* stoken "\""
let brackets_or_not p = brackets p <|> p

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(** Const parsers *)

let parse_bool =
  parse_white_space
  *> ((fun _ -> CBool true)
      <$> string "true"
      <|> ((fun _ -> CBool false) <$> string "false"))
;;

let parse_int =
  let ps = token (option "" (stoken "-" <|> stoken "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> CInt (Int.of_string @@ sign ^ digit)) ps pd
;;

let parse_str =
  char '"' *> take_while (fun a -> not (phys_equal a '"'))
  <* char '"'
  >>| fun a -> CString a
;;

(* Var parsers *)

let parse_type =
  parse_white_space
  *> char ':'
  *> parse_white_space
  *> ((fun _ -> TInt)
      <$> string "int"
      <|> ((fun _ -> TBool) <$> string "bool")
      <|> ((fun _ -> TString) <$> string "string"))
;;

let check_var cond =
  parse_white_space *> take_while1 cond
  >>= fun v ->
  if is_keyword v
  then fail ("You can not use \"" ^ v ^ "\" keywords as vars")
  else if Char.is_digit @@ String.get v 0
  then fail "Identifier first symbol is letter, not digit"
  else return v
;;

let parse_var =
  parse_white_space
  *>
  let is_entry = function
    | c -> is_char c || is_underscore c || is_digit c
  in
  check_var is_entry
;;

(** Pattern parsers *)

let parse_pattern_var =
  (fun a -> PVar (a, TUnknown))
  <$> parse_var
  <|> brackets_or_not @@ lift2 (fun a b -> PVar (a, b)) parse_var parse_type
;;

let parse_pconst = (fun v -> PConst v) <$> choice [ parse_int; parse_bool; parse_str ]
let parse_wild = (fun _ -> PWild) <$> stoken "_"
let parse_pattern = parse_wild <|> parse_pconst <|> parse_pattern_var

(** Expression type *)

(* EConst *)

let parse_econst = (fun v -> EConst v) <$> choice [ parse_int; parse_bool; parse_str ]

(* EVar *)

let parse_evar =
  (fun a -> EVar (a, TUnknown))
  <$> parse_var
  <|> brackets @@ lift2 (fun a b -> EVar (a, b)) parse_var parse_type
  <|> lift2 (fun a b -> EVar (a, b)) parse_var parse_type
;;

(* EBinaryOp *)

let parse_op char_op op = stoken char_op *> return (fun e1 e2 -> EBinaryOp (op, e1, e2))
let pmulti = parse_op "*" Mul <|> parse_op "/" Div <|> parse_op "%" Mod
let padd = parse_op "+" Add <|> parse_op "-" Sub

let pcomp =
  parse_op ">=" Greq <|> parse_op ">" Gre <|> parse_op "<=" Leq <|> parse_op "<" Less
;;

let peq = parse_op "=" Eq <|> parse_op "<>" Neq
let pconj = parse_op "&&" And
let pdisj = parse_op "||" Or

let parse_binop x =
  let multi = chainl1 x pmulti in
  let add = chainl1 multi padd in
  let comp = chainl1 add pcomp in
  let eq = chainl1 comp peq in
  let conj = chainl1 eq pconj in
  chainl1 conj pdisj <* parse_white_space
;;

let parse_ebinop = parse_binop @@ (parse_econst <|> parse_evar)

(* EIfElse *)

let parse_eifelse =
  lift3
    (fun e1 e2 e3 -> EIfElse (e1, e2, e3))
    (stoken "if" *> parse_ebinop)
    (stoken "then" *> parse_ebinop)
    (stoken "else" *> parse_ebinop)
;;

(* EFun *)

let constr_efun pl e = List.fold_right ~init:e ~f:(fun p e -> EFun (p, e)) pl
let parse_fun_args = brackets_or_not @@ many1 parse_pattern

let parse_efun =
  brackets_or_not
  @@ lift2 constr_efun (stoken "fun" *> parse_fun_args) (stoken "->" *> parse_ebinop)
;;

(* EApp *)

let parse_eapp =
  lift2
    (fun f args -> List.fold_left ~init:f ~f:(fun f arg -> EApp (f, arg)) args)
    (parse_evar <|> brackets @@ parse_efun)
    (many1 (parse_evar <|> parse_econst))
;;

(* ELetIn *)

let parse_rec =
  parse_white_space *> stoken "let" *> option "false" (stoken1 "rec")
  >>| fun x -> if String.( <> ) x "false" then Rec else Notrec
;;

let parse_rename =
  brackets @@ parse_white_space
  *> choice
       [ string "=" *> return "Eq"
       ; string "<>" *> return "Neq"
       ; string "&&" *> return "And"
       ; string "||" *> return "Or"
       ; string "*" *> return "Mul"
       ; string "/" *> return "Div"
       ; string "%" *> return "Mod"
       ; string "+" *> return "Add"
       ; string "-" *> return "Sub"
       ; string ">=" *> return "Greq"
       ; string ">" *> return "Gre"
       ; string "<=" *> return "Leq"
       ; string "<" *> return "Less"
       ]
  <* parse_white_space
;;

let parse_eletin =
  let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
  lift5
    (fun is_rec name args expr1 expr2 ->
      let expr = constr_efun args expr1 in
      ELetIn (is_rec, name, expr, expr2))
    parse_rec
    (parse_rename <|> parse_var)
    (many parse_pattern)
    (stoken "=" *> parse_ebinop)
    (stoken "in" *> parse_eapp)
;;

(* Expression parsers *)

let parse_expression =
  parse_eletin
  <|> parse_eifelse
  <|> parse_efun
  <|> parse_ebinop
  <|> parse_econst
  <|> parse_evar
  <|> parse_eapp
;;

(** Binding type *)

let parse_let parse =
  lift4
    (fun flag name args body ->
      let body = constr_efun args body in
      Let (flag, name, body))
    parse_rec
    (parse_rename <|> parse_var)
    (parse_white_space *> many (parse_pattern <|> brackets parse_pattern))
    (stoken "=" *> parse)
;;

let expr_main = (fun expr -> Expression expr) <$> parse_expression
let parse_bindings = parse_let @@ parse_expression <|> expr_main
let parse_statements = sep_by (wspaces_str ";;" <|> parse_white_space) parse_bindings
let parse program = parse_string parse_statements program
