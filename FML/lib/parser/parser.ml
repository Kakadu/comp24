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
let token s = skip_wspace *> string s

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

let identifier =
  parse_name
  >>= fun name ->
  if ((not @@ is_keyword name) && is_lower name.[0]) || name.[0] = '_'
  then return name
  else fail "Syntax error: invalid identifier name"
;;

let parse_int =
  skip_wspace *> take_while1 is_digit <* skip_wspace >>| int_of_string >>| cint
;;

let parse_bool =
  token "true" <|> string "false" >>| bool_of_string >>| cbool
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
    (many1 (token "*" *> p_type))
;;

let parse_function_type p_type =
  let fun_helper =
    token "->" *> return (fun arg ret -> AFunction (arg, ret))
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
  let cons_helper = token "::" *> return pcons in
  chainl1 p_pattern cons_helper
;;

let parse_ptuple p_pattern =
  parens
  @@ lift2
       (fun h tl -> ptuple (h :: tl))
       p_pattern
       (many1 (token "," *> p_pattern))
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
       (many1 (token "," *> p_expr))
;;

let parse_efun p_expr =
  let* args = token "fun" *> many1 parse_pattern in
  let* expr = token "->" *> p_expr in
  match List.rev args with
  | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
  | _ -> fail "Syntax errror"
;;

let rec parse_bundle pexpr =
  let pattern_expr = identifier >>| fun var -> PIdentifier var in
  let expr_with_pattern =
    pattern_expr
    >>= fun pat -> parse_bundle pexpr <|> token "=" *> pexpr >>| fun e -> EFun (pat, e)
  in
  expr_with_pattern <|> token "fun" *> parse_efun pexpr
;;

let parse_ebinop chain1 e parse_binop = chain1 e (parse_binop >>| fun op e1 e2 -> EBinaryOperation (op, e1, e2))
let parse_lbinop = parse_ebinop chainl1

let parse_eif arg =
  skip_while is_whitespace *>
    (lift3
       (fun i t e -> EIf (i, t, e))
       (token "if" *> arg)
       (token "then" *> arg)
       (token "else" *> arg))
;;

let parse_match pexpr =
  let pcase patterns pexpr =
    lift2 (fun p e -> p, e) (token "|" *> patterns) (token "->" *> pexpr)
  in
  lift2
    (fun expr cases -> EMatch (expr, cases))
    (token "match" *> pexpr <* token "with")
    (many1 (pcase parse_pattern pexpr))
;;

let parse_let pexpr =
  token "let"
  *> lift4
       (fun r id e1 e2 -> ELetIn (r, id, e1, e2))
       (token "rec" *> return Rec <|> return NoRec)
       (skip_while is_whitespace *> (token "()") <|> identifier >>| fun var -> PIdentifier var)
       (skip_while is_whitespace *> (token "=") *> pexpr <|> parse_bundle pexpr)
       (token "in" *> pexpr)
;;

let parse_expr =
  fix
  @@ fun self ->
  choice
    [ parens self; parse_econst; parse_identifier; parse_etuple self; parse_efun self ]
;;

(* ------------------------- *)

(* Expressions bin_op *)

let add = string "+" *> return Add
let sub = string "-" *> return Sub
let div = string "/" *> return Div
let eq = string "=" *> return Eq
let neq = string "!=" *> return NEq <|> string "<>" *> return NEq
let gt = string ">" *> return Gt
let gte = string ">=" *> return Gte
let lt = string "<" *> return Lt
let lte = string "<=" *> return Lte
let and_ = string "&&" *> return And
let or_ = string "||" *> return Or

(* Declaration parser *)
let parse_declaration =
  skip_wspace
  *> string "let"
  *> lift3
       ddeclaration
       (skip_wspace *> string "rec" *> return Rec <|> return NoRec)
       parse_pattern
       (let* args = skip_wspace *> many parse_pattern in
        let* expr = skip_wspace *> string "=" *> parse_expr in
        match List.rev args with
        | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
        | _ -> return expr)
;;

(* ------------------ *)

let parse input =
  parse_string
    ~consume:All
    (sep_by (string ";;" <|> string "\n") parse_declaration
     <* option "" (Angstrom.string ";;" <|> Angstrom.string "\n"))
    input
;;