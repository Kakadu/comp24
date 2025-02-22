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
  | "and"
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
let skip_wspace1 = take_while1 is_whitespace *> return ()
let parens p = skip_wspace *> char '(' *> skip_wspace *> p <* skip_wspace <* char ')'
let sqr_br p = skip_wspace *> char '[' *> skip_wspace *> p <* skip_wspace <* char ']'
let token s = skip_wspace *> string s
let keyword s = skip_wspace *> string s <* skip_wspace1

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

let parse_bool = token "true" <|> token "false" >>| bool_of_string >>| cbool
let parse_const constr = choice [ parse_int; parse_bool ] >>| constr

(* Type annotations parsers *)
let parse_primitive_type =
  choice
    [ token "int" *> return AInt
    ; token "bool" *> return ABool
    ; token "(" *> token ")" *> return AUnit
    ]
;;

let parse_list_type p_type = p_type <* token "list" >>= fun l -> return @@ AList l

let parse_tuple_type p_type =
  lift2 (fun h tl -> ATuple (h :: tl)) p_type (many1 (token "*" *> p_type))
;;

let parse_function_type p_type =
  let fun_helper = token "->" *> return (fun arg ret -> AFunction (arg, ret)) in
  chainr1 p_type fun_helper
;;

let parse_type =
  let typ =
    fix @@ fun self -> choice [ parens self; parse_primitive_type; parse_list_type self ]
  in
  let typ = parse_tuple_type typ <|> typ in
  parse_function_type typ <|> typ
;;

let parse_operators =
  parens
  @@ (choice
        [ token "+" *> return "( + )"
        ; token "-" *> return "( - )"
        ; token ">=" *> return "( >= )"
        ; token ">" *> return "( > )"
        ; token "<=" *> return "( <= )"
        ; token "<" *> return "( < )"
        ; token "==" *> return "( == )"
        ; token "=" *> return "( = )"
        ; token "<>" *> return "( <> )"
        ; token "!=" *> return "( != )"
        ; token "&&" *> return "( && )"
        ; token "||" *> return "( || )"
        ; token "*" *> return "( * )"
        ; token "/" *> return "( / )"
        ]
      <* skip_wspace)
  <* skip_wspace
;;

(* ------------------------ *)

(* Pattern parsers*)
let parse_pany = token "_" *> skip_wspace1 >>| pany
let parse_punit = token "(" *> token ")" >>| punit

let parse_pidentifier =
  parse_operators >>| (fun x -> PIdentifier x) <|> parse_identifier pident
;;

let parse_pconst = parse_const pconst
let parse_pnill = sqr_br skip_wspace >>| pnill

let parse_pcons p_pattern =
  let cons_helper = token "::" *> return pcons in
  chainr1 p_pattern cons_helper
;;

let parse_ptuple p_pattern =
  parens
  @@ lift2 (fun h tl -> ptuple (h :: tl)) p_pattern (many1 (token "," *> p_pattern))
;;

let parse_pattern_wout_type =
  fix
  @@ fun self ->
  let patt =
    choice
      [ parens self
      ; parse_pany
      ; parse_punit
      ; parse_pconst
      ; parse_pnill
      ; parse_pidentifier
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

let parse_pattern = parse_pattern_wout_type <|> parse_pattern_with_type

(* ------------------------- *)

(* Binary operations parsers *)

let bin_op op func =
  token op
  *> return (fun op1 op2 ->
    EApplication (func, op1) |> fun app1 -> EApplication (app1, op2))
;;

let add = bin_op "+" (EIdentifier "( + )")
let sub = bin_op "-" (EIdentifier "( - )")
let mul = bin_op "*" (EIdentifier "( * )")
let div = bin_op "/" (EIdentifier "( / )")
let eq = bin_op "=" (EIdentifier "( = )")
let eqq = bin_op "==" (EIdentifier "( == )")
let neq = bin_op "!=" (EIdentifier "( != )") <|> bin_op "<>" (EIdentifier "( <> )")
let gt = bin_op ">" (EIdentifier "( > )")
let gte = bin_op ">=" (EIdentifier "( >= )")
let lt = bin_op "<" (EIdentifier "( < )")
let lte = bin_op "<=" (EIdentifier "( <= )")
let and_ = bin_op "&&" (EIdentifier "( && )")
let or_ = bin_op "||" (EIdentifier "( || )")

(* Expressions parsers *)

let parse_econst = parse_const econst
let parse_identifier = parse_identifier eidentifier
let parse_eunit = token "(" *> token ")" >>| fun _ -> EUnit

let parse_etuple p_expr =
  parens @@ lift2 (fun h tl -> etuple (h :: tl)) p_expr (many1 (token "," *> p_expr))
;;

let parse_efun p_expr =
  let* args = token "fun" *> many1 parse_pattern in
  let* expr = token "->" *> p_expr in
  match List.rev args with
  | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
  | _ -> fail "Syntax errror"
;;

(* let rec parse_bundle pexpr =
  let expr_with_pattern =
    parse_pattern
    >>= fun pat -> parse_bundle pexpr <|> token "=" *> pexpr >>| fun e -> EFun (pat, e)
  in
  expr_with_pattern
;; *)

let parse_eif arg =
  skip_wspace
  *> lift3
       (fun i t e -> EIf (i, t, e))
       (token "if" *> arg)
       (token "then" *> arg)
       (token "else" *> arg)
;;

let parse_ematch pexpr =
  let pcase pexpr =
    lift2
      (fun p e -> p, e)
      (parse_pattern <|> token "|" *> parse_pattern)
      (token "->" *> pexpr)
  in
  lift2
    (fun expr cases -> EMatch (expr, cases))
    (token "match" *> pexpr <* token "with")
    (many1 (pcase pexpr))
;;

let parse_elist arg =
  token "["
  *> fix (fun x ->
    choice
      [ (arg <* skip_wspace <* char ']' >>| fun expr -> ECons (expr, ENill))
      ; (arg <* skip_wspace <* char ';' >>= fun expr -> x >>| fun l -> ECons (expr, l))
      ])
;;

let parse_cons = token "::" *> return (fun e1 e2 -> ECons (e1, e2))
let parse_enill = token "[]" >>| fun _ -> ENill
let parse_elist arg = parse_enill <|> parse_elist arg

let parse_let pexpr =
  let* rec_flag = keyword "let" *> (keyword "rec" *> return Rec <|> return NoRec) in
  let* decl = parse_pattern in
  let* args = many parse_pattern in
  let* expr = token "=" *> pexpr in
  let* expression =
    match List.rev args with
    | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
    | _ -> return expr
  in
  let* in_expression = keyword "in" *> pexpr in
  return @@ ELetIn (rec_flag, decl, expression, in_expression)
;;

let parse_expr_with_type pexpr =
  parens @@ lift2 econstraint pexpr (skip_wspace *> char ':' *> parse_type)
;;

let parse_expr =
  fix
  @@ fun expr ->
  let expr =
    choice
      [ parse_expr_with_type expr
      ; parens expr
      ; parse_etuple expr
      ; parse_efun expr
      ; parse_ematch expr
      ; parse_elist expr
      ; parse_identifier
      ; parse_econst
      ; parse_eif expr
      ; parse_eunit
      ; parse_let expr
      ]
  in
  let apply =
    let rec build_application f args =
      match args with
      | [] -> f
      | h :: t -> build_application (EApplication (f, h)) t
    in
    lift2 build_application expr (many (skip_wspace *> expr))
  in
  let expr = chainl1 apply (mul <|> div) in
  let expr = chainl1 expr (add <|> sub) in
  let expr = chainr1 expr parse_cons in
  let expr = chainl1 expr (choice [ lte; lt; gte; gt; eqq; eq; neq; or_; and_ ]) in
  parse_expr_with_type expr <|> expr
;;

(* ------------------------- *)

let parse_declaration =
  let* rec_flag = keyword "rec" *> return Rec <|> return NoRec in
  let* decl = parse_pattern in
  let* args = many parse_pattern in
  let* expr = token "=" *> parse_expr in
  let* expression =
    match List.rev args with
    | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
    | _ -> return expr
  in
  match decl with
  | PIdentifier _ -> return @@ ddeclaration rec_flag decl expression
  | _ when List.length args <> 0 -> fail "Syntax error"
  | _ -> return @@ ddeclaration rec_flag decl expression
;;

let parse_single_declaration =
  keyword "let" *> parse_declaration >>| fun x -> SingleDecl x
;;

let parse_mutable_rec_declaration =
  keyword "let" *> parse_declaration
  >>= fun first_decl ->
  many1 (keyword "and" *> parse_declaration)
  >>= fun lst -> return @@ MutableRecDecl (first_decl :: lst)
;;

let parse_program = choice [ parse_mutable_rec_declaration; parse_single_declaration ]

(* ------------------ *)

let del = token ";;" *> skip_wspace <|> skip_wspace
let parse input = parse_string ~consume:All (many1 (parse_program <* del)) input
