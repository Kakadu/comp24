(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Stdlib.Format
open Ast

let kwd ppf s = fprintf ppf "%s" s

let pp_tuple ppf tuple_elems pp_elem pp_delim print_parens =
  if print_parens then fprintf ppf "(";
  let rec helper = function
    | [] -> Utils.unreachable ()
    | [ e ] -> pp_elem e
    | e :: es ->
      pp_elem e;
      pp_delim ();
      helper es
  in
  helper tuple_elems;
  if print_parens then fprintf ppf ")"
;;

let rec pp_typ ppf = function
  | Typ_bool -> fprintf ppf "bool"
  | Typ_int -> fprintf ppf "int"
  | Typ_unit -> fprintf ppf "unit"
  | Typ_var id -> fprintf ppf "%d" id
  | Typ_fun ((Typ_fun (_, _) as farg), y) -> fprintf ppf "(%a) -> %a" pp_typ farg pp_typ y
  | Typ_fun (x, y) -> fprintf ppf "%a -> %a" pp_typ x pp_typ y
  | Typ_list ((Typ_fun _ | Typ_tuple _) as complex) ->
    fprintf ppf "(%a) list" pp_typ complex
  | Typ_list x -> fprintf ppf "%a list" pp_typ x
  | Typ_tuple (fst, snd, rest) ->
    pp_tuple
      ppf
      (fst :: snd :: rest)
      (fun e ->
        match e with
        | Typ_tuple _ | Typ_fun _ -> fprintf ppf "(%a)" pp_typ e
        | e -> fprintf ppf "%a" pp_typ e)
      (fun () -> fprintf ppf " * ")
      false
;;

let rec pp_pat ppf = function
  | Pat_wildcard -> fprintf ppf "_"
  | Pat_var id -> fprintf ppf "%s" id
  | Pat_const (Const_bool b) -> fprintf ppf "%b" b
  | Pat_const (Const_int i) -> fprintf ppf "%i" i
  | Pat_const Const_unit -> fprintf ppf "()"
  | Pat_const Const_nil -> fprintf ppf "[]"
  | Pat_tuple (fst, snd, rest) ->
    pp_tuple
      ppf
      (fst :: snd :: rest)
      (fun e -> fprintf ppf "%a" pp_pat e)
      (fun () -> fprintf ppf ", ")
      true
  | Pat_cons (x, xs) -> fprintf ppf "@[(%a::%a) @]" pp_pat x pp_pat xs
  | Pat_constrained (x, typ) -> fprintf ppf "@[(%a@ : %a) @]" pp_pat x pp_typ typ
;;

let is_infix = function
  | "=" | "<" | "<=" | ">" | ">=" | "+" | "*" | "/" | "-" | "||" | "&&" -> true
  | _ -> false
;;

let rec pp_expr ppf = function
  | Expr_var id when is_infix id -> fprintf ppf "(%s)" id
  | Expr_var id -> fprintf ppf "%s" id
  | Expr_const (Const_bool b) -> fprintf ppf "%b" b
  | Expr_const (Const_int i) -> fprintf ppf "%i" i
  | Expr_const Const_unit -> fprintf ppf "()"
  | Expr_const Const_nil -> fprintf ppf "[]"
  | Expr_cons (x, xs) -> fprintf ppf "@[<2>(%a)::(%a) @]" pp_expr x pp_expr xs
  | Expr_fun ((Pat_var _ as f), arg) ->
    fprintf ppf "@[<2>(%a@ %a@ %a@ %a)@]" kwd "fun" pp_pat f kwd "->" pp_expr arg
  | Expr_fun (f, arg) ->
    fprintf ppf "@[<2>(%a@ (%a)@ %a@ %a) @]" kwd "fun" pp_pat f kwd "->" pp_expr arg
  | Expr_tuple (fst, snd, rest) ->
    pp_tuple
      ppf
      (fst :: snd :: rest)
      (fun e -> fprintf ppf "%a" pp_expr e)
      (fun () -> fprintf ppf ", ")
      true
  | Expr_ite (c, t, e) ->
    fprintf ppf "@[<2>(if %a then @,%a @, else  @,%a)@]" pp_expr c pp_expr t pp_expr e
  | Expr_app ((Expr_var _ as f), ((Expr_var _ | Expr_const _) as arg)) ->
    fprintf ppf "@[<2>%a @ %a@]" pp_expr f pp_expr arg
  | Expr_app (f, ((Expr_var _ | Expr_const _) as arg)) ->
    fprintf ppf "@[<2>(%a) %a@]" pp_expr f pp_expr arg
  | Expr_app ((Expr_var _ | Expr_app _ as f), arg) -> fprintf ppf "@[%a (%a)@]" pp_expr f pp_expr arg
  | Expr_app (f, arg) -> fprintf ppf "@[(%a) (%a) @]" pp_expr f pp_expr arg
  | Expr_let (NonRecursive, (p, e), scope) ->
    fprintf ppf "@[(let %a = %a in @, %a)@]" pp_pat p pp_expr e pp_expr scope
  | Expr_let (Recursive, (p, e), scope) ->
    fprintf ppf "@[(let rec %a = %a in @, %a)@]" pp_pat p pp_expr e pp_expr scope
  | Expr_constrained (e, t) -> fprintf ppf "@[((%a) : %a)@]" pp_expr e pp_typ t
  | Expr_match (e, cases) ->
    fprintf ppf "@[<2>(%a@ %a %a@, @]" kwd "match" pp_expr e kwd "with";
    List.iter (fun (p, e) -> fprintf ppf "@[| %a -> %a@, @]" pp_pat p pp_expr e) cases;
    fprintf ppf ")"
;;

let pp_structure ppf s =
  let item_printer = function
    | Str_value (NonRecursive, [ (id, e) ]) ->
      fprintf ppf "@[let %a@, = @ %a@]@." pp_pat id pp_expr e
    | Str_value (Recursive, (p, e) :: rest) ->
      fprintf ppf "@[let rec %a@  = @ %a@]@." pp_pat p pp_expr e;
      List.iter (fun (p, e) -> fprintf ppf "@[@, and %a = %a@]@." pp_pat p pp_expr e) rest
    | _ -> Utils.unreachable ()
  in
  List.iter item_printer s
;;

let pattern_to_code = Format.asprintf "%a" pp_pat
let expr_to_code = Format.asprintf "%a" pp_expr
let structure_to_code = Format.asprintf "%a" pp_structure
