(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let pp_constant formatter = function
  | CInt i -> Format.fprintf formatter "%d" i
  | CBool false -> Format.fprintf formatter "false"
  | CBool true -> Format.fprintf formatter "true"
  | CNil -> Format.fprintf formatter "[]"
;;

let pp_tuple printer formatter l =
  List.iteri
    (fun i e ->
      if i <> 0 then Format.fprintf formatter ", " else ();
      printer formatter e)
    l
;;

let rec pp_pattern formatter = function
  | PWild -> Format.fprintf formatter "_"
  | PCon (hd, tl) -> Format.fprintf formatter "(%a :: %a)" pp_pattern hd pp_pattern tl
  | PVar (n, _) -> Format.fprintf formatter "%s" n
  | PTuple t -> Format.fprintf formatter "(%a)" (pp_tuple pp_pattern) t
  | PConst c -> pp_constant formatter c
;;

let pp_rec formatter = function
  | Rec -> Format.fprintf formatter "rec"
  | Notrec -> ()
;;

let pp_binop formatter = function
  | Add -> Format.fprintf formatter "+"
  | Sub -> Format.fprintf formatter "-"
  | Mul -> Format.fprintf formatter "*"
  | Div -> Format.fprintf formatter "/"
  | And -> Format.fprintf formatter "&"
  | Or -> Format.fprintf formatter "||"
  | Eq -> Format.fprintf formatter "="
  | Neq -> Format.fprintf formatter "<>"
  | Less -> Format.fprintf formatter "<"
  | Gre -> Format.fprintf formatter ">"
  | Leq -> Format.fprintf formatter "<="
  | Greq -> Format.fprintf formatter ">="
;;

let rec pp_type formatter = function
  | TInt -> Format.fprintf formatter "int"
  | TBool -> Format.fprintf formatter "bool"
  | TUnknown -> Format.fprintf formatter "unknown"
  | TArrow (t1, t2) -> Format.fprintf formatter "(%a -> %a)" pp_type t1 pp_type t2
;;

let rec pp_expression formatter e =
  match e with
  | EConst c -> pp_constant formatter c
  | EVar (n, t) ->
    Format.fprintf formatter "%s" n;
    if t <> TUnknown then Format.fprintf formatter " : %a" pp_type t
  | EFun (p, e) -> Format.fprintf formatter "(fun %a -> %a)" pp_pattern p pp_expression e
  | EApp (e1, e2, t) ->
    Format.fprintf formatter "(%a %a)" pp_expression e1 pp_expression e2;
    if t <> TUnknown then Format.fprintf formatter " : %a" pp_type t
  | EIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      pp_expression
      i
      pp_expression
      t
      pp_expression
      e
  | ELetIn (r, n, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a %s = %a\n  in %a"
      pp_rec
      r
      n
      pp_expression
      e
      pp_expression
      ine
  | ELetPatIn (names, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a = %a\n  in %a"
      pp_pattern
      names
      pp_expression
      e
      pp_expression
      ine
  | ETuple t -> Format.fprintf formatter "(%a)" (pp_tuple pp_expression) t
  | EBinaryOp (op, l, r) ->
    Format.fprintf formatter "(%a %a %a)" pp_expression l pp_binop op pp_expression r
  | EList (head, tail) ->
    (* Рекурсивно выводим список *)
    let rec print_list formatter = function
      | EConst CNil -> Format.fprintf formatter ""
      | EList (h, t) -> Format.fprintf formatter "%a; %a" pp_expression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_expression tail
    in
    Format.fprintf formatter "[%a]" print_list (EList (head, tail))
  | EMatch (m, p) ->
    Format.fprintf formatter "(match %a with" pp_expression m;
    List.iter
      (fun (pat, exp) ->
        Format.fprintf formatter "\n| %a -> %a" pp_pattern pat pp_expression exp)
      p
;;

let pp_bindings formatter bindings =
  match bindings with
  | Let (r, lets_list) ->
    (* Обработка списка связываний *)
    List.iteri
      (fun i let_binding ->
        match let_binding with
        | name, expr ->
          (* Обработка обычного связывания let id = expr *)
          if i = 0
          then
            Format.fprintf
              formatter
              "let %a %a = %a\n"
              pp_rec
              r
              pp_pattern
              name
              pp_expression
              expr
          else Format.fprintf formatter "and %a = %a\n" pp_pattern name pp_expression expr)
      lets_list
  | Expression expr ->
    (* Обработка простого выражения *)
    pp_expression formatter expr;
    Format.fprintf formatter "\n"
;;

let pp_bindings_list formatter bindings_list =
  List.iter (fun bindings -> pp_bindings formatter bindings) bindings_list
;;

let printer bindings_list = Format.asprintf "%a" pp_bindings_list bindings_list
