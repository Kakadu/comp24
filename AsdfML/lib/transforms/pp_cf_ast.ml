(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Cf_ast
open Format
open Base
open Utils

let rec pp_expr fmt = function
  | CFConst c -> fprintf fmt "%a" Pp_ast.pp_const c
  | CFVar v -> fprintf fmt "%s" v
  | CFApp (e1, e2) ->
    (match e1 with
     | CFApp (_, _) ->
       let rec pp_rest fmt = function
         | CFApp (e1, e2) -> fprintf fmt "%a %a" pp_rest e1 pp_expr e2
         | e -> fprintf fmt "(%a" pp_expr e
       in
       fprintf fmt "%a %a)" pp_rest e1 pp_expr e2
     | _ -> fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2)
  | CFIfElse (c, t, e) ->
    fprintf fmt "if %a then %a else %a" pp_expr c pp_expr t pp_expr e
  | CFFun (p, e) ->
    fprintf fmt "(fun %s -> %a)" (p |> String.concat ~sep:" ") pp_expr e
  | CFLetIn (id, e1, e2) -> fprintf fmt "let %s = %a in %a" id pp_expr e1 pp_expr e2
  | CFTuple xs -> pp_list ~sep:", " fmt pp_expr xs
  | CFMatch (e, pe_list) ->
    (* fprintf fmt "match %a with\n" pp_expr e;
       pp_print_list
       ~pp_sep:Format.pp_print_newline
       (fun fmt (p, e) -> fprintf fmt "| %a -> %a" pp_pattern p pp_expr e)
       fmt
       pe_list *)
    failwith ""
  | CFList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_expr xs

and pp_definition fmt = function
  | CFDLet (id, e) -> fprintf fmt "let %s = %a\n" id pp_expr e
;;
