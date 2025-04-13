(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ll_ast
open String
open Pprint_ast

let rec pp_ll_expr expr =
  match expr with
  | LId id -> id
  | LConst c -> pp_const c
  | LNot e -> concat "" [ "not "; pp_ll_expr e ]
  | LOr (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "||"; pp_ll_expr e2; ")" ]
  | LAnd (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "&&"; pp_ll_expr e2; ")" ]
  | LEq (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "="; pp_ll_expr e2; ")" ]
  | LGt (e1, e2) -> concat "" [ "("; pp_ll_expr e1; ">"; pp_ll_expr e2; ")" ]
  | LLt (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "<"; pp_ll_expr e2; ")" ]
  | LGte (e1, e2) -> concat "" [ "("; pp_ll_expr e1; ">="; pp_ll_expr e2; ")" ]
  | LLte (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "<="; pp_ll_expr e2; ")" ]
  | LAdd (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "+"; pp_ll_expr e2; ")" ]
  | LSub (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "-"; pp_ll_expr e2; ")" ]
  | LMul (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "*"; pp_ll_expr e2; ")" ]
  | LDiv (e1, e2) -> concat "" [ "("; pp_ll_expr e1; "/"; pp_ll_expr e2; ")" ]
  | LIf (e1, e2, e3) ->
    concat
      ""
      [ "if ("; pp_ll_expr e1; ") then ("; pp_ll_expr e2; ") else ("; pp_ll_expr e3; ")" ]
  | LApp (e, args) ->
    concat "" [ "{"; pp_ll_expr e; " "; concat " " (List.map pp_ll_expr args); "}" ]
  | LIn (id, e1, e2) ->
    concat "" [ "let "; id; " = ("; pp_ll_expr e1; " in "; pp_ll_expr e2; ")" ]
;;

let pp_gl_expr = function
  | LFun (id, args, e) ->
    concat "" [ "(fun "; id; "("; concat " " args; ")->("; pp_ll_expr e; "))" ]
;;
