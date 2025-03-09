(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Cc_ast
open String

let rec pp_cc_expr expr =
  match expr with
  | CId (id) -> id
  | CConst(c) -> Pprint_ast.pp_const c
  | CNot (e) ->            concat "" ["not "; pp_cc_expr e]
  | COr (e1, e2) ->        concat "" ["("; pp_cc_expr e1; "||"; pp_cc_expr e2; ")"]
  | CAnd (e1, e2) ->       concat "" ["("; pp_cc_expr e1; "&&"; pp_cc_expr e2; ")"]
  | CEq (e1, e2) ->        concat "" ["("; pp_cc_expr e1; "="; pp_cc_expr e2; ")"]
  | CGt (e1, e2) ->        concat "" ["("; pp_cc_expr e1; ">"; pp_cc_expr e2; ")"]
  | CLt (e1, e2) ->        concat "" ["("; pp_cc_expr e1; "<"; pp_cc_expr e2; ")"]
  | CGte (e1, e2) ->       concat "" ["("; pp_cc_expr e1; ">="; pp_cc_expr e2; ")"]
  | CLte (e1, e2) ->       concat "" ["("; pp_cc_expr e1; "<="; pp_cc_expr e2; ")"]
  | CAdd (e1, e2) ->       concat "" ["("; pp_cc_expr e1; "+"; pp_cc_expr e2; ")"]
  | CSub (e1, e2) ->       concat "" ["("; pp_cc_expr e1; "-"; pp_cc_expr e2; ")"]
  | CMul (e1, e2) ->       concat "" ["("; pp_cc_expr e1; "*"; pp_cc_expr e2; ")"]
  | CDiv (e1, e2) ->       concat "" ["("; pp_cc_expr e1; "/"; pp_cc_expr e2; ")"]
  | CIf (e1, e2, e3) ->    concat "" ["if ("; pp_cc_expr e1; ") then ("; pp_cc_expr e2; ") else ("; pp_cc_expr e3; ")"]
  | CLet (d, e2) ->        concat "" ["(let "; Pprint_ast.expr_of_decl d; "="; pp_cc_expr e2; ")"]
  | CLetIn (d, e2, e3) ->  concat "" ["(let "; Pprint_ast.expr_of_decl d; "="; pp_cc_expr e2; " in "; pp_cc_expr e3; ")"]
  | CFun (args, e) ->      concat "" ["(fun ";  concat " " args; "->"; pp_cc_expr e; ")"]
  | CApp (e, args) ->      concat "" ["("; pp_cc_expr e; " "; concat " " (List.map pp_cc_expr args); ")"]
;;

