(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type id = string

type imm_expr =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmNil
  | ImmId of id
  | ImmTuple of imm_expr * imm_expr * imm_expr list
  | ImmList of imm_expr list
[@@deriving show { with_path = false }]

type cexpr =
  | CApp of imm_expr * imm_expr list
  | CIfElse of imm_expr * aexpr * aexpr
  | CImmExpr of imm_expr
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of id * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type fn = Fn of id * id list * aexpr [@@deriving show { with_path = false }]
type program = fn list [@@deriving show { with_path = false }]

let count_bindings (fn : fn) =
  let rec helper_cexpr = function
    | CIfElse (_, aexpr1, aexpr2) -> helper_aexpr aexpr1 + helper_aexpr aexpr2
    | _ -> 0
  and helper_aexpr = function
    | ALet (_, cexpr, aexpr) -> 1 + helper_cexpr cexpr + helper_aexpr aexpr
    | ACExpr cexpr -> helper_cexpr cexpr
  and helper = function
    | Fn (_, _, aexpr) -> helper_aexpr aexpr
  in
  helper fn
;;

open Base
open Format
open Utils

let rec pp_imm_expr fmt = function
  | ImmInt n -> fprintf fmt "%d" n
  | ImmBool b -> fprintf fmt "%b" b
  | ImmId id -> fprintf fmt "%s" id
  | ImmUnit -> fprintf fmt "()"
  | ImmNil -> fprintf fmt "[]"
  | ImmTuple (x1, x2, xs) -> 
    let xs = x1 :: x2 :: xs in
    pp_list ~op:"(" ~cl:")" ~sep:", " fmt pp_imm_expr xs
  | ImmList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_imm_expr xs
;;

let rec pp_cexpr fmt = function
  | CApp (e1, e2) ->
    fprintf fmt "%a" pp_imm_expr e1;
    pp_list ~op:" " ~sep:" " ~cl:"" fmt pp_imm_expr e2
  | CIfElse (c, t, e) ->
    fprintf
      fmt
      "if %a @\n@[<2>then@ %a@] @\n@[<2>else@ %a@]"
      pp_imm_expr
      c
      pp_aexpr
      t
      pp_aexpr
      e
  | CImmExpr e -> pp_imm_expr fmt e

and pp_aexpr fmt = function
  | ALet (id, cexpr, aexpr) ->
    fprintf fmt "@[<2>@,let %s =@ %a @]in@\n%a" id pp_cexpr cexpr pp_aexpr aexpr
  | ACExpr cexpr -> pp_cexpr fmt cexpr
;;

let pp_fn fmt = function
  | Fn (id, args, aexpr) ->
    (match args with
     | [] -> fprintf fmt "@[<2>@,let %s =@ %a@]@." id pp_aexpr aexpr
     | _ ->
       fprintf
         fmt
         "@[<2>@,let %s %s =@ %a@]@."
         id
         (String.concat args ~sep:" ")
         pp_aexpr
         aexpr)
;;

let pp_program fmt = pp_list ~op:"" ~cl:"" ~sep:"" fmt pp_fn
