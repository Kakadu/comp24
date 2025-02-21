(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type llexpr =
  | LLConst of const (** Consts *)
  | LLVar of id (** Variables with their names *)
  | LLTuple of llexpr * llexpr * llexpr list
  (** Tuples of 2 or more elements, separated by ',' *)
  | LLList of llexpr list (** Lists [1; 2; 3], ... *)
  | LLBranch of llexpr * llexpr * llexpr (** if [cond] then [a] else [b] *)
  | LLMatch of llexpr * (pattern * llexpr) list (** match [x] with | [p1] -> [e1] | ... *)
  | LLLetIn of id * llexpr * llexpr (** let x = 1, only non-function bindings *)
  | LLApp of llexpr * llexpr (** Application f x y z *)

type lldecl = LLDLet of is_rec * id * typed_arg list * llexpr
type llprogram = lldecl list

let ll_to_ast =
  let rec expr_to_ast = function
    | LLConst c -> EConst c
    | LLVar x -> EVar x
    | LLTuple (e1, e2, es) ->
      ETuple (expr_to_ast e1, expr_to_ast e2, List.map es ~f:expr_to_ast)
    | LLList es -> EList (List.map es ~f:expr_to_ast)
    | LLBranch (cond, t, f) -> EBranch (expr_to_ast cond, expr_to_ast t, expr_to_ast f)
    | LLMatch (e, cases) ->
      EMatch (expr_to_ast e, List.map cases ~f:(fun (pat, e) -> pat, expr_to_ast e))
    | LLLetIn (id, e1, e2) -> ELetIn (NonRec, id, expr_to_ast e1, expr_to_ast e2)
    | LLApp (e1, e2) -> EApp (expr_to_ast e1, expr_to_ast e2)
  in
  let decl_to_ast = function
    | LLDLet (is_rec, id, args, e) ->
      let e =
        List.fold_right args ~init:(expr_to_ast e) ~f:(fun arg e -> EFun (arg, e))
      in
      DLet (is_rec, id, e)
  in
  decl_to_ast
;;
