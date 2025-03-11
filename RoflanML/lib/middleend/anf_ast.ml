(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

type immexpr =
  | ImmConst of const
  | ImmVar of id
  | ImmTuple of immexpr * immexpr * immexpr list
  | ImmList of immexpr list

type cexpr =
  | CImm of immexpr
  | CBranch of immexpr * aexpr * aexpr
  | CApp of immexpr * immexpr list

and aexpr =
  | ALetIn of id * cexpr * aexpr
  | ACExpr of cexpr

type adecl =
  | ADLet of is_rec * id * typed_arg list * aexpr
  | ADMutualLet of (id * typed_arg list * aexpr) list

type aprogram = adecl list

let anf_to_ast =
  let rec imm_to_ast = function
    | ImmConst c -> EConst c
    | ImmVar id -> EVar id
    | ImmTuple (imm1, imm2, imms) ->
      ETuple (imm_to_ast imm1, imm_to_ast imm2, List.map imms ~f:imm_to_ast)
    | ImmList imms -> EList (List.map imms ~f:imm_to_ast)
  in
  let rec cexpr_to_ast = function
    | CImm imm -> imm_to_ast imm
    | CBranch (cond, t, f) -> EBranch (imm_to_ast cond, aexpr_to_ast t, aexpr_to_ast f)
    | CApp (imm, args) ->
      List.fold_left args ~init:(imm_to_ast imm) ~f:(fun acc arg ->
        EApp (acc, imm_to_ast arg))
  and aexpr_to_ast = function
    | ALetIn (id, ce, ae) -> ELetIn (NonRec, id, cexpr_to_ast ce, aexpr_to_ast ae)
    | ACExpr ce -> cexpr_to_ast ce
  in
  let adecl_to_ast = function
    | ADLet (is_rec, id, args, ae) ->
      let e =
        List.fold_right args ~init:(aexpr_to_ast ae) ~f:(fun arg e -> EFun (arg, e))
      in
      DLet (is_rec, id, e)
    | ADMutualLet decls ->
      let decls =
        List.map decls ~f:(fun (id, args, ae) ->
          let e =
            List.fold_right args ~init:(aexpr_to_ast ae) ~f:(fun arg e -> EFun (arg, e))
          in
          id, e)
      in
      DMutualLet (Rec, decls)
  in
  adecl_to_ast
;;
