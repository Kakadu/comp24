(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* open TypeTree *)
open StateResultMonad
open StateResultMonad.Syntax
open TypeErrors
(* open InferPattern *)
(* open TypeUtils *)
open CommonFunctions

let infer_expr =

  let rec helper env = function
  | Ast.EConstant c -> return @@ (Substitution.empty, infer_const c)
  | Ast.EIdentifier (Id name) -> infer_id env name
  | Ast.EIfThenElse (cond, branch1, branch2) -> infer_if_then_else env cond branch1 branch2
  | _ -> fail Occurs_check (* !!! *)

  and infer_if_then_else env cond branch1 branch2 =
    let* sub1, ty1 = helper env cond in
    let* sub2, ty2 = helper env branch1 in
    let* sub3, ty3 = match branch2 with
    | Some expr -> helper env expr
    | None ->
      let* fv = fresh_var in 
      return (Substitution.empty, fv)
    in
    let* sub4 = Substitution.unify ty1 (TGround GTBool) in
    let* sub5 = Substitution.unify ty2 ty3 in
    let* sub = Substitution.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
    let ty = Substitution.apply sub ty3 in
    return (sub, ty)

  in

  helper
;;
