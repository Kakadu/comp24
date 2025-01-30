(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree
open StateResultMonad
open StateResultMonad.Syntax

let fresh_var = fresh >>| fun name -> TVar name

let infer_const = function
  | Ast.CInt _ -> TGround GTInt
  | Ast.CBool _ -> TGround GTBool
  | Ast.CChar _ -> TGround GTChar
  | Ast.CString _ -> TGround GTString
  | Ast.CUnit -> TGround GTUnit
;;

let infer_id env id =
  (* '_' - reserved for expressions whose result is not important to us. *)
  match id with
  | "_" ->
    let* fv = fresh_var in
    return (Substitution.empty, fv)
  | _ -> TypeEnv.lookup_env env id
;;
