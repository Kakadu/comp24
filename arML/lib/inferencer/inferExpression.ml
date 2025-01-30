(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* open TypeTree *)
open StateResultMonad
(* open StateResultMonad.Syntax *)
open TypeErrors
(* open InferPattern *)
(* open TypeUtils *)
open CommonFunctions

let infer_expr =

  (* let rec helper env = function *)
  let helper env = function
  | Ast.EConstant c -> return @@ (Substitution.empty, infer_const c)
  | Ast.EIdentifier (Id name) -> infer_id env name
  | _ -> fail Occurs_check (* !!! *)
  in

  helper
;;
