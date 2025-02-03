(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open InferExpression
open InferProgram
open TypeUtils

let binary_ops = [
  ("( + )", TGround GTInt @-> TGround GTInt @-> TGround GTInt);
  ("( - )", TGround GTInt @-> TGround GTInt @-> TGround GTInt);
  ("( * )", TGround GTInt @-> TGround GTInt @-> TGround GTInt);
  ("( / )", TGround GTInt @-> TGround GTInt @-> TGround GTInt);
  ("( % )", TGround GTInt @-> TGround GTInt @-> TGround GTInt);
  ("( && )", TGround GTBool @-> TGround GTBool @-> TGround GTBool);
  ("( || )", TGround GTBool @-> TGround GTBool @-> TGround GTBool);
  ("( = )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
  ("( > )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
  ("( < )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
  ("( >= )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
  ("( <= )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
  ("( != )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
  ("( <> )", TVar (-1) @-> TVar (-1) @-> TGround GTBool);
]

let start_env =
  List.fold_left
    (fun env (op, ty) -> TypeEnv.extend env op (Generalize.generalize TypeEnv.empty ty))
    TypeEnv.empty
    binary_ops
;;

let run_expr_inferencer expr = Result.map snd (run (infer_expr start_env expr))

let run_program_inferencer program = run (infer_program start_env program)
