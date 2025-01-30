(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open InferExpression
open InferProgram

let run_expr_inferencer expr = Result.map snd (run (infer_expr TypeEnv.empty expr))

let run_program_inferencer program = run (infer_program TypeEnv.empty program)
