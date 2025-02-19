(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open InferDeclaration

let infer_program env program =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl ->
      let* acc_env, acc_name_list = acc in
      let* new_acc_env, new_acc_name_list = infer_declaration acc_env acc_name_list hd in
      helper (return (new_acc_env, new_acc_name_list)) tl
  in
  let* env, names = helper (return (env, [])) program in
  return (env, List.rev names)
;;
