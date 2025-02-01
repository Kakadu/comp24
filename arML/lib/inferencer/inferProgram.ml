(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open InferExpression
open InferDeclaration

let infer_program env program =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl ->
      let* acc_env, acc_name_list = acc in
      (match hd with
      | Ast.SDeclaration decl -> 
        let* new_acc_env, new_acc_name_list = infer_declaration acc_env acc_name_list decl in
        helper (return (new_acc_env, new_acc_name_list)) tl
      | Ast.SExpression expr ->
        let* _ = infer_expr env expr in
        acc)
  in
  let* env, names = helper (return (env, [])) program in
  return (env, List.rev names)
;;
