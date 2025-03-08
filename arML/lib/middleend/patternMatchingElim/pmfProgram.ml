(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open LambdaLifting.Llast
open PmfDeclaration

let rec simplify_ordinary_let_in acc = function
  | [] -> acc
  | hd :: tl ->
    match hd with
    | LDRecursive _ -> simplify_ordinary_let_in (hd :: acc) tl
    | LDOrdinary (case, cases) -> simplify_ordinary_let_in (LDOrdinary (case, []) :: acc) (match cases with
        | [] -> tl
        | hd2 :: tl2 -> LDOrdinary(hd2, tl2) :: tl) 
;;

let eliminate_pm_program start_env program =
  let rec helper env = function
    | [] -> return ([], env)
    | hd :: tl ->
      let* hd, env = eliminate_pm_declaration env hd in
      let* tl, env = helper env tl in
      return @@ (hd @ tl, env)
  in
  helper start_env (List.rev (simplify_ordinary_let_in [] program))
;;
