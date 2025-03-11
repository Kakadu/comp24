(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open TypeUtils
open InferExpression
open InferProgram

let start_env =
  let binary_ops =
    [ "( + )", TGround GTInt @-> TGround GTInt @-> TGround GTInt
    ; "( - )", TGround GTInt @-> TGround GTInt @-> TGround GTInt
    ; "( * )", TGround GTInt @-> TGround GTInt @-> TGround GTInt
    ; "( / )", TGround GTInt @-> TGround GTInt @-> TGround GTInt
    ; "( % )", TGround GTInt @-> TGround GTInt @-> TGround GTInt
    ; "( && )", TGround GTBool @-> TGround GTBool @-> TGround GTBool
    ; "( || )", TGround GTBool @-> TGround GTBool @-> TGround GTBool
    ; "( = )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( == )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( > )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( < )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( >= )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( <= )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( != )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "( <> )", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "logic_and", TGround GTBool @-> TGround GTBool @-> TGround GTBool
    ; "less_than", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ; "equal", TVar (-1) @-> TVar (-1) @-> TGround GTBool
    ]
  in
  let unary_ops =
    [ "U-", TGround GTInt @-> TGround GTInt
    ; "U+", TGround GTInt @-> TGround GTInt
    ; "UNot", TGround GTBool @-> TGround GTBool
    ]
  in
  let stdlib_functions =
    [ "print_int", TGround GTInt @-> TGround GTUnit
    ; "print_bool", TGround GTBool @-> TGround GTUnit
    ; "get_list_length", TList (TVar (-1)) @-> TGround GTInt
    ]
  in
  let unpack_functions =
    [ "unpack_tuple", TVar (-1) @-> TGround GTInt @-> TVar (-2)
    ; "unpack_list_hd", TList (TVar (-1)) @-> TVar (-1)
    ; "unpack_list_tl", TList (TVar (-1)) @-> TList (TVar (-1))
    ; "pattern_matching_failure", TVar (-1)
    ]
  in
  List.fold_left
    (fun env (op, ty) -> TypeEnv.extend env op (Generalize.generalize TypeEnv.empty ty))
    TypeEnv.empty
    (binary_ops @ unary_ops @ unpack_functions @ stdlib_functions)
;;

let run_expr_inferencer expr = Result.map snd (run (infer_expr start_env expr))
let run_program_inferencer program = run (infer_program start_env program)
