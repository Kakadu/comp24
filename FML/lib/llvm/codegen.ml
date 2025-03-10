(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Llvm
open Anf_ast

let ctx = global_context ()
let builder = builder ctx

let compile_binop op x y =
  match op with
  | "( + )" -> build_add x y "add" builder
  | "( - )" -> build_sub x y "sub" builder
  | "( * )" -> build_mul x y "mul" builder
  | "( / )" -> build_sdiv x y "div" builder
  | _ -> failwith ("Invalid operator: " ^ op)
;;
