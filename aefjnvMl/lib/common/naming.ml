(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let alpha_prefix = "ac"
let cc_prefix = "cc"
let me_prefix = "me"
let ll_prefix = "ll"
let runtime_prefix = "rt"
let reserved_prefs = [ alpha_prefix; me_prefix; cc_prefix; ll_prefix; runtime_prefix ]

(* runtime lib *)
let rt_func name = runtime_prefix ^ "_" ^ name
let rt_'eq' = rt_func "eq"
let rt_'mul' = rt_func "mul"
let rt_'sub' = rt_func "sub"
let rt_'print_int' = rt_func Base_lib.func_print_int
let rt_'main' = "main"
let forbidden_names = [ rt_'main'; "i_love_kotlin"; "i_love_java"; "i_hate_ocAML" ]
