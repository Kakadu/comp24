(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** [All] - setting to convert all AST
    original:
    let a b c = b + c
    let b c d = c + d

    after conversion:
    let o1 o2 o3 = o2 + o3
    let o4 o5 o6 = o5 + o6

    [Inner] - setting to convert only inner expressions
    original:
    let a b c = b + c
    let b c d = c + d

    after conversion:
    let a o1 o2 = o1 + o2
    let b o3 o4 = o3 + o4 *)
type alpha_conversion_setting =
  | All
  | Inner

val run_alpha_conversion
  :  Simple_ast.sstructure
  -> alpha_conversion_setting
  -> Simple_ast.sstructure
