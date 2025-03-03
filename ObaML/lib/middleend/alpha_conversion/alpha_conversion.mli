(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** [All] - setting to convert all AST
    original:
    let a = 1;;
    let x = 1;;
    let a x = a + x;;

    after conversion:
    let a = 1;;
    let x = 1;;
    let oba0 oba1 = (a  +  oba1);;

    [Inner] - setting to convert only inner expressions
    original:
    let a = 1;;
    let x = 1;;
    let a x = a + x;;

    after conversion:
    let a = 1;;
    let x = 1;;
    let a oba0 = (a  +  oba0);; *)
type alpha_conversion_setting =
  | All
  | Inner

val run_alpha_conversion
  :  Simple_ast.sstructure
  -> alpha_conversion_setting
  -> Simple_ast.sstructure
