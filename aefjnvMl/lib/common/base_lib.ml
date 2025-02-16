(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

let int_ty_op = TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
let op_mul = "*"
let op_div = "/"
let op_plus = "+"
let op_minus = "-"
let comp_ty_op binder = TArrow (TVar binder, TArrow (TVar binder, TPrim "bool"))
let op_less_eq = "<="
let op_more_eq = ">="
let op_less = "<"
let op_more = ">"
let op_eq = "="
let op_2eq = "=="
let op_not_eq = "!="
let bool_ty_op = TArrow (TPrim "bool", TArrow (TPrim "bool", TPrim "bool"))
let op_and = "&&"
let op_or = "||"
let sig_un_op_minus = TArrow (TPrim "int", TPrim "int")
let un_op_minus = "-"
let sig_un_op_not = TArrow (TPrim "bool", TPrim "bool")
let un_op_not = "!"
let un_op_prefix = "~"
let sig_func_print_int = TArrow (TPrim "int", TPrim "unit")
let func_print_int = "print_int"

let converte_infix = function
  | name when name = op_mul -> "op_mul"
  | name when name = op_div -> "op_div"
  | name when name = op_plus -> "op_plus"
  | name when name = op_minus -> "op_minus"
  | name when name = op_less_eq -> "op_less_eq"
  | name when name = op_more_eq -> "op_more_eq"
  | name when name = op_less -> "op_less"
  | name when name = op_more -> "op_more"
  | name when name = op_eq -> "op_eq"
  | name when name = op_2eq -> "op_2eq"
  | name when name = op_not_eq -> "op_not_eq"
  | name when name = op_and -> "op_and"
  | name when name = op_or -> "op_or"
  | name when name = un_op_prefix ^ un_op_minus -> "un_op_minus"
  | name when name = un_op_prefix ^ un_op_not -> "un_op_minus"
  | name -> name
;;

let is_binop name =
  converte_infix name
  |> function
  | name' when name' = name -> false
  | _ -> true
;;

let std_lib_names =
  [ op_mul
  ; op_div
  ; op_plus
  ; op_minus
  ; (*  *)
    op_less_eq
  ; op_more_eq
  ; op_less
  ; op_more
  ; op_eq
  ; op_2eq
  ; op_not_eq
  ; (*  *)
    op_and
  ; op_or
  ; (*  *)
    un_op_prefix ^ un_op_minus
  ; un_op_prefix ^ un_op_not
  ; (*  *)
    func_print_int
  ]
;;