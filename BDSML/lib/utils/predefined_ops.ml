(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** vars *)
let var_nothing = "__nothing"

(** default operators *)
let op_plus = "( + )", "int -> int -> int", "__op_plus"

let op_minus = "( - )", "int -> int -> int", "__op_minus"
let op_mult = "( * )", "int -> int -> int", "__op_mult"
let op_div = "( / )", "int -> int -> int", "__op_div"
let op_neg = "( ~- )", "int -> int", "__op_neg"
let op_pos = "( ~+ )", "int -> int", "__op_pos"
let op_not = "not", "bool -> bool", "not"
let op_gt = "( > )", "'a -> 'a -> bool", "__op_gt"
let op_ge = "( >= )", "'a -> 'a -> bool", "__op_ge"
let op_lt = "( < )", "'a -> 'a -> bool", "__op_lt"
let op_le = "( <= )", "'a -> 'a -> bool", "__op_le"
let op_eq = "( = )", "'a -> 'a -> bool", "__op_eq"
let op_neq = "( <> )", "'a -> 'a -> bool", "__op_neq"
let op_or = "( || )", "bool -> bool -> bool", "__op_or"
let op_and = "( && )", "bool -> bool -> bool", "__op_and"
let op_phys_eq = "( == )", "'a -> 'a -> bool", "__op_phys_eq"
let print_int = "print_int", "int -> unit", "print_int"

(** special pattern remover *)
let disassemble_constructor = "__disassemble", "'a -> 'b", "__disassemble"

let get_from_tuple = "__get_from_tuple", "'a -> int -> 'c", "__get_from_tuple"
let same_cons = "__same_cons", "'a -> 'b -> bool", "__same_cons"
let get_cons_param = "__get_cons_param", "'a -> 'b", "__get_cons_param"
let exception_ = "__exception", "string -> 'a", "__exception"

(** List with all ops*)
let predefine_operators =
  [ op_plus
  ; op_minus
  ; op_mult
  ; op_div
  ; op_neg
  ; op_pos
  ; op_not
  ; op_gt
  ; op_ge
  ; op_lt
  ; op_le
  ; op_eq
  ; op_neq
  ; op_or
  ; op_and
  ; op_phys_eq
  ; print_int
  ; disassemble_constructor
  ; get_from_tuple
  ; same_cons
  ; get_cons_param
  ; exception_
  ]
;;
