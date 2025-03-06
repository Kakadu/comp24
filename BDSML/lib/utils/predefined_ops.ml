(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let predefine_operators =
  [ "( + )", "int -> int -> int", "__op_plus"
  ; "( - )", "int -> int -> int", "__op_minus"
  ; "( * )", "int -> int -> int", "__op_mult"
  ; "( / )", "int -> int -> int", "__op_div"
  ; "( ~- )", "int -> int", "__op_neg"
  ; "( ~+ )", "int -> int", "__op_pos"
  ; "not", "bool -> bool", "not"
  ; "( > )", "'a -> 'a -> bool", "__op_gt"
  ; "( >= )", "'a -> 'a -> bool", "__op_ge"
  ; "( < )", "'a -> 'a -> bool", "__op_lt"
  ; "( <= )", "'a -> 'a -> bool", "__op_le"
  ; "( = )", "'a -> 'a -> bool", "__op_eq"
  ; "( <> )", "'a -> 'a -> bool", "__op_neq"
  ; "( || )", "bool -> bool -> bool", "__op_or"
  ; "( && )", "bool -> bool -> bool", "__op_and"
  ; "( == )", "'a -> 'a -> bool", "__op_phys_eq"
  ; "print_int", "int -> unit", "print_int"
  ]
;;
