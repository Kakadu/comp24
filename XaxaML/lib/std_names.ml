(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let std_names =
  [ "print_int"
  ; "print_bool"
  ; "#list_hd"
  ; "#list_tl"
  ; "#list_length"
  ; "#unpack_tuple"
  ; "#match_failure"
  ; "~+"
  ; "~-"
  ; "+"
  ; "-"
  ; "*"
  ; "/"
  ; "&&"
  ; "||"
  ; ">"
  ; "<"
  ; "<="
  ; ">="
  ; "="
  ; "<>"
  ; "=="
  ; "!="
  ]
;;

let type_var_count = 10

let to_rename =
  [ "main"
  ; "_start"
  ; "create_int"
  ; "create_bool"
  ; "create_unit"
  ; "get_int"
  ; "get_bool"
  ; "create_closure"
  ; "apply"
  ; "list_cons"
  ; "create_tuple"
  ; "add"
  ; "sub"
  ; "mul"
  ; "div_op"
  ; "eq"
  ; "ne"
  ; "lt"
  ; "gt"
  ; "le"
  ; "ge"
  ; "and_op"
  ; "or_op"
  ; "uplus"
  ; "uminus"
  ; "phys_ne"
  ; "phys_eq"
  ; "list_hd"
  ; "list_tl"
  ; "list_length"
  ; "unpack_tuple"
  ; "match_failure"
  ]
;;
