(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let start_identifiers =
  let binary_operations =
    [ "( + )"
    ; "( - )"
    ; "( * )"
    ; "( / )"
    ; "( % )"
    ; "( > )"
    ; "( < )"
    ; "( >= )"
    ; "( <= )"
    ; "( = )"
    ; "( == )"
    ; "( != )"
    ; "( <> )"
    ; "( && )"
    ; "( || )"
    ]
  in
  let unary_operations = [ "U-"; "U+"; "UNot" ] in
  let stdlib_functions = [ "print_int"; "print_bool"; "print_string"; "print_char" ] in
  binary_operations @ unary_operations @ stdlib_functions
;;
