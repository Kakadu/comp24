(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Types

type std =
  { name : string
  ; typ : ty
  ; extern : string
  }

let stdlib : std list =
  [ { name = "( + )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_add" }
  ; { name = "( - )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_sub" }
  ; { name = "( * )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_mul" }
  ; { name = "( / )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_div" }
  ; { name = "( > )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_gt" }
  ; { name = "( < )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_lt" }
  ; { name = "( >= )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_ge" }
  ; { name = "( <= )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_le" }
  ; { name = "( = )"; typ = dummy_ty ^-> dummy_ty ^-> bool_typ; extern = "ml_eq" }
  ; { name = "( <> )"; typ = dummy_ty ^-> dummy_ty ^-> bool_typ; extern = "ml_ne" }
  ; { name = "( && )"; typ = bool_typ ^-> bool_typ ^-> bool_typ; extern = "ml_and" }
  ; { name = "( || )"; typ = bool_typ ^-> bool_typ ^-> bool_typ; extern = "ml_or" }
  ; { name = "[ - ]"; typ = int_typ ^-> int_typ; extern = "ml_neg" }
  ; { name = "not"; typ = bool_typ ^-> bool_typ; extern = "ml_not" }
  ; { name = "( :: )"
    ; typ = dummy_ty ^-> TList dummy_ty ^-> TList dummy_ty
    ; extern = "ml_list_cons"
    }
  ; { name = "print_int"; typ = int_typ ^-> unit_typ; extern = "ml_print_int" }
  ; { name = "print_bool"; typ = bool_typ ^-> unit_typ; extern = "ml_print_bool" }
  ; { name = "print_tuple"; typ = dummy_ty ^-> unit_typ; extern = "ml_print_tuple" }
  ; { name = "print_list"; typ = dummy_ty ^-> unit_typ; extern = "ml_print_list" }
  ]
;;

let runtime : std list =
  [ { name = "`create_tuple"; typ = arg1; extern = "ml_create_tuple" }
  ; { name = "`tuple_len"; typ = arg1; extern = "ml_tuple_len" }
  ; { name = "`set_tuple_field"; typ = arg3; extern = "ml_set_tuple_field" }
  ; { name = "`get_tuple_field"; typ = arg2; extern = "ml_get_tuple_field" }
  ; { name = "`list_field"; typ = arg2; extern = "ml_list_field" }
  ; { name = "`list_hd"; typ = arg1; extern = "ml_list_hd" }
  ; { name = "`list_tl"; typ = arg1; extern = "ml_list_tl" }
  ; { name = "`list_len"; typ = arg1; extern = "ml_list_len" }
  ; { name = "`list_is_empty"; typ = arg1; extern = "ml_list_is_empty" }
  ; { name = "`create_closure"; typ = arg2; extern = "create_closure" }
  ]
;;

let lookup_extern name =
  stdlib @ runtime
  |> Base.List.find ~f:(fun x -> x.name = name)
  |> Option.map (fun x -> x.extern)
;;
