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
  ; { name = "println_int"; typ = int_typ ^-> unit_typ; extern = "ml_println_int" }
  ; { name = "println_bool"; typ = bool_typ ^-> unit_typ; extern = "ml_println_bool" }
  ; { name = "print_tuple"; typ = dummy_ty ^-> unit_typ; extern = "ml_print_tuple" }
  ; { name = "print_list"; typ = dummy_ty ^-> unit_typ; extern = "ml_print_list" }
  ; { name = "panic"; typ = unit_typ ^-> dummy_ty; extern = "ml_panic" }
  ; { name = "print_newline"; typ = unit_typ ^-> unit_typ; extern = "ml_print_newline" }
  ; { name = "print_char"; typ = int_typ ^-> unit_typ; extern = "ml_print_char" }
  ]
;;

let runtime : std list =
  [ { name = "ml_create_tuple"; typ = int_typ ^-> dummy_ty2; extern = "ml_create_tuple" }
  ; { name = "ml_tuple_len"; typ = dummy_ty ^-> int_typ; extern = "ml_tuple_len" }
  ; { name = "ml_set_tuple_field"
    ; typ = dummy_ty ^-> dummy_ty2 ^-> dummy_ty3 ^-> dummy_ty4
    ; extern = "ml_set_tuple_field"
    }
  ; { name = "ml_get_tuple_field"
    ; typ = dummy_ty ^-> dummy_ty2 ^-> dummy_ty3
    ; extern = "ml_get_tuple_field"
    }
  ; { name = "ml_list_field"
    ; typ = dummy_list ^-> int_typ ^-> dummy_ty
    ; extern = "ml_list_field"
    }
  ; { name = "ml_list_hd"; typ = dummy_list ^-> dummy_ty; extern = "ml_list_hd" }
  ; { name = "ml_list_tl"; typ = dummy_list ^-> dummy_list; extern = "ml_list_tl" }
  ; { name = "ml_list_len"; typ = dummy_list ^-> int_typ; extern = "ml_list_len" }
  ; { name = "ml_list_is_empty"
    ; typ = dummy_list ^-> bool_typ
    ; extern = "ml_list_is_empty"
    }
  ; { name = "ml_create_closure"; typ = arg2; extern = "create_closure" }
  ]
;;

let lookup_extern name =
  stdlib @ runtime
  |> Base.List.find ~f:(fun x -> x.name = name)
  |> Option.map (fun x -> x.extern)
;;
