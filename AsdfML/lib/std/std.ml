open Types

type std =
  { name : string
  ; typ : ty
  ; extern : string
  }

let stdlib : std list =
  [ { name = "( + )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_add"}
  ; { name = "( - )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_sub"}
  ; { name = "( * )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_mul"}
  ; { name = "( / )"; typ = int_typ ^-> int_typ ^-> int_typ; extern = "ml_div"}
  ; { name = "( > )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_gt"}
  ; { name = "( < )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_lt"}
  ; { name = "( >= )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_ge"}
  ; { name = "( <= )"; typ = int_typ ^-> int_typ ^-> bool_typ; extern = "ml_le"}
  ; { name = "( = )"; typ = dummy_ty ^-> dummy_ty ^-> bool_typ; extern = "ml_eq"}
  ; { name = "( <> )"; typ = dummy_ty ^-> dummy_ty ^-> bool_typ; extern = "ml_ne"}
  ; { name = "( && )"; typ = bool_typ ^-> bool_typ ^-> bool_typ; extern = "ml_and"}
  ; { name = "( || )"; typ = bool_typ ^-> bool_typ ^-> bool_typ; extern = "ml_or"}
  ; { name = "[ - ]"; typ = int_typ ^-> int_typ; extern = "ml_neg"}
  ; { name = "not"; typ = bool_typ ^-> bool_typ; extern = "ml_not"}
  ; { name = "( :: )"; typ = dummy_ty ^-> TList dummy_ty ^-> TList dummy_ty; extern = "ml_cons"}
  ; { name = "print_int"; typ = int_typ ^-> unit_typ; extern = "ml_print_int"}
  ]
;;

let runtime : std list =
  [ { name = "`tuple_field"; typ = dummy_ty; extern = "ml_tuple_field"}
  ; { name = "`list_field"; typ = dummy_ty; extern = "ml_list_field"}
  ; { name = "`list_hd"; typ = dummy_ty; extern = "ml_list_hd"}
  ; { name = "`list_tl"; typ = dummy_ty; extern = "ml_list_tl"}
  ; { name = "(TODO: check cons pattern)"; typ = dummy_ty; extern = "TODO:remove"}
  ]
;;
