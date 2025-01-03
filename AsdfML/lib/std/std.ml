open Types

type std =
  { name : string
  ; typ : ty
  }

let stdlib : std list =
  [ { name = "( + )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( - )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( * )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( / )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( > )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( < )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( >= )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( <= )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( = )"; typ = dummy_ty ^-> dummy_ty ^-> bool_typ }
  ; { name = "( <> )"; typ = dummy_ty ^-> dummy_ty ^-> bool_typ }
  ; { name = "( && )"; typ = bool_typ ^-> bool_typ ^-> bool_typ }
  ; { name = "( || )"; typ = bool_typ ^-> bool_typ ^-> bool_typ }
  ; { name = "[ - ]"; typ = int_typ ^-> int_typ }
  ; { name = "not"; typ = bool_typ ^-> bool_typ }
  ; { name = "( :: )"; typ = dummy_ty ^-> TList dummy_ty ^-> TList dummy_ty }
  ; { name = "print_int"; typ = int_typ ^-> unit_typ }
  ]
;;

let runtime : std list =
  [ { name = "`tuple_field"; typ = dummy_ty }
  ; { name = "`list_field"; typ = dummy_ty }
  ; { name = "`list_hd"; typ = dummy_ty }
  ; { name = "`list_tl"; typ = dummy_ty }
  ; { name = "(TODO: check cons pattern)"; typ = dummy_ty }
  ]
;;
