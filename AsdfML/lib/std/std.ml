open Types

type std =
  { name : string
  ; typ : ty
  }

let stdlib : std list =
  let var = TVar (-1) in
  [ { name = "( + )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( - )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( * )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( / )"; typ = int_typ ^-> int_typ ^-> int_typ }
  ; { name = "( > )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( < )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( >= )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( <= )"; typ = int_typ ^-> int_typ ^-> bool_typ }
  ; { name = "( = )"; typ = var ^-> var ^-> bool_typ }
  ; { name = "( <> )"; typ = var ^-> var ^-> bool_typ }
  ; { name = "( && )"; typ = bool_typ ^-> bool_typ ^-> bool_typ }
  ; { name = "( || )"; typ = bool_typ ^-> bool_typ ^-> bool_typ }
  ; { name = "[ - ]"; typ = int_typ ^-> int_typ }
  ; { name = "not"; typ = bool_typ ^-> bool_typ }
  ; { name = "( :: )"; typ = var ^-> TList var ^-> TList var }
  ; { name = "print_int"; typ = int_typ ^-> unit_typ }
  ]
;;