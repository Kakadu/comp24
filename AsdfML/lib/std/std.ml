open Types

type std =
  { name : string
  ; typ : ty
  ; arity : int
  ; extern : string
  }

let stdlib : std list =
  [ { name = "( + )"
    ; typ = int_typ ^-> int_typ ^-> int_typ
    ; extern = "ml_add"
    ; arity = 0
    }
  ; { name = "( - )"
    ; typ = int_typ ^-> int_typ ^-> int_typ
    ; extern = "ml_sub"
    ; arity = 0
    }
  ; { name = "( * )"
    ; typ = int_typ ^-> int_typ ^-> int_typ
    ; extern = "ml_mul"
    ; arity = 0
    }
  ; { name = "( / )"
    ; typ = int_typ ^-> int_typ ^-> int_typ
    ; extern = "ml_div"
    ; arity = 0
    }
  ; { name = "( > )"
    ; typ = int_typ ^-> int_typ ^-> bool_typ
    ; extern = "ml_gt"
    ; arity = 0
    }
  ; { name = "( < )"
    ; typ = int_typ ^-> int_typ ^-> bool_typ
    ; extern = "ml_lt"
    ; arity = 0
    }
  ; { name = "( >= )"
    ; typ = int_typ ^-> int_typ ^-> bool_typ
    ; extern = "ml_ge"
    ; arity = 0
    }
  ; { name = "( <= )"
    ; typ = int_typ ^-> int_typ ^-> bool_typ
    ; extern = "ml_le"
    ; arity = 0
    }
  ; { name = "( = )"
    ; typ = dummy_ty ^-> dummy_ty ^-> bool_typ
    ; extern = "ml_eq"
    ; arity = 0
    }
  ; { name = "( <> )"
    ; typ = dummy_ty ^-> dummy_ty ^-> bool_typ
    ; extern = "ml_ne"
    ; arity = 0
    }
  ; { name = "( && )"
    ; typ = bool_typ ^-> bool_typ ^-> bool_typ
    ; extern = "ml_and"
    ; arity = 0
    }
  ; { name = "( || )"
    ; typ = bool_typ ^-> bool_typ ^-> bool_typ
    ; extern = "ml_or"
    ; arity = 0
    }
  ; { name = "[ - ]"; typ = int_typ ^-> int_typ; extern = "ml_neg"; arity = 0 }
  ; { name = "not"; typ = bool_typ ^-> bool_typ; extern = "ml_not"; arity = 0 }
  ; { name = "( :: )"
    ; typ = dummy_ty ^-> TList dummy_ty ^-> TList dummy_ty
    ; extern = "ml_list_cons"
    ; arity = 0
    }
  ; { name = "print_int"; typ = int_typ ^-> unit_typ; extern = "ml_print_int"; arity = 0 }
  ; { name = "print_bool"; typ = bool_typ ^-> unit_typ; extern = "ml_print_bool"; arity = 0 }
  ; { name = "print_tuple"; typ = dummy_ty ^-> unit_typ; extern = "ml_print_tuple"; arity = 0 }
  ; { name = "print_list"; typ = dummy_ty ^-> unit_typ; extern = "ml_print_list"; arity = 0 }
  ]
  |> Base.List.map ~f:(fun x -> { x with arity = Types.count_arrow_args x.typ })
;;

let runtime : std list =
  let arg1 = dummy_ty ^-> dummy_ty in
  let arg2 = dummy_ty ^-> dummy_ty ^-> dummy_ty in
  let arg3 = dummy_ty ^-> dummy_ty ^-> dummy_ty ^-> dummy_ty in
  [ 
    { name = "`create_tuple"; typ = arg1; extern = "ml_create_tuple"; arity = 0 }
  ; { name = "`set_tuple_field"; typ = arg3; extern = "ml_set_tuple_field"; arity = 0 }
  ; { name = "`get_tuple_field"; typ = arg2; extern = "ml_get_tuple_field"; arity = 0 }
  ; { name = "`list_field"; typ = arg2; extern = "ml_list_field"; arity = 0 }
  ; { name = "`list_hd"; typ = arg1; extern = "ml_list_hd"; arity = 0 }
  ; { name = "`list_tl"; typ = arg1; extern = "ml_list_tl"; arity = 0 }
  ; { name = "`list_is_empty"; typ = arg1; extern = "ml_list_is_empty"; arity = 0 }
  ; { name = "`create_closure"; typ = arg2; extern = "create_closure"; arity = 0 }
  ]
  |> Base.List.map ~f:(fun x -> { x with arity = Types.count_arrow_args x.typ })
;;

let lookup_extern name =
  stdlib @ runtime |> Base.List.find ~f:(fun x -> x.name = name) |> Option.map (fun x -> x.extern)
;;
