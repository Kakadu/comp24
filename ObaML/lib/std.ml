open Typedtree

let std_lst =
  [ "( * )", Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; "( / )", Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; "( + )", Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; "( - )", Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; "( = )", Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool)
  ; ( "( == )"
    , Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; ( "( <> )"
    , Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; ( "( != )"
    , Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; "( < )", Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool)
  ; ( "( <= )"
    , Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; "( > )", Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool)
  ; ( "( >= )"
    , Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; "( && )", Scheme (VarSet.empty, tprim_bool @-> tprim_bool @-> tprim_bool)
  ; "( || )", Scheme (VarSet.empty, tprim_bool @-> tprim_bool @-> tprim_bool)
  ; "print_int", Scheme (VarSet.empty, tprim_int @-> tprim_unit)
  ; "print_string", Scheme (VarSet.empty, tprim_string @-> tprim_unit)
  ; "( ~+ )", Scheme (VarSet.empty, tprim_int @-> tprim_int)
  ; "( ~- )", Scheme (VarSet.empty, tprim_int @-> tprim_int)
  ]
;;

let additional_std_lst =
  [ "#matching_failed#", Scheme (VarSet.singleton (-1), tprim_unit @-> type_var (-1))
  ; ( "#tuple_getter#"
    , Scheme (VarSet.of_list [ -1; -2 ], tprim_int @-> type_var (-1) @-> type_var (-2)) )
  ; ( "#list_head_getter#"
    , Scheme (VarSet.singleton (-1), tlist (type_var (-1)) @-> type_var (-1)) )
  ; ( "#list_tail_getter#"
    , Scheme (VarSet.singleton (-1), tlist (type_var (-1)) @-> tlist (type_var (-1))) )
  ; ( "#list_length_getter#"
    , Scheme (VarSet.singleton (-1), tlist (type_var (-1)) @-> tprim_int) )
  ]
;;

let extended_std_lst = List.append std_lst additional_std_lst
