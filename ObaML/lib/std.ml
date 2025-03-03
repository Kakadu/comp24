open Typedtree
open Containers

let mult = "( * )"
let div = "( / )"
let plus = "( + )"
let minus = "( - )"
let eq = "( = )"
let eqr = "( == )"
let neq = "( <> )"
let neqr = "( != )"
let lt = "( < )"
let lte = "( <= )"
let gt = "( > )"
let gte = "( >= )"
let la = "( && )"
let lo = "( || )"
let pi = "print_int"
let ps = "print_string"
let uplus = "( ~+ )"
let uminus = "( ~- )"
let matching_failed = "#matching_failed#"
let tuple_getter = "#tuple_getter#"
let list_head_getter = "#list_head_getter#"
let list_tail_getter = "#list_tail_getter#"
let list_length_getter = "#list_length_getter#"


let std_lst =
  [ mult, Scheme (VarISet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; div, Scheme (VarISet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; plus, Scheme (VarISet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; minus, Scheme (VarISet.empty, tprim_int @-> tprim_int @-> tprim_int)
  ; eq, Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool)
  ; ( eqr
    , Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; ( neq
    , Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; ( neqr
    , Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; lt, Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool)
  ; ( lte
    , Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; gt, Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool)
  ; ( gte
    , Scheme (VarISet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool) )
  ; la, Scheme (VarISet.empty, tprim_bool @-> tprim_bool @-> tprim_bool)
  ; lo, Scheme (VarISet.empty, tprim_bool @-> tprim_bool @-> tprim_bool)
  ; pi, Scheme (VarISet.empty, tprim_int @-> tprim_unit)
  ; ps, Scheme (VarISet.empty, tprim_string @-> tprim_unit)
  ; uplus, Scheme (VarISet.empty, tprim_int @-> tprim_int)
  ; uminus, Scheme (VarISet.empty, tprim_int @-> tprim_int)
  ]
;;

let additional_std_lst =
  [ matching_failed, Scheme (VarISet.singleton (-1), tprim_unit @-> type_var (-1))
  ; (tuple_getter
    , Scheme (VarISet.of_list [ -1; -2 ], tprim_int @-> type_var (-1) @-> type_var (-2)) )
  ; ( list_head_getter
    , Scheme (VarISet.singleton (-1), tlist (type_var (-1)) @-> type_var (-1)) )
  ; ( list_tail_getter
    , Scheme (VarISet.singleton (-1), tlist (type_var (-1)) @-> tlist (type_var (-1))) )
  ; ( list_length_getter
    , Scheme (VarISet.singleton (-1), tlist (type_var (-1)) @-> tprim_int) )
  ]
;;

let extended_std_lst = List.append std_lst additional_std_lst

let extended_std_var_set =
  let init_var_set = VarSSet.empty in
  let init_var_set = VarSSet.add mult init_var_set in
  let init_var_set = VarSSet.add div init_var_set in
  let init_var_set = VarSSet.add plus  init_var_set in
  let init_var_set = VarSSet.add minus init_var_set in
  let init_var_set = VarSSet.add eq init_var_set in
  let init_var_set = VarSSet.add eqr init_var_set in
  let init_var_set = VarSSet.add neq init_var_set in
  let init_var_set = VarSSet.add neqr init_var_set in
  let init_var_set = VarSSet.add lt init_var_set in
  let init_var_set = VarSSet.add lte init_var_set in
  let init_var_set = VarSSet.add gt init_var_set in
  let init_var_set = VarSSet.add gte init_var_set in
  let init_var_set = VarSSet.add la init_var_set in
  let init_var_set = VarSSet.add lo init_var_set in
  let init_var_set = VarSSet.add pi init_var_set in
  let init_var_set = VarSSet.add ps init_var_set in
  let init_var_set = VarSSet.add uplus init_var_set in
  let init_var_set = VarSSet.add uminus init_var_set in
  let init_var_set = VarSSet.add matching_failed init_var_set in
  let init_var_set = VarSSet.add tuple_getter init_var_set in
  let init_var_set = VarSSet.add list_head_getter init_var_set in
  let init_var_set = VarSSet.add list_tail_getter init_var_set in
  let init_var_set = VarSSet.add list_length_getter init_var_set in
  init_var_set
;;
