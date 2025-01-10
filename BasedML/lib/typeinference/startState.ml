(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open StatementInfer
open Help

type start_bin_op = string * Ast.type_name * Ast.type_name * Ast.type_name

let add_bin_op : start_bin_op -> (state, unit) t =
  fun (name, arg1, arg2, ret) ->
  let tvs = List.fold_left get_tv_from_tp SetPolyType.empty [ arg1; arg2; ret ] in
  let tp = Ast.TFunction (arg1, Ast.TFunction (arg2, ret)) in
  if SetPolyType.is_empty tvs
  then write_flat_var_type name tp
  else write_var_type name (TFSchem (tvs, tp))
;;

let bin_op_list : start_bin_op list =
  Ast.(
    let tv1 = TPoly (PTSystem "'s") in
    [ "( - )", TInt, TInt, TInt
    ; "( + )", TInt, TInt, TInt
    ; "( / )", TInt, TInt, TInt
    ; "( * )", TInt, TInt, TInt
    ; "( < )", tv1, tv1, TBool
    ; "( <= )", tv1, tv1, TBool
    ; "( >= )", tv1, tv1, TBool
    ; "( <> )", tv1, tv1, TBool
    ; "( > )", tv1, tv1, TBool
    ; "( = )", tv1, tv1, TBool
    ; "( :: )", tv1, TList tv1, TList tv1
    ])
;;

let empty_state : state = MapString.empty, [], 0
let (start_state : state), res = run (map_list add_bin_op bin_op_list) empty_state

let _check =
  match res with
  | Result.Error x ->
    let _ = Format.printf "Error during init start state for infer:\n\t%s\n" x in
    exit (-1)
  | _ -> ()
;;
