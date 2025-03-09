(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

let set_empty = Set.empty (module String)
let map_empty = Map.empty (module String)

let get_uniq_name gvars name =
  let rec helper gvars id =
    let name = String.concat [ name; Int.to_string id ] in
    if Set.mem gvars name then helper gvars (id + 1) else name
  in
  if Set.mem gvars name then helper gvars 0 else name
;;

let update_name name local_vars all_vars =
  let new_name = get_uniq_name all_vars name in
  let update_lvars = Map.set local_vars ~key:name ~data:new_name in
  let update_all_vars = Set.add all_vars new_name in
  new_name, update_lvars, update_all_vars
;;

let update_all_name_list name_list vars all_vars =
  let new_name_list, new_vars, new_all_vars =
    List.fold
      ~init:([], vars, all_vars)
      ~f:(fun (acc, acc_lvars, acc_fvars) n ->
        let new_name, new_vars, new_all_vars =
          if Poly.( = ) n "()"
          then update_name "unit" acc_lvars acc_fvars
          else update_name n acc_lvars acc_fvars
        in
        new_name :: acc, new_vars, new_all_vars)
      name_list
  in
  List.rev new_name_list, new_vars, new_all_vars
;;

let update_name_list name_list vars all_vars =
  let new_name_list, new_vars, new_all_vars =
    List.fold
      ~init:([], vars, all_vars)
      ~f:(fun (acc, acc_vars, acc_all_vars) n ->
        let new_name, new_vars, new_all_vars =
          if Poly.( = ) n "()"
          then n, acc_vars, acc_all_vars
          else update_name n acc_vars acc_all_vars
        in
        new_name :: acc, new_vars, new_all_vars)
      name_list
  in
  List.rev new_name_list, new_vars, new_all_vars
;;
