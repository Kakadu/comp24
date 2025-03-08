open Ast
open Name_helper
open Base

let rec pattern_to_string_list = function
  | PWild -> []
  | PConst _ -> []
  | PVar (name, _) -> [ name ]
  | PTuple patterns -> List.concat_map patterns ~f:pattern_to_string_list
  | PCon (hd, tl) -> pattern_to_string_list hd @ pattern_to_string_list tl
;;

let rec uniq_pattern_name vars all_vars = function
  | PWild -> PWild, vars, all_vars
  | PConst c -> PConst c, vars, all_vars
  | PVar (name, t) ->
    let new_name, new_vars, new_all_vars =
      if Poly.( = ) name "()"
      then update_name "unit" vars all_vars
      else update_name name vars all_vars
    in
    PVar (new_name, t), new_vars, new_all_vars
  | PTuple t ->
    let new_t, new_v, new_av =
      List.fold
        ~init:([], vars, all_vars)
        ~f:(fun (acc, v, av) n ->
          let new_n, new_v, new_av = uniq_pattern_name v av n in
          new_n :: acc, new_v, new_av)
        t
    in
    PTuple (List.rev new_t), new_v, new_av
  | PCon (hd, tl) ->
    let new_hd, new_vars, new_all_vars = uniq_pattern_name vars all_vars hd in
    let new_td, new_vars, new_all_vars = uniq_pattern_name new_vars new_all_vars tl in
    PCon (new_hd, new_td), new_vars, new_all_vars
;;

let rec uniq_pattern_name_and_skip_unit vars all_vars = function
  | PWild -> PWild, vars, all_vars
  | PConst c -> PConst c, vars, all_vars
  | PVar (name, t) ->
    let new_name, new_vars, new_all_vars =
      if Poly.( = ) name "()"
      then "()", vars, all_vars
      else update_name name vars all_vars
    in
    PVar (new_name, t), new_vars, new_all_vars
  | PTuple t ->
    let new_t, new_v, new_av =
      List.fold
        ~init:([], vars, all_vars)
        ~f:(fun (acc, v, av) n ->
          let new_n, new_v, new_av = uniq_pattern_name_and_skip_unit v av n in
          new_n :: acc, new_v, new_av)
        t
    in
    PTuple (List.rev new_t), new_v, new_av
  | PCon (hd, tl) ->
    let new_hd, new_vars, new_all_vars =
      uniq_pattern_name_and_skip_unit vars all_vars hd
    in
    let new_td, new_vars, new_all_vars =
      uniq_pattern_name_and_skip_unit new_vars new_all_vars tl
    in
    PCon (new_hd, new_td), new_vars, new_all_vars
;;