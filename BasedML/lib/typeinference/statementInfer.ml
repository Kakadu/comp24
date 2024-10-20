(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

include Common.StateMonad
open Substitution
open Help

type env_map = type_form MapString.t [@@deriving show { with_path = false }]
type tv_num = int
type state = env_map * substitution_list * tv_num

let read_env : (state, env_map) t =
  let* env, _, _ = read in
  return env
;;

let write_env : env_map -> (state, unit) t =
  fun env ->
  let* _, substs, tv = read in
  write (env, substs, tv)
;;

let fresh_tv : (state, Ast.typeName) t =
  let* env, substs, tv = read in
  return (Ast.TPoly (Format.sprintf "_p%x" tv)) <* write (env, substs, tv + 1)
;;

let specialise : SetString.t * Ast.typeName -> (state, Ast.typeName) t =
  fun (free_vars, stp) ->
  let names = SetString.elements free_vars in
  let* v2v_lst =
    map_list
      (fun s ->
        let* new_v = fresh_tv in
        return (s, new_v))
      names
  in
  let v2v = MapString.of_seq (List.to_seq v2v_lst) in
  let rec traverse = function
    | Ast.TPoly x as old_t ->
      (match MapString.find_opt x v2v with
       | Some x -> x
       | None -> old_t)
    | Ast.TFunction (t1, t2) -> Ast.TFunction (traverse t1, traverse t2)
    | Ast.TTuple t_lst -> Ast.TTuple (List.map traverse t_lst)
    | Ast.TList t1 -> Ast.TList (traverse t1)
    | x -> x
  in
  let new_tp = traverse stp in
  (* let _ = Format.printf "[spec]: %s\n" (Ast.show_typeName new_tp) in *)
  return new_tp
;;

let read_var_type : string -> (state, Ast.typeName option) t =
  fun name ->
  let* env = read_env in
  match MapString.find_opt name env with
  | Some (TFSchem (fv, stp)) ->
    let* tp = specialise (fv, stp) in
    return (Some tp)
  | Some (TFFlat x) -> return (Some x)
  | None -> return None
;;

let write_var_type : string -> type_form -> (state, unit) t =
  fun name tf ->
  let* env, substs, tv_num = read in
  let new_env = MapString.add name tf env in
  write (new_env, substs, tv_num)
;;

let write_flat_var_type : string -> Ast.typeName -> (state, unit) t =
  fun s tp -> write_var_type s (TFFlat tp)
;;

let read_subs : (state, substitution_list) t =
  let* _, subs, _ = read in
  return subs
;;

let write_subs : substitution_list -> (state, unit) t =
  fun subs ->
  let* env, _, tv = read in
  write (env, subs, tv)
;;

let write_subst : Ast.typeName -> Ast.typeName -> (state, unit) t =
  fun tp1 tp2 ->
  (* DEBUG PRINT
     let _ =
     Format.printf "[add]: %s %s\n" (Ast.show_typeName tp1) (Ast.show_typeName tp2)
     in *)
  let tp1, tp2 = if tp1 < tp2 then tp1, tp2 else tp2, tp1 in
  let* subs = read_subs in
  let new_subs, res = run (unify tp1 tp2) subs in
  match res with
  | Result.Error x -> fail x
  | Result.Ok _tp -> write_subs new_subs
;;

let rec write_scheme_for_pattern
  : SetString.t -> Ast.pattern_no_constraint -> Ast.typeName -> (state, unit) t
  =
  fun free_vars pattern tp ->
  let rec_call = write_scheme_for_pattern free_vars in
  match pattern, tp with
  | PWildCard, _ | PNil, _ | PConstant _, _ -> return ()
  | PIdentifier x, tp ->
    let used_tvs = get_tv_from_tp SetString.empty tp in
    let new_free_vars = SetString.inter used_tvs free_vars in
    write_var_type x (TFSchem (new_free_vars, tp))
  | PCons (p1, p2), TList t -> rec_call p1 t *> rec_call p2 tp
  | PTuple p_lst, TTuple t_lst ->
    map_list (fun (p, t) -> rec_call p t) (List.combine p_lst t_lst) *> return ()
  | _ -> fail "something strange during generealisetion"
;;

let restore_type : Ast.typeName -> (state, Ast.typeName) t =
  fun tp ->
  let* subs = read_subs in
  return (apply_substs subs tp)
;;

let get_tv_from_env : env_map -> SetString.t =
  fun env ->
  MapString.fold
    (fun _var_name tp acc ->
      match tp with
      | TFFlat tp -> get_tv_from_tp acc tp
      | TFSchem _ -> acc)
    env
    SetString.empty
;;
