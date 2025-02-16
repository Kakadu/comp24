(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

include Common.StateMonad
open Substitution
open Help

type env_map = type_form MapString.t [@@deriving show { with_path = false }]
type tv_num = int
type used_types_set = SetString.t
type state = env_map * substitution_list * tv_num * used_types_set

let read_env : (state, env_map) t =
  let* env, _, _, _ = read in
  return env
;;

let write_env : env_map -> (state, unit) t =
  fun env ->
  let* _, substs, tv, uts = read in
  write (env, substs, tv, uts)
;;

let read_uts : (state, used_types_set) t =
  let* _, _, _, uts = read in
  return uts
;;

let write_uts : used_types_set -> (state, unit) t =
  fun uts ->
  let* env, substs, tv, _ = read in
  write (env, substs, tv, uts)
;;

let fresh_tv : (state, Ast.type_name) t =
  let* env, substs, tv, uts = read in
  let rec gen_new_type_name tv =
    let s = Format.sprintf "_p%x" tv in
    match SetString.find_opt s uts with
    | Some _ -> gen_new_type_name (tv + 1)
    | None -> s, tv + 1, SetString.add s uts
  in
  let tname, tv, uts = gen_new_type_name tv in
  return (Ast.TPoly tname) <* write (env, substs, tv, uts)
;;

let specialise : SetString.t * Ast.type_name -> (state, Ast.type_name) t =
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
  return new_tp
;;

let read_var_type : string -> (state, Ast.type_name option) t =
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
  let* env, substs, tv_num, uts = read in
  let new_env = MapString.add name tf env in
  write (new_env, substs, tv_num, uts)
;;

let write_flat_var_type : string -> Ast.type_name -> (state, unit) t =
  fun s tp -> write_var_type s (TFFlat tp)
;;

let read_subs : (state, substitution_list) t =
  let* _, subs, _, _ = read in
  return subs
;;

let write_subs : substitution_list -> (state, unit) t =
  fun subs ->
  let* env, _, tv, uts = read in
  write (env, subs, tv, uts)
;;

let write_subst : Ast.type_name -> Ast.type_name -> (state, unit) t =
  fun tp1 tp2 ->
  let tp1, tp2 = if tp1 < tp2 then tp1, tp2 else tp2, tp1 in
  let* subs = read_subs in
  let new_subs, res = run (unify tp1 tp2) subs in
  match res with
  | Result.Error x -> fail x
  | Result.Ok _tp -> write_subs new_subs
;;

let rec write_scheme_for_pattern
  : SetString.t -> Ast.pattern -> Ast.type_name -> (state, unit) t
  =
  fun free_vars pattern tp ->
  let rec_call = write_scheme_for_pattern free_vars in
  match pattern, tp with
  | PWildCard, _ | PConstant _, _ -> return ()
  | PIdentifier x, tp ->
    let used_tvs = get_tv_from_tp SetString.empty tp in
    let new_free_vars = SetString.inter used_tvs free_vars in
    write_var_type x (TFSchem (new_free_vars, tp))
  | PCons (p1, p2), TList t -> rec_call p1 t *> rec_call p2 tp
  | PTuple p_lst, TTuple t_lst ->
    map_list (fun (p, t) -> rec_call p t) (List.combine p_lst t_lst) *> return ()
  | PConstraint (pat, _), _ -> rec_call pat tp
  | _ -> fail "something strange during generealisetion"
;;

let restore_type : Ast.type_name -> (state, Ast.type_name) t =
  fun tp ->
  let* subs = read_subs in
  return (apply_substs subs tp)
;;

let get_tv_from_env : env_map -> (state, SetString.t) t =
  fun env ->
  let* subs = read_subs in
  return
    (MapString.fold
       (fun _var_name tp acc ->
         match tp with
         | TFFlat tp -> get_tv_from_tp acc (apply_substs subs tp)
         | TFSchem _ -> acc)
       env
       SetString.empty)
;;
