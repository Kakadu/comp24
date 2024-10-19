(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

include Common.StateMonad
open Substitution

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type type_form =
  | TFFlat of Ast.typeName
  | TFSchem of string list * Ast.typeName
[@@deriving show { with_path = false }]

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

let read_var_type : string -> (state, Ast.typeName option) t =
  fun name ->
  let* env = read_env in
  match MapString.find_opt name env with
  | Some (TFSchem _) -> fail "Schem isn't supported"
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

let read_subs : (state, subs_state) t =
  let* _, subs, _ = read in
  return subs
;;

let write_subs : subs_state -> (state, unit) t =
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

let fresh_tv : (state, Ast.typeName) t =
  let* env, substs, tv = read in
  return (Ast.TPoly (Format.sprintf "_p%x" tv)) <* write (env, substs, tv + 1)
;;
