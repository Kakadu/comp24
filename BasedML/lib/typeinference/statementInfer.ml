(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

include Common.StateMonad
open Constraint

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
type constr_set = ConstraintSet.t [@@deriving show { with_path = false }]
type tv_num = int
type state = env_map * constr_set * tv_num

let read_env : (state, env_map) t =
  let* env, _, _ = read in
  return env
;;

let write_env : env_map -> (state, unit) t =
  fun env ->
  let* _, constrs, tv = read in
  write (env, constrs, tv)
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
  let* env, constrs, tv_num = read in
  let new_env = MapString.add name tf env in
  write (new_env, constrs, tv_num)
;;

let write_flat_var_type : string -> Ast.typeName -> (state, unit) t =
  fun s tp -> write_var_type s (TFFlat tp)
;;

let write_constr : Constraint.t -> (state, unit) t =
  fun constr ->
  let* env, constrs, tv = read in
  let new_constrs = ConstraintSet.add constr constrs in
  write (env, new_constrs, tv)
;;

let fresh_tv : (state, Ast.typeName) t =
  let* env, constr, tv = read in
  return (Ast.TPoly (Format.sprintf "_p%x" tv)) <* write (env, constr, tv + 1)
;;
