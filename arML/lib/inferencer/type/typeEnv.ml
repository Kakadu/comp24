(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open TypeTree

(* Type environment is mapping top-level identifiers to their schema type *)
type t = (string, Schema.schema, Base.String.comparator_witness) Base.Map.t

let empty : t = Base.Map.empty (module Base.String)

let free_vars env =
  Base.Map.fold
    ~init:TypeVarSet.empty
    ~f:(fun ~key:_ ~data acc -> TypeVarSet.union acc (Schema.free_vars data))
    env
;;

let apply env sub = Base.Map.map env ~f:(Schema.apply sub)
let extend env key schema = Base.Map.update ~f:(fun _ -> schema) env key
let find env key = Base.Map.find env key

(* Search for an identifier in the environment. If not found - error. *)
let lookup_env env name =
  match find env name with
  | Some schema ->
    let* ty = Instantiate.instantiate schema in
    return (Substitution.empty, ty)
  | None -> fail (TypeErrors.Unbound_variable name)
;;
