(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Types

module Scheme = struct
  type t = VarSet.t * type_val

  let create set ty : t = set, ty
  let free_vars (set, ty) = VarSet.diff (free_vars ty) set

  let apply sub (names, ty) =
    let s2 = VarSet.fold (fun k s -> Subst.remove k s) names sub in
    names, Subst.apply s2 ty
  ;;

  let get_type (_, ty) = ty

  let equal (_, t1) (_, t2) =
    match Monads.run (Subst.unify t1 t2) @@ TVarId.create 0 with
    | Error _ -> false
    | _ -> true
  ;;
end

module TypeEnv = struct
  module Map = Map.Make (String)

  type t = Scheme.t Map.t

  let find : string -> t -> Scheme.t option = Map.find_opt
  let extend map name scheme = Map.add name scheme map
  let empty : t = Map.empty

  let init values =
    let rec helper map = function
      | (k, v) :: tl -> helper (extend map k v) tl
      | _ -> map
    in
    helper empty values
  ;;

  let free_vars (map : t) : VarSet.t =
    Map.fold
      (fun _ scheme acc -> VarSet.union acc @@ Scheme.free_vars scheme)
      map
      VarSet.empty
  ;;

  let apply s env = Map.map (Scheme.apply s) env

  let diff (env1 : t) (env2 : t) : t =
    Map.filter
      (fun key v ->
        match Map.find_opt key env2 with
        | Some v2 -> not (Scheme.equal v v2)
        | None -> true)
      env1
  ;;

  let to_list env = Map.bindings env
end
