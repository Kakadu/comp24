open Types

module Scheme = struct
  type t = VarSet.t * type_val

  let create set ty : t = set, ty
  let free_vars (set, ty) = VarSet.diff (free_vars ty) set

  let apply sub (names, ty) =
    let s2 = VarSet.fold (fun k s -> Subst.remove k s) names sub in
    names, Subst.apply s2 ty
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
end
