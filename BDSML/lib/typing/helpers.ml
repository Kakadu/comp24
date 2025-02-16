open Types
module VarSet = Set.Make (VarId)

let rec occurs_in (v : VarId.t) = function
  | TVar b -> b = v
  | TTuple (f :: tl) -> occurs_in v f || (occurs_in v @@ TTuple tl)
  | TArrow (l, r) -> occurs_in v l || occurs_in v r
  | TBase _ -> false
  | _ -> raise (Unimplemented "occurs_in")
;;

module Scheme = struct
  type t = VarSet.t * type_val

  let create set ty : t = set, ty
  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TBase _ -> acc
      | _ -> raise (Unimplemented "free_vars")
    in
    helper VarSet.empty
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
end
