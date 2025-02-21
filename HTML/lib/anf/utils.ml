open AstLib.Ast

module StringSet = struct
  type t = (string, Base.String.comparator_witness) Base.Set.t

  let empty = Base.Set.empty (module Base.String)
  let add = Base.Set.add
  let remove = Base.Set.remove
  let diff = Base.Set.diff
  let singleton = Base.Set.add empty
  let contains set key = Base.Set.mem set key
  let union = Base.Set.union
  let union_all = Base.Set.union_list (module Base.String)
  let elements = Base.Set.to_list
  let from_list = Base.Set.of_list (module Base.String)
  let is_empty = Base.Set.is_empty
end

let rec bound_vars_pattern = function
  | PConstraint (p, _) -> bound_vars_pattern p
  | PId s -> [ s ]
  | PTuple (p1, p2, ps) ->
    bound_vars_pattern p1
    @ bound_vars_pattern p2
    @ List.concat (List.map bound_vars_pattern ps)
  | PList (p1, p2) -> bound_vars_pattern p1 @ bound_vars_pattern p2
  | PConst _ -> []
;;