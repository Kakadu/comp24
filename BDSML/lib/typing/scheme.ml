open Types

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
