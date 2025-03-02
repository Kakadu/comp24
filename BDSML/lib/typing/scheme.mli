open Types

type t = VarSet.t * type_val

val create : VarSet.t -> type_val -> t
val apply : Subst.t -> t -> t
val free_vars : t -> VarSet.t
val get_type : t -> type_val
val equal : t -> t -> bool
