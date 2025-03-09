open Types

type t

val find : string -> t -> Scheme.t option
val extend : t -> string -> Scheme.t -> t
val empty : t
val init : (string * Scheme.t) list -> t
val free_vars : t -> VarSet.t
val apply : Subst.t -> t -> t
val diff : t -> t -> t
val to_list : t -> (string * Scheme.t) list
