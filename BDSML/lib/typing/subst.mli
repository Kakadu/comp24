open Types

type t

val fold_left
  :  t
  -> 'a Monads.t
  -> ('a -> VarId.t * type_val -> 'a Monads.t)
  -> 'a Monads.t

val empty : t
val mapping : VarId.t -> type_val -> (VarId.t * type_val) Monads.t
val singleton : VarId.t -> type_val -> t Monads.t
val apply : t -> type_val -> type_val
val unify : type_val -> type_val -> t Monads.t
val extend : t -> VarId.t * type_val -> t Monads.t
val compose_maps : t -> t -> t Monads.t
val compose_all : t list -> t Monads.t
