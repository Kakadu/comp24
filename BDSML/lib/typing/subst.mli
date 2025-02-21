open Types

type t

val fold_left
  :  t
  -> 'a Monads.t
  -> ('a -> TVarId.t * type_val -> 'a Monads.t)
  -> 'a Monads.t

val empty : t
val mapping : TVarId.t -> type_val -> (TVarId.t * type_val) Monads.t
val singleton : TVarId.t -> type_val -> t Monads.t
val apply : t -> type_val -> type_val
val unify : type_val -> type_val -> t Monads.t
val extend : t -> TVarId.t * type_val -> t Monads.t
val compose : t -> t -> t Monads.t
val compose_all : t list -> t Monads.t
val remove : TVarId.t -> t -> t
