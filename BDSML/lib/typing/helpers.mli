open Types

val occurs_in : VarId.t -> type_val -> bool

module VarSet : Set.S with type elt = VarId.t

module Scheme : sig
  type t

  val free_vars : type_val -> VarSet.t
end

module TypeEnv : sig
  type t

  val extend : t -> string -> Scheme.t -> t
  val empty : t
  val init : (string * Scheme.t) list -> t
end
