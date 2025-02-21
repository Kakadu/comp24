open Types

module Scheme : sig
  type t = VarSet.t * type_val

  val create : VarSet.t -> type_val -> t
  val free_vars : t -> VarSet.t
end

module TypeEnv : sig
  type t

  val find : string -> t -> Scheme.t option
  val extend : t -> string -> Scheme.t -> t
  val empty : t
  val init : (string * Scheme.t) list -> t
  val free_vars : t -> VarSet.t
  val apply : Subst.t -> t -> t
end
