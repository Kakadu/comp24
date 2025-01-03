open Ast

type error =
  | Occurs_check of type_id * typ
  | Unification_failed of typ * typ
  | Tuple_unequal_lens of typ * typ
  | Variable_not_found of ident
[@@deriving show]

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val foldl : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  val fresh : type_id t
  val run : 'a t -> ('a, error) Result.t
end

module Varset : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit
end

module Type : sig
  val occurs : type_id -> typ -> bool
  val free_vars : typ -> Varset.t
end

module Subst : sig
  type t

  val empty : t
  val singleton : type_id -> typ -> t
  val find : type_id -> t -> typ option
  val remove : type_id -> t -> t
  val unify_pair : typ -> typ -> t R.t
  val apply : typ -> t -> typ
  val compose : t -> t -> t R.t
  val pp : Stdlib.Format.formatter -> t -> unit
end

type scheme = Scheme of Varset.t * typ (** Forall quantified vars * [typ] *)
[@@deriving show]

module Scheme : sig
  type t

  val free_vars : t -> Varset.t
  val apply_subst : Subst.t -> t -> t
end

type var_name = string

module TypeEnv : sig
  type t

  val empty : t
  val free_vars : t -> Varset.t
  val extend : var_name -> Scheme.t -> t -> t
  val generalize : typ -> t -> Scheme.t
  val generalize_pattern : pattern -> typ -> t -> t
  val pp : Stdlib.Format.formatter -> t -> unit
end

val infer_expr : TypeEnv.t -> expr -> (Subst.t * typ) R.t
val infer_program : structure -> (Subst.t * TypeEnv.t) R.t
