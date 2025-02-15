module VarId : sig
  type t = int

  val to_string : t -> string
  val create : int -> t
  val compare : t -> t -> int
  val ( + ) : t -> int -> t
end

type base_type =
  | TInt
  | TChar
  | TString

type type_val =
  | TVar of VarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TParams of type_val * type_val (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)

val pp_type_val : Format.formatter -> type_val -> unit
val show_type_val : type_val -> string

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check

exception Unimplemented of string

module VarSet : Set.S with type elt = VarId.t

module Scheme : sig
  type t

  val occurs_in : int -> type_val -> bool
  val free_vars : type_val -> VarSet.t
end

module TypeEnv : sig
  type t

  val extend : t -> string -> Scheme.t -> t
  val empty : t
  val init : (string * Scheme.t) list -> t
end
