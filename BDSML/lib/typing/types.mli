module VarId : sig
  type t

  val to_string : t -> string
  val create : int -> t
  val compare : t -> t -> int
  val ( + ) : t -> int -> t
end

type base_type =
  | TInt
  | TChar
  | TString
  | TBool

type type_val =
  | TVar of VarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TParametric of type_val * type_val (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)

val pp_type_val : Format.formatter -> type_val -> unit
val show_type_val : type_val -> string

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check

exception Unimplemented of string
