open Ast

module Varset : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit

  (* let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]" *)

end

module Type : sig
  val occurs : type_id -> typ -> bool
  val free_vars : typ -> Varset.t
end

type scheme = S of Varset.t * typ

module Subst : sig
  type t

  (* val pp : Stdlib.Format.formatter -> t -> unit *)

  val empty : t
  val singleton : type_id -> typ -> t
  val find : type_id -> t -> typ
  val unify : typ -> typ -> t
  val apply : typ -> t -> typ
  val compose : t -> t -> t
end

type error =
  | Occures_check
  | Unification_failed

type result =
  | Ok of expr
  | Err of error

val check_program : structure -> result