open Ast

module StringSet : sig
  include module type of Stdlib.Set.Make (String)
end

type binop =
  | Mul
  | Div
  | Plus
  | Minus
  | Eq
  | Gt
  | Geq
  | Lt
  | Leq
  | And
  | Or

val resolve_binop : ident -> binop

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lbinop of binop * lambda * lambda
  | Lclosure of closure
  (** Represents code and its environment until it can be called.
     Futhermore it should be transofrmed to Lcall*)
  | Lite of lambda * lambda * lambda
  | Lcall of ident * lambda list * closure_env option
  (** [LCall(id, args, env)] represents executable code with arg and optional environment*)
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)
[@@deriving show]

and closure =
  { name : ident
  ; body : lambda
  ; env : closure_env
  ; arg_patterns : pattern list
  ; total_args : int
  ; applied_count : int
  ; freevars : StringSet.t
  }
[@@deriving show]

and closure_env = (ident * lambda) list [@@deriving show]

type lprogram = (ident * lambda) list [@@deriving show]

val lookup_closure_env_exn : closure_env -> ident -> lambda
val apply_arg : closure -> ident -> lambda -> closure
val iconst : int -> lambda
val lvar : ident -> lambda
val lcall : ident -> ?env:closure_env -> lambda list -> lambda
val llet : ident -> lambda -> lambda -> lambda
val lclosure : ident -> lambda -> pattern list -> StringSet.t -> closure
val pp_lam : Stdlib.Format.formatter -> lambda -> unit
val pp : Stdlib.Format.formatter -> lprogram -> unit
