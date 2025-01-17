open Ast

module StringSet : sig 
    include module type of Stdlib.Set.Make(String)
 end

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lclosure of closure
  | Lite of lambda * lambda * lambda
  | Lapp of lambda * lambda
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)
[@@deriving show]

and closure =
  { body : lambda
  ; env : closure_env
  ; total_args : int
  ; applied_count : int
  ; freevars : StringSet.t
  }
[@@deriving show]

and closure_env = (ident * lambda) list [@@deriving show]

and global_value =
  | Fun of closure
  | Var of lambda
[@@deriving show]

type lprogram = (ident * global_value) list [@@deriving show]

val apply_arg : closure -> ident -> lambda -> lambda
val iconst : int -> lambda
val lvar : ident -> lambda
val lapp : lambda -> lambda -> lambda
val llet : ident -> lambda -> lambda -> lambda
val lclosure : lambda -> int ->  StringSet.t -> lambda

val pp_lam : Stdlib.Format.formatter -> lambda -> unit

val pp : Stdlib.Format.formatter -> lprogram -> unit
