open Ast

module Closure_env : sig
  type t

  val lookup : ident -> expr
end

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lclosure of lambda * Closure_env.t
  | Lapp of lambda * lambda
  | Lite of lambda * lambda * lambda
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)

type lprogram = lambda list

val cc : structure -> lprogram
