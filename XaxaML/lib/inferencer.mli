(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of Typedtree.typ * Typedtree.typ
  | Multiple_bound of string
  | Recursive_binding
  | Impossible_state

val pp_error : Format.formatter -> error -> unit

module TypeEnv : sig
  type t

  val pp_env : Format.formatter -> t -> unit
end

val run_infer_expr : Ast.expr -> (Typedtree.typ, error) result
val run_infer_program : Ast.program -> (TypeEnv.t, error) result
