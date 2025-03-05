(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module TypeEnv : sig
  type t

  (** @param std: (string * Typedtree.scheme) list *)
  val pretty_pp_env : Format.formatter -> (string * Typedtree.scheme) list * t -> unit
end

(** @param std: (string * Typedtree.scheme) list *)
val run_structure_infer_with_custom_std
  :  Ast.structure
  -> (string * Typedtree.scheme) list
  -> (TypeEnv.t, Typedtree.error) result

val run_structure_infer : Ast.structure -> (TypeEnv.t, Typedtree.error) result
