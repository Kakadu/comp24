(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module TypeEnv : sig 
    type t 

    val pretty_pp_env : Format.formatter -> t -> unit
end

val run_stucture_infer : Ast.structure -> (TypeEnv.t, Typedtree.error) result