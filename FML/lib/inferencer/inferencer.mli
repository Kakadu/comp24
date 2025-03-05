(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Typedtree
open Inf_errors

module TypeEnv : sig
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t
end

val run_program_inferencer : declaration list -> (TypeEnv.t * id list, error) result
