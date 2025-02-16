(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typing

type fresh = int

module VarSet : sig
  type t = (int, Base.Int.comparator_witness) Base.Set.t
end

module Scheme : sig
  type t = S of VarSet.t * ty
end

module TypeEnv : sig
  type t = (id, Scheme.t, Base.String.comparator_witness) Base.Map.t

  val empty : t
  val equal : t -> t -> bool
end

val run_infer : ?env:TypeEnv.t -> decl -> (ty, error) result
val typecheck : ?env:TypeEnv.t -> program -> (TypeEnv.t, error) result
