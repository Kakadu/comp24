(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Types

module Scheme : sig
  type t = VarSet.t * type_val

  val create : VarSet.t -> type_val -> t
  val free_vars : t -> VarSet.t
  val get_type : t -> type_val
end

module TypeEnv : sig
  type t

  val find : string -> t -> Scheme.t option
  val extend : t -> string -> Scheme.t -> t
  val empty : t
  val init : (string * Scheme.t) list -> t
  val free_vars : t -> VarSet.t
  val apply : Subst.t -> t -> t
  val diff : t -> t -> t
  val to_list : t -> (string * Scheme.t) list
end
