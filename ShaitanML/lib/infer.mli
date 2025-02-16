(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Occurs_check (** Type variable occurs inside type it must be unified with *)
  | `Unbound_variable of Ast.id (** Unbound variable *)
  | `Unification_failed of Typedtree.ty * Typedtree.ty (** Failed to unify two types *)
  | `Unreachable_state of string (** Unreachable state (e.g. empty binding list in let) *)
  | `Let_rec_lhs
  ]

val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  type t = Set.Make(Base.Int).t
end

type scheme = S of VarSet.t * Typedtree.ty

val run_infer
  :  Ast.structure
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result

val test_infer : string -> unit