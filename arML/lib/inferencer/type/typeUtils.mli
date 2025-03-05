(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Returns the set of variables of the types contained in type *)
val type_vars : TypeTree.typ -> TypeTree.TypeVarSet.t

(* Syntactic sugar *)

val ( @-> ) : TypeTree.typ -> TypeTree.typ -> TypeTree.typ

(* ---------------- *)
