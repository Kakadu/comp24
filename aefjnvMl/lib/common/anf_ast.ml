(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id =
  | Id_name of string
  | Id_anf of int * string (** [int] -- uniq id; [string] -- original name *)
  | Id_any
  | Id_unit

type immexpr =
  | ImmInt of int
  | ImmId of id
  | ImmBool of bool
  | ImmNil
  | ImmTuple of immexpr list

type cexpr =
  | CApp of cexpr * cexpr
  | CIte of immexpr * aexpr * aexpr
  | CList of immexpr * immexpr
  | CImmExpr of immexpr

and aexpr =
  | ALetIn of id * cexpr * aexpr
  | ACExpr of cexpr

type anf_bind = ALet of id * id list * aexpr

type anf_decl =
  | ADSingleLet of anf_bind
  | ADMutualRecDecl of anf_bind list (**List.length >= 2 *)

type anf_prog = anf_decl list