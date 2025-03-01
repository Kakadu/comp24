(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AstLib.Ast
(* open Common.Base_bin_ops *)

type identifier =
  | Id of string
  | IdConstraint of string typed

type immexpr =
  | ImmConst of const
  | ImmIdentifier of ident
  | ImmTuple of immexpr list
  | ImmCons of immexpr * immexpr
  | ImmConstraint of immexpr typed

type cexpr =
  | CApp of cexpr * cexpr
  (* | CBinOp of bin_op * cexpr * cexpr *)
  | CIf of immexpr * aexpr * aexpr
  | CImmExpr of immexpr

and aexpr =
  | ALetIn of identifier * cexpr * aexpr
  | ACExpr of cexpr

  (* todo no patterns here *)
type let_body = identifier * identifier list * aexpr

type anf_decl =
  | ADSingleLet of rec_flag * let_body
  | ADMutualRecDecl of rec_flag * let_body list (**todo List.length >= 2 *)

type anf_prog = anf_decl list
