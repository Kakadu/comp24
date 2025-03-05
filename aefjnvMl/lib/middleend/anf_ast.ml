(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Match_elimination.Me_ast

(* TODO: MB ADD SCOPE CHECKING -- put Global funcs in specific type (reformat name and me_ident) *)

type immexpr =
  | Imm_int of int
  | Imm_bool of bool
  | Imm_nil
  | Imm_unit (** mb remove *)
  | Imm_id of me_ident

type cexpr =
  | C_apply of string * immexpr * immexpr list
  | C_ifthenelse of immexpr * aexpr * aexpr
  | C_tuple of immexpr list
  | C_rlist of immexpr * immexpr list (** reversed list <- first elem -- nil *)
  | C_immexpr of immexpr

and aexpr =
  | A_let of me_ident * cexpr * aexpr
  | A_cexpr of cexpr

type anf_fun =
  { adec_name : string
  ; adec_args : me_ident * me_ident list
  ; adec_body : aexpr
  }

type anf_decl =
  | A_GlobalV of me_ident * aexpr
  | ADMutualRecDecl of Common.Ast.rec_flag * anf_fun * anf_fun list

type anf_prog = anf_decl list
