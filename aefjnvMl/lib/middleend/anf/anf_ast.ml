(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Match_elimination.Me_ast

type 'a scoped =
  | Local_id of 'a
  | Global_func of 'a
  | Global_var of 'a

type immexpr =
  | Imm_int of int
  | Imm_bool of bool
  | Imm_nil
  | Imm_unit
  | Imm_id of string scoped id_t

type cexpr =
  | C_immexpr of immexpr
  | C_apply of string scoped * immexpr * immexpr list
  | C_tuple of immexpr list
  | C_rlist of immexpr * immexpr list (** reversed list <- first elem -- nil *)
  | C_ifthenelse of immexpr * aexpr * aexpr

and aexpr =
  | A_let of string id_t * cexpr * aexpr
  | A_cexpr of cexpr

type anf_fun =
  { adec_name : string
  ; adec_args : string id_t * string id_t list
  ; adec_body : aexpr
  }

type anf_decl =
  | A_GlobalV of string id_t * aexpr
  | A_NonrecDecl of anf_fun
  | A_RecDecl of anf_fun * anf_fun list

type anf_prog = anf_decl list
