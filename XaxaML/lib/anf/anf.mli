(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type aexp =
  | Ae_int of int
  | Ae_bool of bool
  | Ae_unit
  | Ae_val of string
  | Ae_empty_list

type cexp =
  | Ce_atom of aexp
  | Ce_app of aexp * aexp list
  | Ce_ite of aexp * exp * exp

and exp =
  | E_let_in of string * cexp * exp
  | E_complex of cexp

type func = string * string list * exp

type toplevel =
  | Value of string * exp
  | Non_rec of func
  | Rec of func list

type anf_program = toplevel list
type error = IncorrectAst of string

module Convert : sig
  val to_ast : anf_program -> Ast.program
end

module PP : sig
  val pp_anf_program : Format.formatter -> anf_program -> unit
  val pp_error : Format.formatter -> error -> unit
end

val run_to_anf_program
  :  int
  -> Remove_patterns.rp_program
  -> (anf_program, error) Result.t
