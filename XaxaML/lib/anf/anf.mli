(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type aexp =
  | Ae_int of int
  | Ae_bool of bool
  | Ae_unit
  | Ae_val of string
  | Ae_empty_list
  | Ae_tuple of aexp list
[@@deriving show { with_path = false }]

type cexp =
  | Ce_atom of aexp
  | Ce_app of string * aexp list
  | Ce_ite of aexp * exp * exp
  | Ce_cons_list of aexp * aexp
[@@deriving show { with_path = false }]

and exp =
  | E_let_in of string * cexp * exp
  | E_complex of cexp
[@@deriving show { with_path = false }]

type func = string * string list * exp [@@deriving show { with_path = false }]

type toplevel =
  | Value of string * exp
  | Non_rec of func
  | Rec of func list
[@@deriving show { with_path = false }]

type anf_program = toplevel list [@@deriving show { with_path = false }]
type error = IncorrectAst of string

module Convert : sig
  val to_ast : anf_program -> Ast.program
end

module PP : sig
  val pp_anf_program : Format.formatter -> anf_program -> unit
  val pp_error : Format.formatter -> error -> unit
end

val run_to_anf
  :  Common.NamesHolder.t
  -> int
  -> Remove_patterns.rp_program
  -> (anf_program, error) Result.t
