(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf

module StringSet : sig
  include module type of Stdlib.Set.Make (String)
end

type flambda =
  | Fl_const of const
  | Fl_var of ident
  | Fl_binop of binop * flambda * flambda
  | Fl_unop of unop * flambda
  | Fl_getfield of int * flambda
  | Fl_cons of flambda * flambda
  | Fl_tuple of flambda list
  | Fl_app of flambda * flambda list
  | Fl_closure of closure
  | Fl_ite of flambda * flambda * flambda
  | Fl_let of ident option * flambda * flambda

and closure =
  { name : ident
  ; env_size : int
  ; arrange : (int * flambda) list (* idx, value*)
  ; arity : int
  }

type fun_decl =
  { param_names : ident list (* 1. args 2. freevars*)
  ; arity : int
  ; body : flambda
  }

type fl_fun =
  | Fun_with_env of fun_decl
  | Fun_without_env of fun_decl

type flstructure = (ident * fl_fun) list

val flvar : ident -> flambda
val pp : Stdlib.Format.formatter -> flstructure -> unit
