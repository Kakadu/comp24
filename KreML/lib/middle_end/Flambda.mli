(** Copyright 2024-2025, KreML Compiler Commutnity *)

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
  | Fl_getfield of int * flambda
  | Fl_cons of flambda * flambda
  | Fl_tuple of flambda list
  | Fl_app of flambda * flambda list
  | Fl_closure of closure
  | Fl_ite of flambda * flambda * flambda
  | Fl_let of ident * flambda * flambda

and closure =
  { name : ident
  ; env_size : int
  ; arrange : (int * flambda) list (* idx, value*)
  }

type fun_with_env =
  { arg : ident
  ; captured_args : ident list
  ; arity : int
  ; body : flambda
  }

type fl_fun =
  | Fun_with_env of fun_with_env
  | Fun_without_env of ident option * flambda (** [Fun_without_env(arg, body)] *)

type flstructure = (ident * fl_fun) list

val flvar : ident -> flambda
val pp : Stdlib.Format.formatter -> flstructure -> unit
