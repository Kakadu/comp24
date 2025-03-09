(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val fun_exception : string -> Reduced_ast.rexpr
val exp_to_string : Reduced_ast.error -> string

module Monads :
  Utils.Counter_monad.S with type counter = Int.t with type error = Reduced_ast.error
