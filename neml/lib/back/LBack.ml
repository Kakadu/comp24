[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

module LLId = BCommon.LLId
module LLRuntime = BRuntime

module BCodegen : sig
  open Llvm

  type err = TypeError of string | NotImplemented of string
  val pp_err : Stdlib.Format.formatter -> err -> unit

  type builtin = llbuilder -> LLId.t * llvalue * lltype

  module LLCodeGen : functor
    (_ : sig
       val lmod : llmodule
     end)
    (_ : sig
       val create_closure : func:llvalue -> argc:int -> llbuilder -> llvalue
       val apply_closure :
         closure:llvalue -> args:llvalue list -> llbuilder -> llvalue
     end)
    -> sig
    val gen : builtins:builtin list -> LMiddle.MAnf.t -> (unit, err) Result.t
  end
end =
  BCodegen
