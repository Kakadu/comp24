[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Llvm

module Make (LLModule : sig
  val lmod : llmodule
end) : sig
  val create_closure : func:llvalue -> argc:int -> llbuilder -> llvalue
  val apply_closure :
    closure:llvalue -> args:llvalue list -> llbuilder -> llvalue
  val print_int : llvalue -> llbuilder -> llvalue
end = struct
  let lmod = LLModule.lmod
  let i64 = i64_type (module_context lmod)

  module Func = struct
    module T = struct
      type t = CreateClosure | ApplyClosure | PrintInt
      [@@deriving ord, sexp_of]
    end
    include T
    include Comparator.Make (T)
  end

  let declare id typ =
    let func = declare_function id typ lmod in
    (func, typ)

  let funcs =
    Map.of_alist_exn
      (module Func)
      [ ( CreateClosure
        , declare "neml_create_closure" (function_type i64 [|i64; i64|]) )
      ; ( ApplyClosure
        , declare "neml_apply_closure" (var_arg_function_type i64 [|i64; i64|])
        )
      ; (PrintInt, declare "neml_print_int" (function_type i64 [|i64|])) ]

  let create_closure ~func ~argc bld =
    let fcreate, fcreate_type = Map.find_exn funcs CreateClosure in
    build_call fcreate_type fcreate [|func; const_int i64 argc|] "r" bld

  let apply_closure ~closure ~args bld =
    let fapply, fapply_type = Map.find_exn funcs ApplyClosure in
    let argc = List.length args in
    build_call fapply_type fapply
      (Array.of_list (closure :: const_int i64 argc :: args))
      "r" bld

  let print_int value bld =
    let fprint, fprint_type = Map.find_exn funcs PrintInt in
    build_call fprint_type fprint [|value|] "r" bld
end
