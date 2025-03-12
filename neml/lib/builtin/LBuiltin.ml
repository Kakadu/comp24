[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Llvm

open LMisc
open LTypes
open LBack

module Make (LLModule : sig
  val lmod : llmodule
end) (LLRuntime : sig
  val print_int : llvalue -> llbuilder -> llvalue
end) =
struct
  let lmod = LLModule.lmod
  let lctx = module_context lmod
  let i64 = i64_type lctx

  let define_function id typ bld =
    let func = define_function id typ lmod in
    add_function_attr func (create_enum_attr lctx "alwaysinline" 0L) Function ;
    position_at_end (entry_block func) bld ;
    func

  let llvm_binop (id : Id.t)
      (build_op : llvalue -> llvalue -> string -> llbuilder -> llvalue) :
      BCodegen.builtin =
    let (LId id as lid) = LLId.from_tagged (id, User) in
    fun bld ->
      let typ = function_type i64 [|i64; i64|] in
      let func = define_function id typ bld in
      let ret = build_op (param func 0) (param func 1) "r" bld in
      let ret = build_intcast ret i64 "r" bld in
      let _ = build_ret ret bld in
      (lid, func, typ)

  let llvm_print_int (id : Id.t) : BCodegen.builtin =
    let (LId id as lid) = LLId.from_tagged (id, User) in
    fun bld ->
      let typ = function_type i64 [|i64|] in
      let func = define_function id typ bld in
      let ret = LLRuntime.print_int (param func 0) bld in
      let _ = build_ret ret bld in
      (lid, func, typ)

  let ty s = LParse.parse_ty s |> Option.value_exn
  let ty_binop = ty "int -> int -> int"
  let ty_cmp_binop = ty "int -> int -> bool"

  let builtins : (Id.t * Ty.t * BCodegen.builtin) list =
    [ (let id = Id.I "+" in
       (id, ty_binop, llvm_binop id build_add) )
    ; (let id = Id.I "-" in
       (id, ty_binop, llvm_binop id build_sub) )
    ; (let id = Id.I "*" in
       (id, ty_binop, llvm_binop id build_mul) )
    ; (let id = Id.I "/" in
       (id, ty_binop, llvm_binop id build_sdiv) )
    ; (let id = Id.I "=" in
       (id, ty_cmp_binop, llvm_binop id (build_icmp Eq)) )
    ; (let id = Id.I "<" in
       (id, ty_cmp_binop, llvm_binop id (build_icmp Slt)) )
    ; (let id = Id.I "<=" in
       (id, ty_cmp_binop, llvm_binop id (build_icmp Sle)) )
    ; (let id = Id.I "print_int" in
       (id, ty "int -> unit", llvm_print_int id) ) ]
end
