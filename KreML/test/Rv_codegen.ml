(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Cursedml_lib
open Cursedml_lib.Inferencer
module Rv_allocator = Linear_scan_allocation.Allocator (Riscv.RegistersStorage)

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Parser.run input with
  | Ok structure ->
    (match Inferencer.run structure with
     | Ok _ ->
       let alpha = Alpha_transformer.transform structure in
       let mf_structure = Match_elimination.eliminate alpha in
       let arities, anf = Anf.transform mf_structure in
       let ccinfo = Closure_conversion.cc arities anf in
       let insns = Riscv_codegen.codegen_program ccinfo in
       Riscv_codegen.dump insns
     | Error error ->
       fprintf std_formatter "An error occured while type checking: %a" pp_error error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
