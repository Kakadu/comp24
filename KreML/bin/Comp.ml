(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* alpha conv -> match elim -> anf -> clos conv *)
open Cursedml_lib

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Parser.run input with
  | Ok structure ->
    (match Cursedml_lib.Inferencer.run structure with
     | Ok _ ->
       let mf_structure = Match_elimination.eliminate structure in
       let alpha_structure = Alpha_transformer.transform mf_structure in
       let arities, anf_structure = Anf.transform alpha_structure in
       (* Anf. std_formatter anf_structure; *)
       let flstructure = Closure_conversion.cc arities anf_structure in
       let mdl = Llvm_codegen.get_module flstructure in
       Llvm.print_module "out.ll" mdl
     | Error error ->
       fprintf
         std_formatter
         "An error occured while type checking: %a"
         Inferencer.pp_error
         error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
