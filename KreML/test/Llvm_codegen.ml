(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Cursedml_lib

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Parser.run input with
  | Ok structure ->
    (match Inferencer.run structure with
     | Ok _ ->
       let match_free_program = Match_elimination.eliminate structure in
       let alpha = Alpha_transformer.transform match_free_program in
       let arities, anf = Anf.transform alpha in
       let flstructure = Closure_conversion.cc arities anf |> fst in
       Llvm_codegen.dump flstructure
     | Error error ->
       fprintf
         std_formatter
         "An error occured while type checking: %a"
         Inferencer.pp_error
         error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
