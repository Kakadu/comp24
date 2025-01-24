(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Parser.run input with
  | Ok structure ->
    (match Inferencer.run structure with
     | Ok _ ->
       let alpha = Alpha_transformer.transform structure in
       let arities, anf = Anf.transform alpha in
       let flstructure = Closure_conversion.cc arities anf in
       Llvm_codegen.dump flstructure
     | Error error ->
       fprintf
         std_formatter
         "An error occured while type checking: %a"
         Inferencer.pp_error
         error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
