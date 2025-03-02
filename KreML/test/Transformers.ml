(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Cursedml_lib.Inferencer
open Cursedml_lib

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Parser.run input with
  | Ok structure ->
    (match Inferencer.run structure with
     | Ok _ ->
       let alpha = Alpha_transformer.transform structure in
       let mf_structure = Match_elimination.eliminate alpha in
       let anf = Anf.transform mf_structure |> snd in
       Anf_printer.pp std_formatter anf
     | Error error ->
       fprintf std_formatter "An error occured while type checking: %a" pp_error error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
