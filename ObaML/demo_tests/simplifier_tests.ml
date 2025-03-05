(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.structure_from_string s with
  | Ok structure ->
    (match Inferencer.run_structure_infer structure with
     | Ok _ ->
       let simple_structure = To_simple_ast.convert structure in
       Format.printf "%a" Simple_ast_pretty_printer.print_structure simple_structure
     | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
  | Error err -> Format.printf "Parser: %s\n" err
;;
