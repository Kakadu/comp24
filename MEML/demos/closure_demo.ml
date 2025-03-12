(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib.Closure
open MEML_lib.Parser
open MEML_lib.Inferencer
open MEML_lib.PprinterCAST
open MEML_lib.PprinterTY

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match parser s with
  | Ok ast ->
    (match run_infer ast with
     | Ok _ ->
       let converted = closure ast in
       Format.print_string @@ pp_closure converted
     | Error e -> Format.printf "Infer Error: %a\n" pp_error e)
  | Error message -> Format.printf "Parser Error: %s\n" message
;;
