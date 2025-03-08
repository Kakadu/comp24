(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib.Closure
open MEML_lib.Anf
open MEML_lib.Lambdalift
open MEML_lib.Parser
open MEML_lib.Inferencer
open MEML_lib.PprinterTY
open MEML_lib.Codegen

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match parser s with
  | Ok ast ->
    (match run_infer ast with
     | Ok _ ->
       let codegen = codegen @@ anf @@ lambda_lift @@ closure ast in
       (match codegen with
        | Ok llvalue_list ->
          Base.List.iter llvalue_list ~f:(fun f ->
            Format.printf "%s\n" (Llvm.string_of_llvalue f))
        | Error e -> Stdlib.Format.printf "Comp Error: %s" e)
     | Error e -> Format.printf "Infer Error: %a\n" pp_error e)
  | Error message -> Format.printf "Parser Error: %s\n" message
;;
