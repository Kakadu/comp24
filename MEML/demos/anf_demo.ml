(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib.Closure
open MEML_lib.Anf
open MEML_lib.PprinterANF
open MEML_lib.Lambdalift
open MEML_lib.Parser
open MEML_lib.Inferencer
open MEML_lib.PprinterTY
open MEML_lib.Anf_to_ast

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match parser s with
  | Ok ast ->
    (match run_infer ast with
     | Ok _ ->
       let anf = anf @@ lambda_lift @@ closure ast in
       let ast = anf_to_ast anf in
       (match run_infer ast with
        | Ok env ->
          Format.printf "Types: \n";
          Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
            match TypeEnv.find v start_env with
            | Some (S (_, init_ty)) when Stdlib.(ty = init_ty) -> ()
            | _ -> Format.printf "val %s: %a\n" v pp_typ ty);
          Format.printf "\n";
          Format.print_string @@ pp_anf anf
        | Error e -> Format.printf "Infer Error %a\n" pp_error e)
     | Error e -> Format.printf "Infer Error: %a\n" pp_error e)
  | Error message -> Format.printf "Parser Error: %s\n" message
;;
