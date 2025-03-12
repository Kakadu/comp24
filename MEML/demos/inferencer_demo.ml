(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib.Inferencer
open MEML_lib.Parser
open MEML_lib.PprinterTY

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match parser s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
         match TypeEnv.find v start_env with
         | Some (S (_, init_ty)) when Stdlib.(ty = init_ty) -> ()
         | _ -> Stdlib.Format.printf "val %s: %a\n" v pp_typ ty)
     | Error e -> Format.printf "Infer Error %a\n" pp_error e)
  | Error message -> Format.printf "Parser Error: %s\n" message
;;
