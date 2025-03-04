(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.structure_from_string s with
  | Ok structure ->
    (match Inferencer.run_stucture_infer structure with
     | Ok env -> Format.printf "%a" Inferencer.TypeEnv.pretty_pp_env env
     | Error err -> Format.printf "Infer: %a" Typedtree.pp_error err)
  | Error err -> Format.printf "Parser: %s\n" err
;;
