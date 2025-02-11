(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.structure_from_string s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_structure ast
  | Error _ -> Format.printf "Syntax error"
;;
