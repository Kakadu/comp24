(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Middleend.Ast_mapper.Mapper (Middleend.Ast_mapper.Result)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast ->
    let trans = declarations_of_sexpr (sexpr_of_declarations ast) in
    (match trans with
     | Ok aa -> Format.printf "%a\n" Ast.pp_declarations aa
     | Error m -> Format.printf "Error: %s\n" m)
  | Error message -> Format.printf "Error: %s\n" message
;;
