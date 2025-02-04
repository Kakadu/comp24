(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Parser

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  match parse input with
  | Ok v -> Format.printf "%s\n" Ast.(show_program v)
  | Error _ -> Format.printf "Syntax error\n"
;;