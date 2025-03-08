(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Stdio
module Format = Stdlib.Format

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res ->
      printer Format.std_formatter res
  | Error _ ->
      print_endline "syntax error"
