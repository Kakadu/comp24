[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom

open LAst

let parse : string -> structure option =
 fun s -> parse_string ~consume:All PStr.p s |> Result.ok
