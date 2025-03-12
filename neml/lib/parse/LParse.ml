[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom

open LAst

let parse : string -> structure option =
 fun s -> parse_string ~consume:All PStr.p s |> Result.ok

let parse_ty : string -> LTypes.Ty.t option =
 fun s -> parse_string ~consume:All PTy.p s |> Result.ok
