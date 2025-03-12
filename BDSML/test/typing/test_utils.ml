(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Typing

let ( >>= ) a b =
  match a with
  | Ok x -> b x
  | Error _ as e -> e
;;

let print_types = function
  | Result.Ok res -> Typing.Types.print_types res
  | Result.Error s -> Format.eprintf "Error%s" s
;;

let test str = Parser.Main_parser.parse str >>= Inference.infer_program |> print_types
