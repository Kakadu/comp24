(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

let pp_parse_result parser printer str =
  parse parser str
  |> function
  | Result.Ok res -> Format.printf "%a" printer res
  | Result.Error s -> Format.eprintf "Error%s" s
;;
