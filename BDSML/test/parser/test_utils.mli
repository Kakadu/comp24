(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val pp_parse_result : 'a Angstrom.t -> (Format.formatter -> 'a -> unit) -> string -> unit
