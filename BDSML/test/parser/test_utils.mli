(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val parse_with_parser : 'a Angstrom.t -> string -> ('a, string) result
val pp_result : (Format.formatter -> 'a -> unit) -> ('a, string) result -> unit
