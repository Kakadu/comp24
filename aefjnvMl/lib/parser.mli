(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Errors

val parse : string -> (structure_item list, error) result
val parse_prefix : string -> (structure_item list, error) result

module PP : sig
  val pp_error : Format.formatter -> parse_error -> unit
end
