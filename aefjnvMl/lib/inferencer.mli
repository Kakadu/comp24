(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Typedtree
open Common.Errors

val check_program
  :  structure_item list
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result

module PP : sig
  val pp_type : Format.formatter -> ty -> unit

  val pp_program
    :  Format.formatter
    -> (string, scheme, Base.String.comparator_witness) Base.Map.t
    -> unit

  val pp_error : Format.formatter -> infer_error -> unit
end
