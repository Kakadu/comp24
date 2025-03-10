(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
module Format = Stdlib.Format

val pp : (Format.formatter -> 'a -> unit) -> 'a t -> string -> unit
(**
  Run parser on string and pretty print the output using printer.
  Used for inline expect tests
*)
