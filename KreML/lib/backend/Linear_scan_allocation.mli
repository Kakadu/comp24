(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Liveness_analysis

type 'a location =
  | Reg of 'a
  | StackLoc of int

type 'a regs_assignment = (string, 'a location, Base.String.comparator_witness) Base.Map.t
type 'a program_allocation = (string * 'a regs_assignment) list

module Allocator (S : Registers_storage_intf.S) : sig
  val scan_fun : 'a S.t -> range list -> 'a regs_assignment
  val scan_program : 'a S.t -> Flambda.flstructure -> 'a program_allocation
end

val pp
  :  Format.formatter
  -> (Format.formatter -> 'a -> unit)
  -> 'a program_allocation
  -> unit
