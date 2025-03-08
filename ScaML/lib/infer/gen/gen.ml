(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module BoundVars = Pattern.BoundVars
module Assumptions = Common.Assumptions
module ConArityAssumpt = Common.ConArityAssumpt

open Common

let gen str_item =
  GenMonad.run @@ Structure.gen str_item
  |> Result.map
       ~f:(fun ((asm, bound, ty, defined_types), constrs, con_assumpt) ->
         (asm, bound, ty, constrs, con_assumpt, defined_types) )
