[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LAst
open LTypes

open ICommon

module As = IGCommon.As
module Bounds = IGCommon.Bounds

type output =
  { defined_types: DefTys.t
  ; assumptions: As.t
  ; bounds: Bounds.t
  ; constraints: ConSet.t
  ; ty: Ty.t option }

(** Tries to generate type constraints for structure item *)
let gen (deftys : DefTys.t) (item : StrItem.t) : (output, IError.t) Result.t =
  let open Result in
  let constraints, res = IGCommon.IGMonad.run (IGStr.gen deftys item) in
  res
  >>| fun (defined_types, assumptions, bounds, ty) ->
  {defined_types; assumptions; bounds; constraints; ty}
