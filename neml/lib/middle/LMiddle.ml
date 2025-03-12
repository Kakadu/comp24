[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

module IdSet = MCommon.IdSet
module IdTagged = MCommon.IdTagged
module FuncDef = MCommon.FuncDef

(**
  Middleend IRs.
  LAst.Expr.t -> MSimpl.t --MOpt.opt--> MSimpl.t -> MCLess.t -> MAnf.t
*)

module MSimpl = MSimpl
module MOpt = MOpt
module MCLess = MCLess
module MAnf = MAnf
