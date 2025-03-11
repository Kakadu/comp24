(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val eliminate_pm_program
  :  Common.IdentifierStructs.IdentifierSet.t
  -> LambdaLifting.Llast.ll_program
  -> (Pmfast.pmf_program * Common.IdentifierStructs.IdentifierSet.t) Common.StateMonad.t
