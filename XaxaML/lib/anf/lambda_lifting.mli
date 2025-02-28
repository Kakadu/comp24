(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val run
  :  Common.NamesHolder.t
  -> int
  -> Remove_patterns.rp_program
  -> Common.NamesHolder.t * int * Remove_patterns.rp_program
