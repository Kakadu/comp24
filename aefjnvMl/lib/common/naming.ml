(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let alpha_prefix = "ac"
let cc_prefix = "cc"
let me_prefix = "me"
let ll_prefix = "ll"
let anf_prefix = "nf"
let runtime_prefix = "rt"
let with_pref pref name = pref ^ "_" ^ name

let reserved_prefs =
  [ alpha_prefix; me_prefix; cc_prefix; ll_prefix; anf_prefix; runtime_prefix ]
;;

let rt_'main' = "main"
let forbidden_names = [ rt_'main'; "_start" ]
