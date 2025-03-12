(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The user can use these names, but they will be renamed later at the alpha conversion stage. *)
val forbidden_names : string list

val alpha_prefix : string
val me_prefix : string
val cc_prefix : string
val ll_prefix : string
val runtime_prefix : string
val anf_prefix : string
val with_pref : string -> string -> string

(** Reserved prefixes: The user can use them, but they will be renamed during the alpha conversion stage. *)
val reserved_prefs : string list

(** INTERNAL LIB *)

(** entry point -- just a name *)
val rt_'main' : string
