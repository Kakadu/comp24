(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Smisc
open Tys

type types_arity = (Ident.t, int, Ident.comparator_witness) Map.t

type t =
  { map: (Ident.t, Scheme.t, Ident.comparator_witness) Map.t
  ; types_arity: types_arity }

let get_types_arity env = env.types_arity

let set_types_arity env types_arity = {env with types_arity}

let idents env = Map.keys env.map

let set env ~key ~data = {env with map= Map.set env.map ~key ~data}

let map env ~f = {env with map= Map.map env.map ~f}

let find env = Map.find env.map

let find_exn env = Map.find_exn env.map

let of_alist_exn list =
  { map= Map.of_alist_exn (module Ident) list
  ; types_arity= Map.empty (module Ident) }
