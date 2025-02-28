(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Liveness_analysis

module type S = sig
  type 'a t

  val remove : 'a -> 'a t -> 'a t
  val add : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val pop : 'a t -> 'a * 'a t
  val with_ranges : 'a t -> range list -> ('a * range) t
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val find : ('a -> bool) -> 'a t -> 'a
  val to_list : 'a t -> 'a list
  val empty : 'a t
end
