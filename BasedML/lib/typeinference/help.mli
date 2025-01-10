(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module PolyType : sig
  type t = Ast.poly_type

  val compare : 'a -> 'a -> int
end

module MapString : sig
  type key = string
  type 'a t = 'a Map.Make(String).t

  val empty : 'a t
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val bindings : 'a t -> (key * 'a) list
  val find_opt : key -> 'a t -> 'a option
  val of_seq : (key * 'a) Seq.t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module MapPolyType : sig
  type key = Ast.poly_type
  type 'a t = 'a Map.Make(PolyType).t

  val empty : 'a t
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val bindings : 'a t -> (key * 'a) list
  val find_opt : key -> 'a t -> 'a option
  val of_seq : (key * 'a) Seq.t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module SetPolyType : sig
  type elt = Ast.poly_type
  type t = Set.Make(PolyType).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val filter : (elt -> bool) -> t -> t
  val elements : t -> elt list
  val pp : Format.formatter -> t -> unit
end

type type_form =
  | TFFlat of Ast.type_name
  | TFSchem of SetPolyType.t * Ast.type_name

val pp_type_form : Format.formatter -> type_form -> unit
val show_type_form : type_form -> string
val get_tv_from_tp : SetPolyType.t -> Ast.type_name -> SetPolyType.t
