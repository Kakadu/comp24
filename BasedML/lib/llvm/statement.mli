(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type ('st, 'a) t = 'st -> 'st * ('a, string) result

val return : 'a -> 'b -> 'b * ('a, 'c) result
val fail : 'a -> 'b -> 'b * ('c, 'a) result
val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val run : ('st, 'a) t -> 'st -> 'st * ('a, string) result
val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
val ( *> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
val ( >>| ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
val ( <* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b) t
val map_list : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t

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

type local_map = Llvm.llvalue MapString.t
type glob_funs = (Llvm.lltype * Llvm.llvalue) MapString.t
type state = string * glob_funs * local_map

val read_curr_fun : (state, string) t
val write_curr_fun : string -> (state, unit) t
val findopt_glob_fun : string -> (state, (Llvm.lltype * Llvm.llvalue) option) t
val add_glob_fun : string -> Llvm.lltype -> Llvm.llvalue -> (state, unit) t
val findopt_loc_var : string -> (state, Llvm.llvalue option) t
val add_loc_var : string -> Llvm.llvalue -> (state, unit) t
val new_fun_scope : string -> (state, 'a) t -> (state, 'a) t
