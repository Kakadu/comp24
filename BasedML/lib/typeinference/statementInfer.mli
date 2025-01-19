(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type ('st, 'a) t = 'st -> 'st * ('a, string) result

val return : 'a -> 'b -> 'b * ('a, 'c) result
val fail : 'a -> 'b -> 'b * ('c, 'a) result
val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val read : ('st, 'st) t
val write : 'st -> ('st, unit) t
val run : ('st, 'a) t -> 'st -> 'st * ('a, string) result
val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
val ( *> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
val ( >>| ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
val ( <* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b) t
val map_list : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t

type env_map = Help.type_form Help.MapString.t

val pp_env_map : Format.formatter -> env_map -> unit
val show_env_map : env_map -> string

type tv_num = int
type state = env_map * Substitution.substitution_list * tv_num

val fresh_tv : (state, Ast.type_name) Substitution.t
val read_env : (state, env_map) Substitution.t
val write_env : env_map -> (state, unit) Substitution.t
val read_var_type : string -> (state, Ast.type_name option) Substitution.t
val write_var_type : string -> Help.type_form -> (state, unit) Substitution.t
val write_flat_var_type : string -> Ast.type_name -> (state, unit) Substitution.t

val write_scheme_for_pattern
  :  Help.SetPolyType.t
  -> Ast.pattern
  -> Ast.type_name
  -> (state, unit) Substitution.t

val read_subs : (state, Substitution.substitution_list) Substitution.t
val write_subst : Ast.type_name -> Ast.type_name -> (state, unit) Substitution.t
val restore_type : Ast.type_name -> (state, Ast.type_name) Substitution.t
val get_tv_from_env : env_map -> Help.SetPolyType.t
