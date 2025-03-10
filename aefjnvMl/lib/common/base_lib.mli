(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module LibF : sig
  open Ast

  type rt_alias = Alias of string

  type vararg =
    | Varargs of core_type
    | NoVarargs

  type env_func =
    | StdF of string * rt_alias * (int * Ast.core_type * vararg)
    | SysF of string * rt_alias * (int * Ast.core_type * vararg)

  val get_row_func : env_func -> string * rt_alias * (int * Ast.core_type * vararg)
  val get_name : env_func -> ident
  val get_rt_alias : env_func -> string
  val get_type : env_func -> int * Ast.core_type * vararg
  val get_row_type : env_func -> vararg * (core_type list * core_type)
  val vararg : string
  val un : ident -> ident
  val f'sub' : ident

  (* DECLS: *)

  val op_mul : env_func
  val op_div : env_func
  val op_plus : env_func
  val op_minus : env_func
  val op_less_eq : env_func
  val op_more_eq : env_func
  val op_less : env_func
  val op_more : env_func
  val op_eq2 : env_func
  val op_eq : env_func
  val op_not_eq : env_func
  val op_and : env_func
  val op_or : env_func
  val un_op_minus : env_func
  val func_print_int : env_func

  (*  *)
  val get_by_idx : env_func
  val get_list_len : env_func
  val get_list_tail : env_func
  val part_match_fail : env_func
  val create_empty_closure : env_func
  val apply_arguments : env_func
  val alloc_tuple : env_func
  val append_to_list : env_func
end

val user_funcs : LibF.env_func list
val sys_funcs : LibF.env_func list
val base_lib_decls : LibF.env_func list
val get_op_decls : (string * LibF.rt_alias * (int * Ast.core_type * LibF.vararg)) list
val get_infix_ops : (string * LibF.rt_alias * (int * Ast.core_type * LibF.vararg)) list
val get_un_ops : (string * LibF.rt_alias * (int * Ast.core_type * LibF.vararg)) list
val find_by_name : string -> LibF.env_func
val is_binop : string -> bool
val converte_op : string -> string
