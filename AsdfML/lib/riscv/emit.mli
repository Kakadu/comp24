(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val stack_pos : int ref
val code : (Machine.instr * string) Base.Queue.t
val emit : ?comm:string -> ((Machine.instr -> unit) -> 'a) -> 'a
val set_code : unit -> unit
val set_fn_code : unit -> unit
val flush_fn : unit -> unit
val emit_store : ?comm:string -> Machine.reg -> Machine.loc
val emit_fn_decl : string -> string list -> int -> unit
val emit_fn_ret : int -> unit
val emit_load : ?comm:string -> Machine.loc -> Machine.rvalue -> unit
val emit_load_reg : ?comm:string -> Machine.reg -> Machine.rvalue -> unit
val emit_load_tuple_field : ?comm:string -> Machine.reg -> Machine.loc -> unit
val emit_fn_call : string -> Machine.rvalue list -> Machine.reg
val dump_reg_args_to_stack : string list -> (string * Machine.loc) list
val is_direct_unop : string -> bool
val is_direct_binop : string -> bool
val emit_direct_unop : ?comm:string -> Machine.reg -> string -> Machine.reg -> unit
val emit_direct_binop
  :  ?comm:string
  -> Machine.reg
  -> string
  -> Machine.reg
  -> Machine.reg
  -> unit
