(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val word : int
val n_reg_args : int
type reg =
  | SP
  | Reg of string
  | Temp of int
val equal_reg : reg -> reg -> bool
val pp_reg : Format.formatter -> reg -> unit
val show_reg : reg -> string
type offset = reg * int
val equal_offset : offset -> offset -> bool
val pp_offset : Format.formatter -> offset -> unit
val show_offset : offset -> string
type rvalue =
  | RInt of int
  | RFn of string
  | RReg of reg
  | ROffset of offset
type loc =
  | LReg of reg
  | LMem of offset
  | LArr of loc * int
val pp_loc : Format.formatter -> loc -> unit
val show_loc : loc -> string
val loc_to_rvalue : loc -> rvalue
val pp_reg : Format.formatter -> reg -> unit
val pp_offset : Format.formatter -> reg * int -> unit
val pp_rvalue : Format.formatter -> rvalue -> unit
type instr =
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Addi of reg * reg * int
  | Xori of reg * reg * int
  | Xor of reg * reg * reg
  | Slt of reg * reg * reg
  | Seqz of reg * reg
  | Snez of reg * reg
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Ld of reg * offset
  | Sd of reg * offset
  | Li of reg * int
  | La of reg * string
  | Mv of reg * reg
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | J of string
  | Label of string
  | Call of string
  | ECall
  | Ret
  | Comment of string
  | Str of string
val pp_instr : Format.formatter -> instr -> unit
val zero : reg
val ra : reg
val sp : reg
val s0 : reg
val fp : reg
val t0 : reg
val t1 : reg
val t2 : reg
val t3 : reg
val t4 : reg
val t5 : reg
val t6 : reg
val a0 : reg
val a1 : reg
val a2 : reg
val a3 : reg
val a4 : reg
val a5 : reg
val a6 : reg
val a7 : reg
val arg_regs : reg list
val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val div : (instr -> 'a) -> reg -> reg -> reg -> 'a
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val xori : (instr -> 'a) -> reg -> reg -> int -> 'a
val xor : (instr -> 'a) -> reg -> reg -> reg -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val seqz : (instr -> 'a) -> reg -> reg -> 'a
val snez : (instr -> 'a) -> reg -> reg -> 'a
val and_ : (instr -> 'a) -> reg -> reg -> reg -> 'a
val or_ : (instr -> 'a) -> reg -> reg -> reg -> 'a
val ld : (instr -> 'a) -> reg -> offset -> 'a
val sd : (instr -> 'a) -> reg -> offset -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val la : (instr -> 'a) -> reg -> string -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val blt : (instr -> 'a) -> reg -> reg -> string -> 'a
val j : (instr -> 'a) -> string -> 'a
val label : (instr -> 'a) -> string -> 'a
val call : (instr -> 'a) -> string -> 'a
val ecall : (instr -> 'a) -> 'a
val ret : (instr -> 'a) -> 'a
val comment : (instr -> 'a) -> string -> 'a
val str : (instr -> 'a) -> string -> 'a
