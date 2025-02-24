type reg =
  | Zero
  | Ra
  | Sp
  | Temp of int (** t{i} *)
  | Saved of int (** s{i} *)
  | Arg of int (** a{i} *)

val temp : int -> reg
val saved : int -> reg
val arg : int -> reg
val fp: reg
val is_saved : reg -> bool

type rvalue =
  | Rv_imm of int
  | Rv_reg of reg
  | Rv_mem of reg * int (** base, offset *)
  | Rv_function of Flambda.fun_decl
  | Rv_nop

val a0value : rvalue

type location =
  | Loc_reg of reg
  | Loc_mem of reg * int

val idc_loc : location

type op =
  (* R-type *)
  | ADD
  | MUL
  | DIV
  | SLT
  | SLTU
  | AND
  | OR
  | XOR
  | SLL
  | SRL
  | SUB
  | SRA
  (* I-type *) (* reg < imm*) (* reg > imm ?*)
  | ADDI
  | SLTI
  | SLTIU
  | ANDI
  | ORI
  | XORI
  | LW
  | LH
  | LHU
  | LB
  (* S-type *)
  | SW
  | SH
  | SB
  (* B-type *)
  | BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  (* U-type *)
  | LUI
  | AUIPC
  (* J-type *)
  | JAL
  | JALR


type instruction =
  | Rtype of reg * reg * reg * op (** rd, rs1, rs2, op *)
  | Itype of reg * reg * int * op (** rd, rs1, imm, op *)
  | Stype of reg * int * reg * op (** base, offset, value, op *)
  | Btype of reg * reg * string * op (** rs1, rs2, target, op *)
  | Utype of reg * int * op (** dst, imm, op *)
  (* | Jtype of string * reg * op *)
   (** signed offset, link reg, op *)
  | Pseudo of string (* can not unify signature *)
  | Label of string

val extend_stack_insn : int -> instruction
val shrink_stack_insn : int -> instruction
val sw : v:reg -> int -> dst: reg -> instruction
val lw : rd:reg -> int -> src:reg -> instruction

module RegistersStorage : sig
  include Registers_storage_intf.S with type 'a t = 'a list
end

val available_regs : reg list
val temporary_regs : reg list


val pp_reg : Format.formatter -> reg -> unit
val pp_op : Format.formatter -> op -> unit
val pp_insn : Format.formatter -> instruction -> unit

module Pseudo : sig
  val mv: rd:reg -> src:reg -> instruction
  val li: reg -> int -> instruction
  val la : reg -> string -> instruction
  val neg: reg -> instruction
  val seqz : rd:reg -> src:reg -> instruction
  val call: string -> instruction
  val jump: string -> instruction
  val ret: instruction
end


