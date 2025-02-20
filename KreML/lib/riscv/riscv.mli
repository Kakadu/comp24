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
val is_saved : reg -> bool

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
  (* I-type *)
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
val sw : v:reg -> int -> src: reg -> instruction
val lw : rd:reg -> int -> src:reg -> instruction

module RegistersStorage : sig
  include Registers_storage_intf.S with type 'a t = 'a list
  (* val available :  reg t *)
end

val pp_reg : Format.formatter -> reg -> unit
val pp_op : Format.formatter -> op -> unit
val pp_insn : Format.formatter -> instruction -> unit


