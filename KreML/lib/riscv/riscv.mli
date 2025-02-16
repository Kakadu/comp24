type reg =
  | Zero
  | Ra
  | Sp
  | Temp of int (** t{i} *)
  | Saved of int (** s{i} *)
  | Arg of int (** a{i} *)

type op =
  (* R-type *)
  | ADD
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
  | Rtype of reg * reg * reg * op (** dst, rs1, rs2, op *)
  | Itype of reg * int * reg * op (** dst, imm, rs1, op *)
  | SType of reg * int * reg * op (** base, offset, value, op *)
  | BType of reg * reg * int * op (** rs1, rs2, target, op *)
  | UType of reg * int * op (** dst, imm, op *)
  | JType of int * reg * op (** signed offset, link reg, op *)
