(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | Zero
  | Ra
  | Sp
  | Temp of int
  | Saved of int
  | Arg of int

let temp idx = Temp idx
let saved idx = Saved idx
let arg idx = Arg idx
let fp = Saved 0

let is_saved = function
  | Saved _ -> true
  | _ -> false
;;

type rvalue =
  | Rv_imm of int
  | Rv_reg of reg
  | Rv_mem of reg * int (** base, offset *)
  | Rv_function of Flambda.fun_decl
  | Rv_nop

let a0value = Rv_reg (arg 0)

type location =
  | Loc_reg of reg
  | Loc_mem of reg * int

let idc_loc = Loc_reg Zero

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
  | LD
  | LH
  | LHU
  | LB
  (* S-type *)
  | SW
  | SD
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
  | Stype of reg * int * reg * op (** value, offset, dst, op *)
  | Btype of reg * reg * string * op (** rs1, rs2, target, op *)
  | Utype of reg * int * op (** dst, imm, op *)
  | Pseudo of string (* can not unify signature *)
  | Label of string
  | Global of string

let extend_stack_insn size = Itype (Sp, Sp, -size, ADDI)
let shrink_stack_insn size = Itype (Sp, Sp, size, ADDI)
let sd ~v offset ~dst = Stype (v, offset, dst, SD)
let ld ~rd offset ~src = Itype (rd, src, offset, LD)

module RegistersStorage : Registers_storage_intf.S with type 'a t = 'a list = struct
  type 'a t = 'a list

  let remove reg = List.filter (( <> ) reg)
  let add = List.cons
  let find = List.find
  let size = List.length

  let pop regs =
    match regs with
    | x :: xs -> x, xs
    | _ -> Utils.internalfail "Pop from empty registers storage"
  ;;

  let with_ranges = List.combine
  let fold = List.fold_left
  let empty = []
  let to_list = Fun.id
end

(* fp is not available *)
let available_regs = List.init 7 temp @ List.init 11 (fun i -> saved (i + 1))
let temporary_regs = List.init 7 temp

let pp_reg fmt =
  let open Format in
  function
  | Zero -> fprintf fmt "x0"
  | Ra -> fprintf fmt "ra"
  | Sp -> fprintf fmt "sp"
  | Temp i -> fprintf fmt "t%i" i
  | Saved i -> fprintf fmt "s%i" i
  | Arg i -> fprintf fmt "a%i" i
;;

let pp_op fmt =
  let open Format in
  let p = fprintf fmt in
  function
  | ADD -> p "add"
  | MUL -> p "mul"
  | DIV -> p "div"
  | SLT -> p "slt"
  | SLTU -> p "sltu"
  | AND -> p "and"
  | OR -> p "or"
  | XOR -> p "xor"
  | SLL -> p "sll"
  | SRL -> p "srl"
  | SUB -> p "sub"
  | SRA -> p "sra"
  | ADDI -> p "addi"
  | SLTI -> p "slti"
  | SLTIU -> p "sltiu"
  | ANDI -> p "andi"
  | ORI -> p "ori"
  | XORI -> p "xori"
  | LW -> p "lw"
  | LD -> p "ld"
  | LH -> p "lh"
  | LHU -> p "lhu"
  | LB -> p "lb"
  | SW -> p "sw"
  | SD -> p "sd"
  | SH -> p "sh"
  | SB -> p "sb"
  | BEQ -> p "beq"
  | BNE -> p "bne"
  | BLT -> p "blt"
  | BLTU -> p "bltu"
  | BGE -> p "bge"
  | BGEU -> p "bgeu"
  | LUI -> p "lui"
  | AUIPC -> p "auipc"
  | JAL -> p "jal"
  | JALR -> p "jalr"
;;

let pp_insn fmt =
  let open Format in
  function
  | Rtype (rd, rs1, rs2, op) ->
    fprintf fmt "@[\t%a %a, %a, %a@]@." pp_op op pp_reg rd pp_reg rs1 pp_reg rs2
  | Itype (rd, rs, imm, LD) ->
    fprintf fmt "@[\t%a %a, %i(%a) @]@." pp_op LD pp_reg rd imm pp_reg rs
  | Itype (rd, rs, imm, op) ->
    fprintf fmt "@[\t%a %a, %a, %i@]@." pp_op op pp_reg rd pp_reg rs imm
  | Stype (value, offset, dst, op) ->
    fprintf fmt "@[\t%a %a, %i(%a) @]@." pp_op op pp_reg value offset pp_reg dst
  | Btype (rs1, rs2, target, op) ->
    fprintf fmt "@[\t%a  %a, %a, %s@]@." pp_op op pp_reg rs1 pp_reg rs2 target
  | Utype (rd, imm, op) -> fprintf fmt "@[\t%a %a, %i@]@." pp_op op pp_reg rd imm
  | Pseudo p -> fprintf fmt "@[\t%s@]@." p
  | Label l -> fprintf fmt "@[%s:@]@." l
  | Global s -> fprintf fmt "@[.global %s@]@." s
;;

module Pseudo = struct
  open Format

  let mv ~rd ~src =
    let insn = asprintf "mv %a, %a" pp_reg rd pp_reg src in
    Pseudo insn
  ;;

  let li rd imm =
    let insn = asprintf "li %a, %i" pp_reg rd imm in
    Pseudo insn
  ;;

  let la rd label =
    let insn = asprintf "la  %a, %s" pp_reg rd label in
    Pseudo insn
  ;;

  let neg r =
    let insn = asprintf "neg %a" pp_reg r in
    Pseudo insn
  ;;

  let seqz ~rd ~src =
    let insn = asprintf "seqz %a, %a" pp_reg rd pp_reg src in
    Pseudo insn
  ;;

  let call label =
    let insn = Format.sprintf "call %s" label in
    Pseudo insn
  ;;

  let jump label =
    let insn = sprintf "j %s" label in
    Pseudo insn
  ;;

  let ret = Pseudo "ret"
end
