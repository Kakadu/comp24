(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let word = 8
let n_reg_args = 8

type reg =
  | SP
  | Reg of string
  | Temp of int
[@@deriving eq, show { with_path = false }]

type offset = reg * int [@@deriving eq, show { with_path = false }]

type rvalue =
  | RInt of int
  | RFn of string
  | RReg of reg
  | ROffset of offset

type loc =
  | LReg of reg
  | LMem of offset
[@@deriving show { with_path = false }]

let rec loc_to_rvalue = function
  | LReg r -> RReg r
  | LMem o -> ROffset o
;;

let rec pp_reg ppf =
  let open Format in
  function
  | SP -> fprintf ppf "sp"
  | Reg src -> fprintf ppf "%s" src
  | Temp n -> fprintf ppf "t%d" n

and pp_offset ppf offset = Format.fprintf ppf "%d(%a)" (snd offset) pp_reg (fst offset)

let pp_rvalue ppf =
  let open Format in
  function
  | RInt n -> fprintf ppf "%d" n
  | RFn n -> fprintf ppf "%s" n
  | RReg r -> pp_reg ppf r
  | ROffset o -> fprintf ppf "%a" pp_offset o
;;

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
  (* *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  (* *)
  | Ld of reg * offset
  | Sd of reg * offset
  | Li of reg * int
  | La of reg * string
  | Mv of reg * reg
  (* *)
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | J of string
  (* *)
  | Label of string
  | Call of string
  | ECall
  | Ret
  (* *)
  | Comment of string
  | Str of string

let pp_instr fmt =
  let open Format in
  function
  | Add (dst, src1, src2) ->
    fprintf fmt "    add %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Sub (dst, src1, src2) ->
    fprintf fmt "    sub %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Mul (dst, src1, src2) ->
    fprintf fmt "    mul %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Div (dst, src1, src2) ->
    fprintf fmt "    div %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Addi (dst, src, n) -> fprintf fmt "    addi %a,%a,%d" pp_reg dst pp_reg src n
  | Xori (dst, src, n) -> fprintf fmt "    xori %a,%a,%d" pp_reg dst pp_reg src n
  | Xor (dst, src1, src2) ->
    fprintf fmt "    xor %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Slt (dst, src1, src2) ->
    fprintf fmt "    slt %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Seqz (dst, src) -> fprintf fmt "    seqz %a,%a" pp_reg dst pp_reg src
  | Snez (dst, src) -> fprintf fmt "    snez %a,%a" pp_reg dst pp_reg src
  (* *)
  | And (dst, src1, src2) ->
    fprintf fmt "    and %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  | Or (dst, src1, src2) ->
    fprintf fmt "    or %a,%a,%a" pp_reg dst pp_reg src1 pp_reg src2
  (* *)
  | Ld (dst, src) -> fprintf fmt "    ld %a,%a" pp_reg dst pp_offset src
  | Sd (dst, src) -> fprintf fmt "    sd %a,%a" pp_reg dst pp_offset src
  | Li (dst, n) -> fprintf fmt "    li %a,%d" pp_reg dst n
  | La (dst, s) -> fprintf fmt "    la %a,%s" pp_reg dst s (* TODO: (l)la *)
  | Mv (dst, src) -> fprintf fmt "    mv %a,%a" pp_reg dst pp_reg src
  (* *)
  | Beq (dst, src, where) -> fprintf fmt "    beq %a,%a,%s" pp_reg dst pp_reg src where
  | Blt (dst, src, where) -> fprintf fmt "    blt %a,%a,%s" pp_reg dst pp_reg src where
  | J where -> fprintf fmt "    j %s" where
  (* *)
  | Label src -> fprintf fmt "%s:" src
  | Call src -> fprintf fmt "    call %s" src
  | ECall -> fprintf fmt "    ecall"
  | Ret -> fprintf fmt "    ret"
  (* *)
  | Comment src -> fprintf fmt "    # %s" src
  | Str src -> fprintf fmt "%s" src
;;

let zero = Reg "zero"
let ra = Reg "ra"
let sp = SP
let s0 = Reg "s0"
let fp = s0
let t0, t1, t2, t3, t4, t5, t6 = Temp 0, Temp 1, Temp 2, Temp 3, Temp 4, Temp 5, Temp 6

let a0, a1, a2, a3, a4, a5, a6, a7 =
  Reg "a0", Reg "a1", Reg "a2", Reg "a3", Reg "a4", Reg "a5", Reg "a6", Reg "a7"
;;

let arg_regs = [ a0; a1; a2; a3; a4; a5; a6; a7 ]
let add k dst src1 src2 = k (Add (dst, src1, src2))
let sub k dst src1 src2 = k (Sub (dst, src1, src2))
let mul k dst src1 src2 = k (Mul (dst, src1, src2))
let div k dst src1 src2 = k (Div (dst, src1, src2))
let addi k dst src n = k (Addi (dst, src, n))
let xori k dst src n = k (Xori (dst, src, n))
let xor k dst src1 src2 = k (Xor (dst, src1, src2))
let slt k dst src1 src2 = k (Slt (dst, src1, src2))
let seqz k dst src = k (Seqz (dst, src))
let snez k dst src = k (Snez (dst, src))

(*  *)
let and_ k dst src1 src2 = k (And (dst, src1, src2))
let or_ k dst src1 src2 = k (Or (dst, src1, src2))

(*  *)
let ld k dst src = k (Ld (dst, src))
let sd k src dst = k (Sd (src, dst))
let li k dst n = k (Li (dst, n))
let la k dst n = k (La (dst, n))
let mv k dst src = k (Mv (dst, src))

(*  *)
let beq k dst src1 where = k (Beq (dst, src1, where))
let blt k dst src1 where = k (Blt (dst, src1, where))
let j k where = k (J where)

(*  *)
let label k src = k (Label src)
let call k src = k (Call src)
let ecall k = k ECall
let ret k = k Ret

(*  *)
let comment k src = k (Comment src)
let str k src = k (Str src)
