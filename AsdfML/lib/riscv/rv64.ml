type reg =
  | SP
  | Reg of string
  | Offset of reg * int
  | Temp of int
(* | User of int *)

type instr =
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Addi of reg * reg * int
  (* *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  (* *)
  | Ld of reg * reg
  | Sd of reg * reg
  | Li of reg * int
  (* *)
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  (* *)
  | Label of string
  | Call of string
  | ECall
  | Ret
  (* *)
  | Comment of string

let zero = Reg "zero"
let ra = Reg "ra"
let sp = SP
let t0, t1, t2, t3, t4, t5, t6 = Temp 0, Temp 1, Temp 2, Temp 3, Temp 4, Temp 5, Temp 6

let a0, a1, a2, a3, a4, a5, a6, a7 =
  Reg "a0", Reg "a1", Reg "a2", Reg "a3", Reg "a4", Reg "a5", Reg "a6", Reg "a7"
;;

let add k rd rs1 rs2 = k (Add (rd, rs1, rs2))
let sub k rd rs1 rs2 = k (Sub (rd, rs1, rs2))
let mul k rd rs1 rs2 = k (Mul (rd, rs1, rs2))
let div k rd rs1 rs2 = k (Div (rd, rs1, rs2))
let addi k rd rs n = k (Addi (rd, rs, n))

(*  *)
let and_ k rd rs1 rs2 = k (And (rd, rs1, rs2))
let or_ k rd rs1 rs2 = k (Or (rd, rs1, rs2))

(*  *)
let ld k rd rs1 = k (Ld (rd, rs1))
let sd k rd rs1 = k (Sd (rd, rs1))
let li k rd n = k (Li (rd, n))

(*  *)
let beq k rd rs1 where = k (Beq (rd, rs1, where))
let blt k rd rs1 where = k (Blt (rd, rs1, where))

(*  *)
let label k s = k (Label s)
let call k s = k (Call s)
let call k = k ECall
let ret k = k Ret

(*  *)
let comment k s = k (Comment s)

open Base

let code : (instr * string) Queue.t = Queue.create ()
let emit ?(comm = "") instr = instr (fun i -> Queue.enqueue code (i, comm))
