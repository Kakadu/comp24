type reg =
  | SP
  | Reg of string
  | Offset of reg * int
  | Temp of int

let rec pp_reg ppf =
  let open Format in
  function
  | SP -> fprintf ppf "sp"
  | Reg s -> fprintf ppf "%s" s
  | Offset (r, n) -> fprintf ppf "%a(%d)" pp_reg r n
  | Temp n -> fprintf ppf "t%d" n
;;

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

let pp_instr fmt =
  let open Format in
  function
  | Add (rd, rs1, rs2) -> fprintf fmt "    add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Sub (rd, rs1, rs2) -> fprintf fmt "    sub %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Mul (rd, rs1, rs2) -> fprintf fmt "    mul %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Div (rd, rs1, rs2) -> fprintf fmt "    div %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Addi (rd, rs, n) -> fprintf fmt "    addi %a, %a, %d" pp_reg rd pp_reg rs n
  (* *)
  | And (rd, rs1, rs2) -> fprintf fmt "    and %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Or (rd, rs1, rs2) -> fprintf fmt "    or %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  (* *)
  | Ld (rd, rs) -> fprintf fmt "    ld %a, %a" pp_reg rd pp_reg rs
  | Sd (rd, rs) -> fprintf fmt "    sd %a, %a" pp_reg rd pp_reg rs
  | Li (rd, n) -> fprintf fmt "    li %a, %d" pp_reg rd n
  (* *)
  | Beq (rd, rs, where) -> fprintf fmt "    beq %a, %a, %s" pp_reg rd pp_reg rs where
  | Blt (rd, rs, where) -> fprintf fmt "    blt %a, %a, %s" pp_reg rd pp_reg rs where
  (* *)
  | Label s -> fprintf fmt "%s:" s
  | Call s -> fprintf fmt "    call %s" s
  | ECall -> fprintf fmt "    ecall"
  | Ret -> fprintf fmt "    ret"
  (* *)
  | Comment s -> fprintf fmt " # %s" s
;;

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

let code : string Queue.t = Queue.create ()

let emit ?(comm = "") instr =
  instr (fun i ->
    Queue.enqueue
      code
      (Format.asprintf
         "%a%s"
         pp_instr
         i
         (if String.(comm <> "") then Format.sprintf " # %s\n" comm else "\n")))
;;

let emit_str s = Queue.enqueue code s

let emit_fn_decl name =
  emit_str
    (Format.sprintf {|
    .globl %s
    .type %s, @function
%s:
|} name name name)
;;
