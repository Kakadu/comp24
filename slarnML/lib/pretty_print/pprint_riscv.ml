open Riscv_ast
open String

let pp_reg = function
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | S n -> "s"^(string_of_int n)
  | A n -> "a"^(string_of_int n)
  | T n -> "t"^(string_of_int n)
;;

let pp_cond = function
  | Beq -> "beq "
  | Bge -> "bge "
  | Bgt -> "bgt "
  | Blt -> "blt "
  | Ble -> "ble "
  | Bne -> "bne "
;;

let pp_addr = function
  | Id id -> id
  | Reg r -> pp_reg r
;;

let pp_math rd r1 r2 op = 
  let pp_rr rd r1 r2 = (concat "," [(pp_reg rd); (pp_reg r1); (pp_reg r2)]) in
  match op with
  | I op -> (match op with
  | Add -> "add "^(pp_rr rd r1 r2)
  | And -> "and "^(pp_rr rd r1 r2)
  | Or  -> "or " ^(pp_rr rd r1 r2)
  | Xor -> "xor "^(pp_rr rd r1 r2)
  | Sll -> "sll "^(pp_rr rd r1 r2)
  | Srl -> "srl "^(pp_rr rd r1 r2)
  )
  | Mul -> "mul "^(pp_rr rd r1 r2)
  | Sub -> "sub "^(pp_rr rd r1 r2)
  | Div -> "div "^(pp_rr rd r1 r2)
;;
let pp_imm = function
  | ImmInt i -> string_of_int i
  | ConstAddr (c, f) -> concat "" ["%";c;"";"(";f;")"]
;;

let pp_mathi rd r1 n op = 
  let pp_rn rd r1 n = (concat "," [(pp_reg rd); (pp_reg r1); (pp_imm n)]) in
  match op with
  | Add -> "addi "^(pp_rn rd r1 n)
  | And -> "andi "^(pp_rn rd r1 n)
  | Or  -> "ori " ^(pp_rn rd r1 n)
  | Xor -> "xori "^(pp_rn rd r1 n)
  | Sll -> "slli "^(pp_rn rd r1 n)
  | Srl -> "srli "^(pp_rn rd r1 n)
;;

let pp_instruction tab instr = 
  let pp_stack_arg rd n r1 = (concat "" [(pp_reg rd);",";(pp_imm n);"(";(pp_reg r1);")"]) in
  match instr with
  | Tag t -> t^":"
  | Math (op, rd, r1, r2) -> tab^pp_math rd r1 r2 op
  | Mathi (op, rd, r1, r2) -> tab^pp_mathi rd r1 r2 op
  | Beqz (r1, a) -> "beqz " ^ (concat "," [(pp_reg r1); (pp_addr a)])
  | Bnez (r1, a) -> "bnez " ^ (concat "," [(pp_reg r1); (pp_addr a)])
  | Bnch (cond, r1, r2, a) -> (pp_cond cond) ^ (concat "," [(pp_reg r1); (pp_reg r2); (pp_addr a)])
  | Call a -> "call "^(pp_addr a)
  | Jmp a -> tab^"j "^(pp_addr a)
  | La (rd, a) -> tab^"la "^(concat "," [(pp_reg rd); (pp_addr a)])
  | Li (rd, n) -> tab^"li "^(concat "," [(pp_reg rd); (pp_imm n)])
  | Mv (rd, rs) -> tab^"mv "^(concat "," [(pp_reg rd); (pp_reg rs)])
  | Ret -> "ret"
  | Ld (rd, n, r1) -> tab^"ld "^(pp_stack_arg rd n r1)
  | Sd (rd, n, r1) -> tab^"sd "^(pp_stack_arg rd n r1)
  | Lui (rd, imm) -> tab^"lui "^(concat "," [(pp_reg rd); (pp_imm imm)])
  | Ecall -> "ecall"
  | Global g -> ".global "^g
  | Attribute a -> ".attribute "^a
(* Unused
  | Lb (rd, n, r1) -> "la "^(pp_stack_arg rd n r1)
  | Lh (rd, n, r1) -> "lh "^(pp_stack_arg rd n r1)
  | Lw (rd, n, r1) -> "lw "^(pp_stack_arg rd n r1)
  | Sb (rd, n, r1) -> "sa "^(pp_stack_arg rd n r1)
  | Sh (rd, n, r1) -> "sh "^(pp_stack_arg rd n r1)
  | Sw (rd, n, r1) -> "sw "^(pp_stack_arg rd n r1)
*)
;;

