(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ll_ast
open Anf_ast

let free = ref 0;;
let next_free _ = free := !free + 1; !free;;
let clear_free _ = free := 0; !free;;
let get_name name = 
  let num = next_free () in 
  String.concat "" [name; "#"; (string_of_int num)]
;;

let rec anf_expr e expr_with_hole = 
  let get_const = function
    | Ast.CInt i -> AInt i
    | Ast.CBool b -> ABool b
    | Ast.CUnit -> AUnit
  in
  let anf_bin_op f e1 e2 = anf_expr e1 (fun limm -> anf_expr e2 (fun rimm -> 
    let name = get_name "anf_op" in
    ALet (name, f limm rimm, expr_with_hole (AId name))
  ))
  in
  match e with
  | LId id -> expr_with_hole (AId id)
  | LConst c -> expr_with_hole (get_const c)
  | LNot e -> anf_expr e (fun imm -> 
    let name = get_name "anf_not" in
    ALet (name, ANot imm, expr_with_hole (AId name))
  )
  | LOr (e1, e2) -> anf_bin_op (fun e1 e2 -> AOr(e1, e2)) e1 e2
  | LAnd (e1, e2) -> anf_bin_op (fun e1 e2 -> AAnd(e1, e2)) e1 e2
  | LEq (e1, e2) -> anf_bin_op (fun e1 e2 -> AEq(e1, e2)) e1 e2
  | LGt (e1, e2) -> anf_bin_op (fun e1 e2 -> AGt(e1, e2)) e1 e2
  | LLt (e1, e2) -> anf_bin_op (fun e1 e2 -> ALt(e1, e2)) e1 e2
  | LGte (e1, e2) -> anf_bin_op (fun e1 e2 -> AGte(e1, e2)) e1 e2
  | LLte (e1, e2) -> anf_bin_op (fun e1 e2 -> ALte(e1, e2)) e1 e2
  | LAdd (e1, e2) -> anf_bin_op (fun e1 e2 -> AAdd(e1, e2)) e1 e2
  | LSub (e1, e2) -> anf_bin_op (fun e1 e2 -> ASub(e1, e2)) e1 e2
  | LMul (e1, e2) -> anf_bin_op (fun e1 e2 -> AMul(e1, e2)) e1 e2
  | LDiv (e1, e2) -> anf_bin_op (fun e1 e2 -> ADiv(e1, e2)) e1 e2
  | LIf (e1, e2, e3) -> anf_expr e1 (fun cimm -> 
    let name = get_name "anf_if" in
    let t_anf = anf_expr e2 (fun imm -> ACExpr(CImmExpr imm)) in
    let f_anf = anf_expr e3 (fun imm -> ACExpr(CImmExpr imm)) in
    ALet(name, AIf(cimm, t_anf, f_anf), expr_with_hole (AId name))
  )
  | LApp (id, arg::args) -> let args = List.rev args in anf_expr arg (fun imm_arg -> (List.fold_left 
    (fun f a -> (fun lst imm0 -> (anf_expr a (fun imm1 -> (f (imm0::lst) imm1)))))
    (fun lst imm -> 
      let name = get_name "anf_app" in
      ALet(name, AApp(AId id, List.rev (imm::lst)), expr_with_hole (AId name)))
    args) [] imm_arg)
  | LApp (id, []) -> (
    let name = get_name "anf_app" in
    ALet(name, AApp(AId id, []), expr_with_hole (AId name))
  )
  (* | LApp (id, []) -> expr_with_hole (AId id) *)
;;

let anf_fun = function
  | LFun (id, args, e) -> AFun(id, args, anf_expr e (fun imm -> ACExpr(CImmExpr imm)))
;;

let anf = List.map anf_fun
