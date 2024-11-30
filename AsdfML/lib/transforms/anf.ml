open Base
open Anf_ast
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax

let rec anf_expr (expr : cf_expr) (cont : imm_expr -> aexpr State.IntStateM.t) =
  match expr with
  | CFConst c ->
    (match c with
     | Ast.CInt n -> cont (ImmInt n)
     | Ast.CBool b -> cont (ImmBool b)
     | _ -> failwith "Not implemented")
  | CFVar x -> cont (ImmId x)
  | CFApp (func, arg) ->
    let* fresh = fresh in
    let name = string_of_int fresh in
    let* body = cont (ImmId name) in
    anf_expr func (fun imm_func ->
      anf_expr arg (fun imm_arg ->
        let res = ALet (name, CApplication (imm_func, imm_arg), body) in
        return res))
  | CFIfElse (i, t, e) ->
    let* fresh = fresh in
    let name = string_of_int fresh in
    let* body = cont (ImmId name) in
    anf_expr i (fun i ->
      let* t = anf_expr t (fun x -> return @@ ACExpr (CImmExpr x)) in
      let* e = anf_expr e (fun x -> return @@ ACExpr (CImmExpr x)) in
      let res = ALet (name, CIfElse (i, t, e), body) in
      return res)
  | CFLetIn (def, exp) -> failwith "Not implemented"
  | _ -> failwith "Not implemented"
;;
