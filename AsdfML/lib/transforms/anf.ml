open Base
open Anf_ast
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax

let new_var () =
  let* fresh = fresh in
  return ("a" ^ string_of_int fresh)
;;

let rec anf_expr env (expr : cf_expr) (cont : imm_expr -> aexpr State.IntStateM.t)
  : aexpr State.IntStateM.t
  =
  match expr with
  | CFConst c ->
    (match c with
     | Ast.CInt n -> cont (ImmInt n)
     | Ast.CBool b -> cont (ImmBool b)
     | _ -> failwith "Not implemented")
  | CFVar x -> cont (ImmId x)
  | CFApp (func, arg) ->
    let* name = new_var () in
    let* body = cont (ImmId name) in
    anf_expr env func (fun imm_func ->
      anf_expr env arg (fun imm_arg ->
        return (ALet (name, CApp (imm_func, imm_arg), body))))
  | CFIfElse (i, t, e) ->
    let* name = new_var () in
    let* body = cont (ImmId name) in
    anf_expr env i (fun i ->
      let* t = anf_expr env t (fun x -> return @@ ACExpr (CImmExpr x)) in
      let* e = anf_expr env e (fun x -> return @@ ACExpr (CImmExpr x)) in
      return (ALet (name, CIfElse (i, t, e), body)))
  | CFLetIn (id, expr, body) ->
    let* name = new_var () in
    let new_env = Map.set env ~key:id ~data:name in
    anf_expr env expr (fun imm_expr ->
      let* body = anf_expr new_env body cont in
      return (ALet (name, CImmExpr imm_expr, body)))
  | CFTuple _ | CFList _ -> failwith "Not implemented"
;;

let anf_def env (def : cf_definition) : fn State.IntStateM.t =
  match def with
  | CFLet (id, args, expr) ->
    let* aexpr = anf_expr env expr (fun x -> return @@ ACExpr (CImmExpr x)) in
    return (Fn (id, args, aexpr))
;;

type env = string Map.M(String).t

let anf (ast : Cf_ast.program) : Anf_ast.program =
  let h ast =
    List.fold
      ast
      ~init:(return ([] : fn list))
      ~f:(fun acc x ->
        let* fns = acc in
        let* x = anf_def Map.Poly.empty x in
        return (x :: fns))
  in
  run (h ast)
;;
