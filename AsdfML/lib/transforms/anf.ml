open Base
open Anf_ast
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax
open Utils
open Std

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
     | Ast.CUnit -> cont ImmUnit
     | Ast.CNil -> cont ImmNil)
  | CFVar x ->
    cont
      (ImmId
         (match Map.find env x with
          | Some v -> v
          | None -> Format.sprintf "(Variable %s not found)" x))
  (* | CFVar x -> cont (ImmId x) *)
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

let anf_def env (def : cf_definition) =
  match def with
  | CFLet (id, args, expr) ->
    let env = List.fold args ~init:env ~f:(fun acc x -> Map.set acc ~key:x ~data:x) in
    let env = Map.set env ~key:id ~data:id in
    let* aexpr = anf_expr env expr (fun x -> return @@ ACExpr (CImmExpr x)) in
    return ((Fn (id, args, aexpr)), env)
;;

let default_env =
  let env = Map.Poly.empty in
  let env =
    stdlib |> List.fold ~init:env ~f:(fun acc x -> Map.set acc ~key:x.name ~data:x.name)
  in
  env
;;

let anf (ast : Cf_ast.program) : Anf_ast.program =
  let helper ast =
    List.fold
      ast
      ~init:(return (([] : fn list), default_env))
      ~f:(fun acc x ->
        let* fns, env = acc in
        let* x, env = anf_def env x in
        return (x :: fns, env))
    >>| fun (defs, _) -> List.rev defs
  in
  run (helper ast)
;;
