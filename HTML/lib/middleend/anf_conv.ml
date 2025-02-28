(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AstLib.Ast
open Anf_ast
open Utils

type 'a anf = int -> 'a * int

let return (x : 'a) : 'a anf = fun s -> x, s

let bind (m : 'a anf) (f : 'a -> 'b anf) : 'b anf =
  fun s ->
  let x, s' = m s in
  f x s'
;;

let ( let* ) = bind

let sequence lst =
  let rec aux acc = function
    | [] -> return (List.rev acc)
    | x :: xs -> bind x (fun y -> aux (y :: acc) xs)
  in
  aux [] lst

let fresh_name (prefix : string) : string anf = fun s -> prefix ^ string_of_int s, s + 1

let rec gen_var (base : string) (global : StringSet.t) : string anf =
  let* fresh_id = fresh_name base in
  if StringSet.contains global fresh_id then gen_var base global else return fresh_id
;;

let rec anf_expr (env : StringSet.t) (e : expr) (k : immexpr -> aexpr anf) : aexpr anf =
  let anf_list env exprs cont =
    let rec helper acc = function
      | [] -> cont (List.rev acc)
      | h :: tl -> anf_expr env h (fun imm -> helper (imm :: acc) tl)
    in
    helper [] exprs
  in
  match e with
  | EConstraint (e, t) -> anf_expr env e (fun e -> k (ImmConstraint (e, t)))
  | EConst c -> k (ImmConst c)
  | EId id -> k (ImmIdentifier id)
  | EApp _ ->
    let rec unroll_eapp acc = function
      | EApp (e1, e2) -> unroll_eapp (e2 :: acc) e1
      | e -> e :: acc
    in
    let args = unroll_eapp [] e in
    let f, args = List.hd args, List.tl args in
    anf_expr env f (fun fimm ->
      anf_list env args (fun argsimm ->
        let* v = gen_var "app_" env in
        let rec roll_capp = function
          | [ h ] -> return (CImmExpr h)
          | h :: tl ->
            let* rest = roll_capp tl in
            return (CApp (rest, CImmExpr h))
          | [] -> failwith "todo impossible"
          (* todo: identifier from ast *)
        in
        let* capp = roll_capp @@ List.rev (fimm :: argsimm) in
        let* aexpr = k @@ ImmIdentifier (ident_of_definable @@ ident_letters v) in
        let let_expr = ALetIn (POpPat (PId v), capp, aexpr) in
        return let_expr))
  | EIf (e1, e2, e3) ->
    anf_expr env e1 (fun cimm ->
      let* v = gen_var "if_" env in
      let* then_aexpr = anf_expr env e2 (fun timm -> return (ACExpr (CImmExpr timm))) in
      let* else_aexpr = anf_expr env e3 (fun eimm -> return (ACExpr (CImmExpr eimm))) in
      let* aexpr = k @@ ImmIdentifier (ident_of_definable @@ ident_letters v) in
      let let_expr = ALetIn (POpPat (PId v), CIf (cimm, then_aexpr, else_aexpr), aexpr) in
      return let_expr)
  | ETuple (e1, e2, es) ->
     anf_list env (e1::e2::es) (fun imm ->
     k (ImmTuple imm))
  | EList (e1, e2) ->
    anf_expr env e1 (fun imm1 ->
      anf_expr env e2 (fun imm2 -> k (ImmCons (imm1, imm2))))
  | EClsr (DLet (_, (pat, e1)), e2) ->
    anf_expr env e1 (fun imm1 ->
      let* aexpr = anf_expr env e2 k in
      (* todo remove let a = b in *)
      return (ALetIn (pat, CImmExpr imm1, aexpr)))
  | _ -> failwith "not implemented / needed"
;;

let rec unroll_efun (expr : expr) : pattern list * expr =
  match expr with
  | EFun (pat, body) ->
    let pats, body' = unroll_efun body in
    pat :: pats, body'
  | _ -> [], expr
;;

let anf_decl (env : StringSet.t) (d : decl) : anf_decl anf =
  let helper (pat_or_op, expr)= 
    let efun_pats, body_expr = unroll_efun expr in
  let env =
    StringSet.union
      env
      (StringSet.from_list @@ List.concat_map bound_vars_pattern efun_pats)
  in
  let* aexpr =
    anf_expr env body_expr (fun immexpr -> return (ACExpr (CImmExpr immexpr)))
  in
  return (pat_or_op, efun_pats, aexpr)
in
  match d with
  | DLet (rf, lb) ->
       let* lb = helper lb in
       return (ADSingleLet (rf, lb))

  | DLetMut (rf, lb, lb2, lbs) ->
    let* lbs = List.map helper (lb :: lb2 :: lbs) |> sequence in
    return (ADMutualRecDecl (rf, lbs))

;;

let anf_program (decls : decl list) : anf_decl list =
  let rec get_initial_env ds acc =
    match ds with
    | [] -> acc
    | DLet (_, (POpPat (PId s), _)) :: rest -> get_initial_env rest (StringSet.add acc s)
    | _ :: rest -> get_initial_env rest acc
  in
  let env = get_initial_env decls StringSet.empty in
  let decls = List.map (fun decls -> anf_decl env decls 0 |> fst) decls in
  decls
;;
