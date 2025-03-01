(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf_ast
open Utils
open Common.Counter.R

let rec gen_var (base : string) (global : StringSet.t) : string t =
  (* let* fresh_id = fresh_name base in *)
  let* fresh_var = fresh in
  let fresh_name = base ^ string_of_int fresh_var in
  if StringSet.contains global fresh_name then gen_var base global else return fresh_name
;;

open AstLib.Ast

let pattern_to_identifier = function
  | PId s -> Id s
  | PConstraint (PId s, ty) -> IdConstraint(s, ty)
  | _ -> failwith "not expected"

let pattern_or_op_to_identifier = function
  | POpPat p -> pattern_to_identifier p
  | POpOp s -> Id s
  | _ -> failwith "not expected"

let rec anf_expr (env : StringSet.t) (e : expr) (k : immexpr -> aexpr t) : aexpr t =
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
        let let_expr = ALetIn (Id v, capp, aexpr) in
        return let_expr))
  | EIf (e1, e2, e3) ->
    anf_expr env e1 (fun cimm ->
      let* v = gen_var "if_" env in
      let* then_aexpr = anf_expr env e2 (fun timm -> return (ACExpr (CImmExpr timm))) in
      let* else_aexpr = anf_expr env e3 (fun eimm -> return (ACExpr (CImmExpr eimm))) in
      let* aexpr = k @@ ImmIdentifier (ident_of_definable @@ ident_letters v) in
      let let_expr = ALetIn (Id v, CIf (cimm, then_aexpr, else_aexpr), aexpr) in
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
      let id =  pattern_or_op_to_identifier pat in
      return (ALetIn (id, CImmExpr imm1, aexpr)))

  | _ -> failwith "not implemented / needed"
;;

let rec unroll_efun (expr : expr) : pattern list * expr =
  match expr with
  | EFun (pat, body) ->
    let pats, body' = unroll_efun body in
    pat :: pats, body'
  | _ -> [], expr
;;

let anf_decl (env : StringSet.t) (d : decl) : anf_decl t =
  let helper (pat_or_op, expr)= 
    let efun_pats, body_expr = unroll_efun expr in
    let efun_ids = List.map pattern_to_identifier efun_pats in
  let env =
    StringSet.union
      env
      (StringSet.from_list @@ List.concat_map bound_vars_pattern efun_pats)
  in
  let* aexpr =
    anf_expr env body_expr (fun immexpr -> return (ACExpr (CImmExpr immexpr)))
  in
  return (pattern_or_op_to_identifier pat_or_op, efun_ids, aexpr)
in
  match d with
  | DLet (rf, lb) ->
       let* lb = helper lb in
       return (ADSingleLet (rf, lb))

  | DLetMut (rf, lb, lb2, lbs) ->
    let* lbs = List.map helper (lb :: lb2 :: lbs) |> sequence in
    return (ADMutualRecDecl (rf, lbs))

;;

let anf_program (decls : decl list) =
  let rec get_initial_env ds acc =
    match ds with
    | [] -> acc
    | DLet (_, (POpPat (PId s), _)) :: rest -> get_initial_env rest (StringSet.add acc s)
    | _ :: rest -> get_initial_env rest acc
  in
  let env = get_initial_env decls StringSet.empty in
  (* let decls = Base.List.fold_right ~f:(fun decls acc -> 
    let* acc = acc in 
    let decl = anf_decl env decls 0 in
    return (decl :: acc)
    ) ~init:(return []) decls in *)

  let decls = Base.List.map decls ~f:(fun decls -> 
   snd (anf_decl env decls 0)
  )  in

    
  decls
;;

let run prog =
  let prog = anf_program prog in
  let open Result in
  let rec sequence acc = function
    | [] -> Ok (List.rev acc)
    | Ok x :: xs -> sequence (x :: acc) xs
    | Error e :: _ -> Error e
  in
  sequence [] prog
;;

