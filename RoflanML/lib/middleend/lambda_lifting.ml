(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ll_ast
open Common
open Common.Counter_Monad
open Common.Middleend_Common

let gen_name = gen_name "LL"

let lift decl =
  let rec lift_expr e lifted renames =
    match e with
    | EConst c -> return (LLConst c, lifted)
    | EVar x ->
      (match Map.find renames x with
       | Some id -> return (LLVar id, lifted)
       | None -> return (LLVar x, lifted))
    | ETuple (e1, e2, es) ->
      let* e1, lifted = lift_expr e1 lifted renames in
      let* e2, lifted = lift_expr e2 lifted renames in
      let* es, lifted =
        List.fold_left
          es
          ~init:(return ([], lifted))
          ~f:(fun acc e ->
            let* es, lifted = acc in
            let* e, lifted = lift_expr e lifted renames in
            return (e :: es, lifted))
      in
      return (LLTuple (e1, e2, List.rev es), lifted)
    | EList es ->
      let* es, lifted =
        List.fold_left
          es
          ~init:(return ([], lifted))
          ~f:(fun acc e ->
            let* es, lifted = acc in
            let* e, lifted = lift_expr e lifted renames in
            return (e :: es, lifted))
      in
      return (LLList (List.rev es), lifted)
    | EBranch (cond, t, f) ->
      let* cond, lifted = lift_expr cond lifted renames in
      let* t, lifted = lift_expr t lifted renames in
      let* f, lifted = lift_expr f lifted renames in
      return (LLBranch (cond, t, f), lifted)
    | EMatch (e, cases) ->
      let* e, lifted = lift_expr e lifted renames in
      let* cases, lifted =
        List.fold_left
          cases
          ~init:(return ([], lifted))
          ~f:(fun acc (p, e) ->
            let* cases, lifted = acc in
            let* e, lifted = lift_expr e lifted renames in
            return ((p, e) :: cases, lifted))
      in
      return (LLMatch (e, List.rev cases), lifted)
    | ELetIn (is_rec, id, (EFun _ as e1), e2) ->
      let args, e = uncurry e1 in
      let* new_name = gen_name in
      let renames =
        match is_rec with
        | Rec -> Map.update renames id ~f:(fun _ -> new_name)
        | NonRec -> renames
      in
      let* e, lifted = lift_expr e lifted renames in
      let lifted = LLDLet (is_rec, new_name, args, e) :: lifted in
      let renames = Map.update renames id ~f:(fun _ -> new_name) in
      let* e2, lifted = lift_expr e2 lifted renames in
      return (e2, lifted)
    | ELetIn (_, id, e1, e2) ->
      let* e1, lifted = lift_expr e1 lifted renames in
      let* e2, lifted = lift_expr e2 lifted renames in
      return (LLLetIn (id, e1, e2), lifted)
    | EFun _ as f ->
      let args, e = uncurry f in
      let* new_name = gen_name in
      let* e, lifted = lift_expr e lifted renames in
      let lifted = LLDLet (NonRec, new_name, args, e) :: lifted in
      return (LLVar new_name, lifted)
    | EApp (e1, e2) ->
      let* e1, lifted = lift_expr e1 lifted renames in
      let* e2, lifted = lift_expr e2 lifted renames in
      return (LLApp (e1, e2), lifted)
  in
  let lift_decl decl lifted renames =
    match decl with
    | DLet (is_rec, id, (EFun _ as f)) ->
      let args, e = uncurry f in
      let* e, lifted = lift_expr e lifted renames in
      return (LLDLet (is_rec, id, args, e), lifted)
    | DLet (is_rec, id, e) ->
      let* e, lifted = lift_expr e lifted renames in
      return (LLDLet (is_rec, id, [], e), lifted)
    | DMutualLet _ -> failwith "Not Implemented"
  in
  lift_decl decl [] (Map.empty (module String))
;;

let lift_program prog =
  let helper prog =
    List.fold_left prog ~init:(return []) ~f:(fun acc decl ->
      let* acc = acc in
      let* decl, lifted = lift decl in
      return (List.concat [ acc; List.rev (decl :: lifted) ]))
  in
  run (helper prog)
;;
