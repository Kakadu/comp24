(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

let find_free_vars global_env =
  let rec find_free_vars_pattern pat =
    match pat with
    | PWild | PEmpty | PConst _ -> Set.empty (module String)
    | PVar x -> Set.singleton (module String) x
    | PCons (p1, p2, ps) ->
      Set.union_list
        (module String)
        [ find_free_vars_pattern p1
        ; find_free_vars_pattern p2
        ; List.fold
            ps
            ~init:(Set.empty (module String))
            ~f:(fun acc p -> Set.union acc (find_free_vars_pattern p))
        ]
      (* According to OCaml rules, all patterns in POr must have same variables set *)
    | POr (p1, _, _) -> find_free_vars_pattern p1
  in
  let rec find_free_vars_case env (pat, e) =
    let env = Set.union env (find_free_vars_pattern pat) in
    helper env e
  and helper env = function
    | EConst _ -> Set.empty (module String)
    | EVar x ->
      if Set.mem env x then Set.empty (module String) else Set.singleton (module String) x
    | ETuple (e1, e2, es) ->
      Set.union_list
        (module String)
        [ helper env e1
        ; helper env e2
        ; List.fold
            es
            ~init:(Set.empty (module String))
            ~f:(fun acc e -> Set.union acc (helper env e))
        ]
    | EList es -> Set.union_list (module String) (List.map es ~f:(fun e -> helper env e))
    | EBranch (cond, t, f) ->
      Set.union_list (module String) [ helper env cond; helper env t; helper env f ]
    | EMatch (e, cases) ->
      let vs1 = helper env e in
      let vs2 =
        List.fold
          cases
          ~init:(Set.empty (module String))
          ~f:(fun acc case -> Set.union acc (find_free_vars_case env case))
      in
      Set.union vs1 vs2
    | ELet (is_rec, id, e1, e2) ->
      let env =
        match is_rec with
        | Rec -> Set.add env id
        | NonRec -> env
      in
      let vs1 = helper env e1 in
      let env = Set.add env id in
      let vs2 =
        match e2 with
        | Some e2 -> helper env e2
        | None -> Set.empty (module String)
      in
      Set.union vs1 vs2
    | EFun ((id, _), e) ->
      let env = Set.add env id in
      helper env e
    | EApp (e1, e2) -> Set.union (helper env e1) (helper env e2)
  in
  helper global_env
;;

let closure_app e args =
  List.fold_left args ~init:e ~f:(fun acc arg -> EApp (acc, EVar arg))
;;

let uncurry e =
  let rec helper args = function
    | EFun (x, e) -> helper (x :: args) e
    | e -> args, e
  in
  let args, e = helper [] e in
  List.rev args, e
;;

let curry args e = Base.List.fold_right args ~init:e ~f:(fun arg acc -> EFun (arg, acc))

let closure_fun f args =
  Base.List.fold_right args ~init:f ~f:(fun arg acc -> EFun ((arg, None), acc))
;;

let close_expr e env =
  let rec helper e free_vars env =
    match e with
    | EConst c -> EConst c
    | EVar x ->
      (match Map.find free_vars x with
       | Some vs -> closure_app (EVar x) vs
       | None -> EVar x)
    | ETuple (e1, e2, es) ->
      ETuple
        ( helper e1 free_vars env
        , helper e2 free_vars env
        , List.map es ~f:(fun e -> helper e free_vars env) )
    | EList es -> EList (List.map es ~f:(fun e -> helper e free_vars env))
    | EBranch (cond, t, f) ->
      EBranch (helper cond free_vars env, helper t free_vars env, helper f free_vars env)
    | EMatch (e, cases) ->
      let e = helper e free_vars env in
      let cases = List.map cases ~f:(fun (pat, e) -> pat, helper e free_vars env) in
      EMatch (e, cases)
    | ELet (is_rec, id, (EFun (_, _) as e1), e2) ->
      let free = Set.to_list (find_free_vars env e) in
      let free_vars = Map.update free_vars id ~f:(fun _ -> free) in
      let args, e = uncurry e1 in
      let e = helper e free_vars env in
      let f = curry args e in
      let f = closure_fun f free in
      let e2 =
        match e2 with
        | Some e2 -> Some (helper e2 free_vars env)
        | None -> None
      in
      ELet (is_rec, id, f, e2)
    | ELet (is_rec, id, e1, e2) ->
      let e1 = helper e1 free_vars env in
      let e2 =
        match e2 with
        | Some e2 -> Some (helper e2 free_vars env)
        | None -> None
      in
      ELet (is_rec, id, e1, e2)
    | EFun (_, _) as f ->
      let free = Set.to_list (find_free_vars env f) in
      let args, e = uncurry f in
      let e = helper e free_vars env in
      let f = curry args e in
      let f = closure_fun f free in
      closure_app f free
    | EApp (e1, e2) -> EApp (helper e1 free_vars env, helper e2 free_vars env)
  in
  helper e (Map.empty (module String)) env
;;
