(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Anf_ast
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax
open Utils
open Std

let new_var () =
  let* fresh = fresh in
  return ("anf" ^ string_of_int fresh)
;;

let new_or_bind = function
  | Some x -> return x
  | None -> new_var ()
;;

let rec anf_expr
  ?(bind = None)
  env
  (expr : cf_expr)
  (cont : imm_expr -> aexpr State.IntStateM.t)
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
         (if Set.mem env x
          then x
          else failwith (Format.sprintf "anf: variable `%s` not found" x)))
  | CFApp (CFApp (_, _), _) as app ->
    let* name = new_or_bind bind in
    let* body = cont (ImmId name) in
    let[@warning "-8"] rec collect_args acc = function
      | CFApp ((CFApp _ as inner), a) -> collect_args (a :: acc) inner
      | CFApp (f, a) -> f, a :: acc
    in
    let func, args = collect_args [] app in
    anf_expr env func (fun imm_func ->
      let rec anf_args args cont =
        match args with
        | [] -> cont []
        | head :: tail ->
          anf_expr env head (fun imm_hd ->
            anf_args tail (fun anf_tl -> cont (imm_hd :: anf_tl)))
      in
      anf_args args (fun imm_args ->
        return (ALet (name, CApp (imm_func, imm_args), body))))
  | CFApp (func, arg) ->
    let* name = new_or_bind bind in
    let* body = cont (ImmId name) in
    anf_expr env func (fun imm_func ->
      anf_expr env arg (fun imm_arg ->
        return (ALet (name, CApp (imm_func, [ imm_arg ]), body))))
  | CFIfElse (i, t, e) ->
    let* name = new_or_bind bind in
    let* body = cont (ImmId name) in
    anf_expr env i (fun i ->
      let* t = anf_expr env t (fun x -> return @@ ACExpr (CImmExpr x)) in
      let* e = anf_expr env e (fun x -> return @@ ACExpr (CImmExpr x)) in
      return (ALet (name, CIfElse (i, t, e), body)))
  | CFLetIn ("_", expr, body) ->
    anf_expr env expr (fun imm_expr ->
      let* body = anf_expr env body cont in
      return (ALet ("_", CImmExpr imm_expr, body)))
  | CFLetIn (id, expr, body) ->
    let new_env = Set.add env id in
    anf_expr ~bind:(Some id) env expr (fun imm_expr ->
      let* body = anf_expr new_env body cont in
      return (ALet (id, CImmExpr imm_expr, body)))
  | CFList xs ->
    let rec helper acc = function
      | [] -> cont (ImmList (List.rev acc))
      | x :: xs -> anf_expr env x (fun x -> helper (x :: acc) xs)
    in
    helper [] xs
  | CFTuple (x1, x2, xs) ->
    anf_expr env x1 (fun x1' ->
      anf_expr env x2 (fun x2' ->
        let rec helper acc = function
          | [] -> cont (ImmTuple (x1', x2', List.rev acc))
          | x :: rest -> anf_expr env x (fun x' -> helper (x' :: acc) rest)
        in
        helper [] xs))
;;

let anf_def env (def : cf_definition) =
  match def with
  | CFLet (flag, id, args, expr) ->
    let env = List.fold args ~init:env ~f:Set.add in
    let* aexpr = anf_expr env expr (fun x -> return (ACExpr (CImmExpr x))) in
    return (Fn (flag, id, args, aexpr), env)
;;

let default_env =
  Set.of_list (module String) (List.map ~f:(fun x -> x.name) (stdlib @ runtime))
;;

(** Removes `let ax = ... in ax` and `let ax = ay in ...` *)
let remove_useless_bindings fn =
  let remaps = Hashtbl.create (module String) in
  let rec find_rec id =
    match Hashtbl.find remaps id with
    | Some value -> find_rec value
    | None -> id
  in
  let useless =
    let rec useless_c = function
      | CIfElse (c, a1, a2) -> CIfElse (c, useless_a a1, useless_a a2)
      | x -> x
    and useless_a = function
      | ALet (id1, c, ACExpr (CImmExpr (ImmId id2))) when String.equal id1 id2 ->
        useless_a @@ ACExpr (useless_c c)
      | ALet (new_id, CImmExpr (ImmId old_id), a) when not (String.equal new_id old_id) ->
        Hashtbl.set remaps ~key:new_id ~data:old_id;
        useless_a a
      | ALet (id, c, a) -> ALet (id, useless_c c, useless_a a)
      | ACExpr c -> ACExpr (useless_c c)
    in
    function
    | Fn (f, id, args, a) -> Fn (f, id, args, useless_a a)
  in
  let remap =
    let rec remap_i = function
      | ImmId id -> ImmId (find_rec id)
      | ImmTuple (x1, x2, xs) -> ImmTuple (remap_i x1, remap_i x2, List.map ~f:remap_i xs)
      | ImmList xs -> ImmList (List.map ~f:remap_i xs)
      | x -> x
    and remap_c = function
      | CIfElse (c, a1, a2) -> CIfElse (remap_i c, remap_a a1, remap_a a2)
      | CApp (i1, i2) -> CApp (remap_i i1, List.map i2 ~f:remap_i)
      | CImmExpr i -> CImmExpr (remap_i i)
    and remap_a = function
      | ALet (id1, CImmExpr (ImmId id2), a) when String.equal id1 id2 -> remap_a a
      | ALet (id, c, a) -> ALet (find_rec id, remap_c c, remap_a a)
      | ACExpr c -> ACExpr (remap_c c)
    in
    function
    | Fn (f, id, args, a) -> Fn (f, id, args, remap_a a)
  in
  remap (useless fn)
;;

let collect_toplevels env =
  List.fold ~init:env ~f:(fun env ->
      function
      | CFLet (_, id, _, _) -> Set.add env id)
;;

let anf (ast : Cf_ast.program) : Anf_ast.program =
  let default_env = collect_toplevels default_env ast in
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
  run (helper ast) |> List.map ~f:remove_useless_bindings
;;
