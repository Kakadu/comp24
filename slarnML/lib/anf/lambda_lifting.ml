(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ll_ast
open Cc_ast
open Res

let new_anon = map (fun (ast, prog, env, num) -> Result (ast, prog, env, num + 1))

let get_anon_name =
  map (fun (_, _, _, num) -> Result (String.concat "$" [ "anon"; string_of_int num ]))
;;

let get_name id stack = String.concat "#" (id :: stack)

let find_name id =
  map (fun (_, _, env, _) ->
    match List.find_opt (fun (_, name, _) -> name = id) env with
    | None ->
      Result (LApp (id, []))
      (* Error (String.concat "" ["Not found new name '"; id; "'\n"]) *)
    | Some (_, _, new_name) -> Result (LId new_name))
;;

let insert_let a = map (fun (ast, lst, env, num) -> Result (ast, a :: lst, env, num))

let update_env name new_name lvl =
  map (fun (ast, prog, env, num) -> Result (ast, prog, (lvl, name, new_name) :: env, num))
;;

let update_env_fun name stack lvl = update_env name (get_name name stack) lvl
let update_env_arg name lvl = update_env name name lvl
let get_ast = map (fun (ast, _, _, _) -> Result ast)
let get_prog = map (fun (_, prog, _, _) -> Result prog)

let update_ast f =
  map (fun (ast, prog, env, num) ->
    f ast >>= fun new_ast -> Result (new_ast, prog, env, num))
;;

let filter lvl =
  map (fun (ast, prog, env, num) ->
    Result (ast, prog, List.filter (fun (l, _, _) -> l < lvl) env, num))
;;

let rec lifting cc_ast stack lvl res =
  let lifting_bin_op f e1 e2 =
    res
    |> lifting e1 stack lvl
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 -> r1 |> lifting e2 stack lvl |> update_ast (fun a2 -> Result (f a1 a2))
  in
  let get_id = function
    | Ast.Decl (id, _) | Ast.DeclRec (id, _) -> id
  in
  let get_args = function
    | Ast.Decl (_, args) | Ast.DeclRec (_, args) -> args
  in
  let get_fun_let d e = LFun (get_id d, get_args d, e) in
  let get_decl = function
    | Ast.Decl (id, args) -> Ast.Decl (get_name id stack, args)
    | Ast.DeclRec (id, args) -> Ast.DeclRec (get_name id stack, args)
  in
  let update_env_decl args res =
    List.fold_left (fun r a -> r |> update_env_arg a lvl) res args
  in
  match cc_ast with
  | CId id -> res |> find_name id >>= fun ast -> update_ast (fun _ -> Result ast) res
  | CConst c -> update_ast (fun _ -> Result (LConst c)) res
  | CNot e -> res |> lifting e stack lvl
  | COr (e1, e2) -> lifting_bin_op (fun a1 a2 -> LOr (a1, a2)) e1 e2
  | CAnd (e1, e2) -> lifting_bin_op (fun a1 a2 -> LAnd (a1, a2)) e1 e2
  | CEq (e1, e2) -> lifting_bin_op (fun a1 a2 -> LEq (a1, a2)) e1 e2
  | CGt (e1, e2) -> lifting_bin_op (fun a1 a2 -> LGt (a1, a2)) e1 e2
  | CLt (e1, e2) -> lifting_bin_op (fun a1 a2 -> LLt (a1, a2)) e1 e2
  | CGte (e1, e2) -> lifting_bin_op (fun a1 a2 -> LGte (a1, a2)) e1 e2
  | CLte (e1, e2) -> lifting_bin_op (fun a1 a2 -> LLte (a1, a2)) e1 e2
  | CAdd (e1, e2) -> lifting_bin_op (fun a1 a2 -> LAdd (a1, a2)) e1 e2
  | CSub (e1, e2) -> lifting_bin_op (fun a1 a2 -> LSub (a1, a2)) e1 e2
  | CMul (e1, e2) -> lifting_bin_op (fun a1 a2 -> LMul (a1, a2)) e1 e2
  | CDiv (e1, e2) -> lifting_bin_op (fun a1 a2 -> LDiv (a1, a2)) e1 e2
  | CIf (e1, e2, e3) ->
    res
    |> lifting e1 stack lvl
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    r1
    |> lifting e2 stack lvl
    |> fun r2 ->
    r2
    |> get_ast
    >>= fun a2 ->
    r2 |> lifting e3 stack lvl |> update_ast (fun a3 -> Result (LIf (a1, a2, a3)))
  | CLet (d, e) ->
    let id = get_id d in
    res
    |> update_env_decl (get_args d)
    |> update_env_fun id stack lvl
    |> lifting e (id :: stack) (lvl + 1)
    |> fun r1 ->
    r1 |> get_ast >>= fun a -> r1 |> insert_let (get_fun_let (get_decl d) a) |> filter lvl
  | CLetIn (d, e1, e2) ->
    let id = get_id d in
    res
    |> update_env_decl (get_args d)
    |> update_env_fun id stack lvl
    |> lifting e1 (id :: stack) (lvl + 1)
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    (if id = "()" then r1 else r1 |> insert_let (get_fun_let (get_decl d) a1))
    |> lifting e2 stack lvl
    |> update_ast (fun a2 ->
      Result (LIn ((if id = "()" then id else get_name id stack), a1, a2)))
    |> filter lvl
  | CFun (args, e) ->
    res
    |> new_anon
    |> update_env_decl args
    |> fun res ->
    res
    |> get_anon_name
    >>= fun name ->
    let new_name = get_name name stack in
    res
    |> lifting e (name :: stack) (lvl + 1)
    |> fun r ->
    r
    |> get_ast
    >>= (fun a -> r |> insert_let (get_fun_let (Ast.Decl (new_name, args)) a))
    |> update_ast (fun _ -> Result (LId new_name))
  | CApp (e, args) ->
    List.fold_left
      (fun r e ->
        r
        >>= fun (r, lst) ->
        Result r
        |> lifting e stack lvl
        >>= fun res -> Result res |> get_ast >>= fun a -> Result (res, a :: lst))
      (res >>= fun r -> Result (r, []))
      args
    >>= fun (r, args) ->
    let args = List.rev args in
    Result r
    |> lifting e stack lvl
    |> update_ast (fun a ->
      match a with
      | LApp (a, new_args) -> Result (LApp (a, List.append new_args args))
      | LId a -> Result (LApp (a, args))
      | _ -> Error "Apply on not correct expr")
;;

let default_res = Result (LId "Error", [], [], 0)

let lambda_lifting cc_ast =
  List.fold_left
    (fun ll_ast ast ->
      ll_ast
      >>= fun ll_ast ->
      lifting ast [] 0 default_res |> get_prog >>= fun p -> Result (ll_ast @ List.rev p))
    (Result [])
    cc_ast
;;
