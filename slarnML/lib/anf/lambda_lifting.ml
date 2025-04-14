(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ll_ast
open Cc_ast
open Res

let new_anon = map (fun (ast, prog, env, num) -> Result (ast, prog, env, num + 1))

let get_anon_name =
  map (fun (_, _, _, num) -> Result (String.concat "$" [ "anon"; string_of_int num ]))
;;

let get_name id _ =
  if String.contains id '#' then String.sub id 0 (String.index id '#') else id
;;

let find_name args fun_ids id =
  map (fun (_, _, env, _) ->
    (* print_endline ("find_name " ^ id ^ " " ^ String.concat ", " fun_ids); *)
    match List.find_opt (fun (_, name, _) -> name = id) env with
    | None -> if List.mem id fun_ids then Result (LApp (LId id, [])) else Result (LId id)
    | Some (_, _, new_name) ->
      if List.mem id fun_ids && not (List.mem new_name args)
      then Result (LApp (LId new_name, []))
      else Result (LId new_name))
;;

let insert_let a = map (fun (ast, lst, env, num) -> Result (ast, a :: lst, env, num))

let update_env name new_name lvl =
  map (fun (ast, prog, env, num) -> Result (ast, prog, (lvl, name, new_name) :: env, num))
;;

let update_env_fun name stack lvl =
  map (fun (ast, prog, env, num) ->
    let new_name = get_name name stack in
    Result (ast, prog, (lvl, name, new_name) :: env, num))
;;

let update_env_arg name lvl = update_env name name lvl
let get_ast = map (fun (ast, _, _, _) -> Result ast)
let get_prog = map (fun (_, prog, _, _) -> Result prog)
let get_num = map (fun (_, _, _, num) -> Result num)

let update_ast f =
  map (fun (ast, prog, env, num) ->
    f ast >>= fun new_ast -> Result (new_ast, prog, env, num))
;;

let filter lvl =
  map (fun (ast, prog, env, num) ->
    Result (ast, prog, List.filter (fun (l, _, _) -> l < lvl) env, num))
;;

let rec lifting cc_ast fun_ids g_args stack lvl res =
  let lifting_bin_op f e1 e2 =
    res
    |> lifting e1 fun_ids g_args stack lvl
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    r1 |> lifting e2 fun_ids g_args stack lvl |> update_ast (fun a2 -> Result (f a1 a2))
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
  let init_func d e1 res =
    let id = get_id d in
    let args = get_args d @ g_args in
    let f1, f2 =
      match d with
      | Ast.Decl _ -> (fun x -> x), update_env_decl (get_args d)
      | Ast.DeclRec _ -> update_env_decl (get_args d), fun x -> x
    in
    let funs = if List.length args = 0 then fun_ids else id :: fun_ids in
    res
    |> f1
    |> update_env_fun id stack lvl
    |> lifting e1 funs args (id :: stack) (lvl + 1)
    |> f2
  in
  match cc_ast with
  | CId id ->
    res |> find_name g_args fun_ids id >>= fun ast -> update_ast (fun _ -> Result ast) res
  | CConst c -> update_ast (fun _ -> Result (LConst c)) res
  | CNot e -> res |> lifting e fun_ids g_args stack lvl
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
    |> lifting e1 fun_ids g_args stack lvl
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    r1
    |> lifting e2 fun_ids g_args stack lvl
    |> fun r2 ->
    r2
    |> get_ast
    >>= fun a2 ->
    r2
    |> lifting e3 fun_ids g_args stack lvl
    |> update_ast (fun a3 -> Result (LIf (a1, a2, a3)))
  | CLet (d, e) ->
    (* let id = get_id d in *)
    res
    |> init_func d e
    |> fun r1 ->
    r1 |> get_ast >>= fun a -> r1 |> insert_let (get_fun_let (get_decl d) a) |> filter lvl
  | CLetIn (d, e1, e2) ->
    let id = get_id d in
    let e2_funs = if List.length (get_args d) = 0 then fun_ids else id :: fun_ids in
    res
    |> init_func d e1
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    (if List.length (get_args d) = 0
     then r1
     else r1 |> insert_let (get_fun_let (get_decl d) a1))
    |> lifting e2 e2_funs g_args stack lvl
    |> update_ast (fun a2 -> Result (LIn (id, a1, a2)))
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
    |> lifting e fun_ids (args @ g_args) (name :: stack) (lvl + 1)
    |> fun r ->
    r
    |> get_ast
    >>= (fun a -> r |> insert_let (get_fun_let (Ast.Decl (new_name, args)) a))
    |> update_ast (fun _ -> Result (LApp (LId new_name, [])))
  | CApp (e, args) ->
    List.fold_left
      (fun r e ->
        r
        >>= fun (r, lst) ->
        Result r
        |> lifting e fun_ids g_args stack lvl
        >>= fun res -> Result res |> get_ast >>= fun a -> Result (res, a :: lst))
      (res >>= fun r -> Result (r, []))
      args
    >>= fun (r, args) ->
    let args = List.rev args in
    Result r
    |> lifting e fun_ids g_args stack lvl
    |> update_ast (fun a ->
      match a with
      | LApp (a, new_args) -> Result (LApp (a, List.append new_args args))
      | LId a -> Result (LApp (LId a, args))
      | _ -> Error "Apply on not correct expr")
;;

let rec unwrap_app expr =
  match expr with
  | LApp (id, args) ->
    (match args with
     | [] -> expr
     | [ arg ] -> LApp (unwrap_app id, [ unwrap_app arg ])
     | fst :: args ->
       List.fold_left
         (fun app arg -> LApp (app, [ unwrap_app arg ]))
         (LApp (unwrap_app id, [ unwrap_app fst ]))
         args)
  | LId _ | LConst _ -> expr
  | LNot e -> LNot (unwrap_app e)
  | LOr (e1, e2) -> LOr (unwrap_app e1, unwrap_app e2)
  | LAnd (e1, e2) -> LAnd (unwrap_app e1, unwrap_app e2)
  | LEq (e1, e2) -> LEq (unwrap_app e1, unwrap_app e2)
  | LGt (e1, e2) -> LGt (unwrap_app e1, unwrap_app e2)
  | LLt (e1, e2) -> LLt (unwrap_app e1, unwrap_app e2)
  | LGte (e1, e2) -> LGte (unwrap_app e1, unwrap_app e2)
  | LLte (e1, e2) -> LLte (unwrap_app e1, unwrap_app e2)
  | LAdd (e1, e2) -> LAdd (unwrap_app e1, unwrap_app e2)
  | LSub (e1, e2) -> LSub (unwrap_app e1, unwrap_app e2)
  | LMul (e1, e2) -> LMul (unwrap_app e1, unwrap_app e2)
  | LDiv (e1, e2) -> LDiv (unwrap_app e1, unwrap_app e2)
  | LIf (e1, e2, e3) -> LIf (unwrap_app e1, unwrap_app e2, unwrap_app e3)
  | LIn (id, e1, e2) -> LIn (id, unwrap_app e1, unwrap_app e2)
;;

let default_res num = Result (LId "Error", [], [], num)

let lambda_lifting cc_ast =
  List.fold_left
    (fun prev_res ast ->
      prev_res
      >>= fun (anon_num, ll_ast) ->
      let funs =
        List.map
          (fun e ->
            match e with
            | LFun (id, _, _) -> id)
          ll_ast
      in
      lifting ast funs [] [] 0 (default_res anon_num)
      |> fun res ->
      res
      |> get_num
      >>= fun num -> res |> get_prog >>= fun p -> Result (num, ll_ast @ List.rev p))
    (Result (0, []))
    cc_ast
  >>= fun (_, ast) ->
  Result ast
  >>= fun g_ast ->
  Result
    (List.fold_left
       (fun acc ast ->
         match ast with
         | LFun (id, args, e) -> acc @ [ LFun (id, args, unwrap_app e) ])
       []
       g_ast)
;;
