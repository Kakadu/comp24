(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Cc_ast
open Res

let get_ast = map (fun (ast, _, _, _, _) -> Result ast)

let get_cc_args lvl =
  map (fun (_, _, p_args, _, _) ->
    let cc_args =
      List.map (fun (a, _) -> a) (List.filter (fun (_, l) -> l <= lvl) p_args)
    in
    Result cc_args)
;;

let get_app_args a_id res =
  match res with
  | Error _ -> []
  | Result (_, _, _, args, _) ->
    (match a_id with
     | CId id ->
       (match List.find_opt (fun (i, _, _) -> i = id) args with
        | None -> []
        | Some (_, args, _) -> List.map (fun a -> CId a) args)
     | _ -> [])
;;

let update_ast f =
  map (fun (ast, args, p_args, app_args, funs) ->
    Result (f ast, args, p_args, app_args, funs))
;;

let update_args c_args lvl =
  map (fun (ast, args, p_args, app_args, funs) ->
    Result
      (ast, List.append (List.map (fun a -> a, lvl) c_args) args, p_args, app_args, funs))
;;

let update_app id i_args lvl =
  map (fun (ast, args, p_args, app_args, funs) ->
    Result (ast, args, p_args, (id, i_args, lvl) :: app_args, funs))
;;

let update_func f =
  map (fun (ast, args, p_args, app_args, funs) ->
    Result (ast, args, p_args, app_args, f funs))
;;

let filter lvl =
  map (fun (ast, args, p_args, app_args, funs) ->
    Result
      ( ast
      , List.filter (fun (_, l) -> lvl >= l) args
      , List.filter (fun (_, l) -> lvl > l) p_args
      , List.filter (fun (_, _, l) -> lvl >= l) app_args
      , funs ))
;;

let simplify_id id lvl f =
  map (fun (ast, args, p_args, app_args, funs) ->
    match List.find_opt (fun (a, _) -> a = id) args with
    | None ->
      (match List.find_opt (fun a -> a = id) funs with
       | Some _ -> Result (f ast, args, p_args, app_args, funs)
       | None -> Error (String.concat "" [ id; " not exist" ]))
    | Some (_, l) ->
      if l = lvl
      then Result (f ast, args, p_args, app_args, funs)
      else (
        match List.find_opt (fun (a, _) -> a = id) p_args with
        | None -> Result (f ast, args, List.append p_args [ id, l ], app_args, funs)
        | _ -> Result (f ast, args, p_args, app_args, funs)))
;;

let rec simplify ast lvl f res =
  let f_id _ a = a in
  let simplify_bin_op f e1 e2 res =
    res
    |> simplify e1 lvl f_id
    |> fun res1 ->
    res1
    |> get_ast
    >>= fun a1 -> res1 |> simplify e2 lvl f_id |> update_ast (fun a2 -> f a1 a2)
  in
  match ast with
  | Id id -> res |> simplify_id id lvl (fun _ -> (f res) (CId id))
  | Const c -> res |> update_ast (fun _ -> CConst c)
  | Not e -> res |> simplify e lvl f_id |> update_ast (fun e -> CNot e)
  | Or (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> COr (e1, e2)) e1 e2
  | And (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CAnd (e1, e2)) e1 e2
  | Eq (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CEq (e1, e2)) e1 e2
  | Gt (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CGt (e1, e2)) e1 e2
  | Lt (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CLt (e1, e2)) e1 e2
  | Gte (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CGte (e1, e2)) e1 e2
  | Lte (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CLte (e1, e2)) e1 e2
  | Add (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CAdd (e1, e2)) e1 e2
  | Sub (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CSub (e1, e2)) e1 e2
  | Mul (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CMul (e1, e2)) e1 e2
  | Div (e1, e2) -> res |> simplify_bin_op (fun e1 e2 -> CDiv (e1, e2)) e1 e2
  | If (e1, e2, e3) ->
    res
    |> simplify e1 lvl f_id
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    r1
    |> simplify e2 lvl f_id
    |> fun r2 ->
    r2
    |> get_ast
    >>= fun a2 -> r2 |> simplify e3 lvl f_id |> update_ast (fun a3 -> CIf (a1, a2, a3))
  | Let (d, e) ->
    let id, args, env, dec =
      match d with
      | Decl (id, args) -> id, args, args, fun id args -> Decl (id, args)
      | DeclRec (id, args) -> id, args, id :: args, fun id args -> DeclRec (id, args)
    in
    res
    |> update_args env (lvl + 1)
    |> simplify e (lvl + 1) f_id
    |> get_cc_args lvl
    >>= fun new_args ->
    res
    |> update_app id new_args lvl
    |> update_args env (lvl + 1)
    |> simplify e (lvl + 1) f_id
    |> filter lvl
    |> update_ast (fun a -> CLet (dec id (List.append new_args args), a))
    |> update_args [ id ] lvl
  | LetIn (d, e1, e2) ->
    let id, args, env, dec =
      match d with
      | Decl (id, args) -> id, args, args, fun id args -> Decl (id, args)
      | DeclRec (id, args) -> id, args, id :: args, fun id args -> DeclRec (id, args)
    in
    res
    |> update_args env (lvl + 1)
    |> simplify e1 (lvl + 1) f_id
    |> get_cc_args lvl
    >>= fun new_args ->
    res
    |> update_app id new_args lvl
    |> update_args env (lvl + 1)
    |> simplify e1 (lvl + 1) f_id
    |> fun r1 ->
    r1
    |> get_ast
    >>= fun a1 ->
    r1
    |> filter lvl
    |> update_args [ id ] lvl
    |> simplify e2 lvl f_id
    |> update_ast (fun a2 -> CLetIn (dec id (List.append new_args args), a1, a2))
  | Fun (a, e) ->
    (match a with
     | [] -> Error "Fun hasn't args"
     | args ->
       res
       |> update_args args (lvl + 1)
       |> simplify e (lvl + 1) f_id
       |> fun r ->
       r
       |> get_cc_args lvl
       >>= fun new_args ->
       r
       |> filter lvl
       |> update_ast (fun a ->
         CApp (CFun (List.append new_args args, a), List.map (fun a -> CId a) new_args)))
  | App (func, args) ->
    List.fold_left
      (fun prev e ->
        prev
        >>= fun (ap, r) ->
        Result r
        |> simplify e lvl f_id
        >>= fun r -> Result r |> get_ast >>= fun a -> Result (a :: ap, r))
      (res >>= fun r -> Result ([], r))
      args
    >>= fun (r_args, res) ->
    let args = List.rev r_args in
    Result res
    |> simplify func lvl (fun r a -> CApp (a, get_app_args a r))
    |> update_ast (fun a -> CApp (a, args))
;;

let default_res = Result (CId "Error", [], [], [], [])

let get_func ast =
  match ast with
  | CLet (Decl (id, _), _) -> [ id ]
  | CLetIn (Decl (id, _), _, _) -> [ id ]
  | _ -> []
;;

let default_fun = List.map (fun (id, _) -> id) Call_define.default_func

let clos_conv ast =
  List.fold_left
    (fun cc_ast ast ->
      cc_ast
      >>= fun (cc_ast, funs) ->
      default_res
      |> update_func (fun _ -> funs)
      |> simplify ast 0 (fun _ a -> a)
      |> get_ast
      >>= fun ast -> Result (cc_ast @ [ ast ], get_func ast @ funs))
    (Result ([], default_fun))
    ast
  >>= fun (ast, _) -> Result ast
;;
