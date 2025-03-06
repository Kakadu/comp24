(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Remove_patterns
open Common
open Base

let empty_bindings = Map.empty (module String)

let update_bindings last new1 =
  Map.merge_skewed last new1 ~combine:(fun ~key:_ _ v2 -> v2)
;;

let rec get_free binded = function
  | Rp_e_const _ -> StrSet.empty
  | Rp_e_ident ident ->
    if StrSet.find binded ident then StrSet.empty else StrSet.singleton ident
  | Rp_e_ite (e1, e2, e3) ->
    StrSet.union_list [ get_free binded e1; get_free binded e2; get_free binded e3 ]
  | Rp_e_fun (args, e) ->
    let binded = StrSet.union binded (StrSet.of_list args) in
    get_free binded e
  | Rp_e_app (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | Rp_e_let (Rp_non_rec (name, e1), e2) ->
    StrSet.union (get_free binded e1) (get_free (StrSet.add binded name) e2)
  | Rp_e_let (Rp_rec decls, e2) ->
    let idents, expr_list =
      List.fold_right decls ~init:(StrSet.empty, []) ~f:(fun (name, e) (ids, exprs) ->
        StrSet.add ids name, e :: exprs)
    in
    let binded = StrSet.union binded idents in
    List.fold expr_list ~init:(get_free binded e2) ~f:(fun acc e ->
      StrSet.union acc (get_free binded e))
  | Rp_e_cons_list (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | Rp_e_tuple e_list ->
    List.fold e_list ~init:StrSet.empty ~f:(fun acc e ->
      StrSet.union acc (get_free binded e))
;;

let create_app expr to_apply bindings =
  List.fold to_apply ~init:expr ~f:(fun acc name ->
    let expr_to_app =
      match Map.find bindings name with
      | Some e -> e
      | None -> Rp_e_ident name
    in
    Rp_e_app (acc, expr_to_app))
;;

let rec cc_expr global_env bindings = function
  | Rp_e_const _ as orig -> orig
  | Rp_e_ident ident as orig ->
    (match Map.find bindings ident with
     | Some new_expr -> new_expr
     | None -> orig)
  | Rp_e_ite (if1, then1, else1) ->
    let if1 = cc_expr global_env bindings if1 in
    let then1 = cc_expr global_env bindings then1 in
    let else1 = cc_expr global_env bindings else1 in
    Rp_e_ite (if1, then1, else1)
    (* only for anonymous functions *)
  | Rp_e_fun (args, body) as orig ->
    let free = get_free global_env orig |> StrSet.to_list in
    let new_body = cc_expr global_env empty_bindings body in
    let new_fun = Rp_e_fun (free @ args, new_body) in
    create_app new_fun free bindings
  | Rp_e_app (e1, e2) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    Rp_e_app (e1, e2)
  | Rp_e_let (Rp_non_rec (name, e1), e2) ->
    let new_e1, new_bindings = cc_decl_non_rec global_env bindings name e1 in
    let bindings = update_bindings bindings new_bindings in
    let new_e2 = cc_expr global_env bindings e2 in
    Rp_e_let (Rp_non_rec (name, new_e1), new_e2)
  | Rp_e_let (Rp_rec decl_list, e2) ->
    let new_decl_list, new_bindings = cc_decl_rec global_env bindings decl_list in
    let bindings = update_bindings bindings new_bindings in
    let new_e2 = cc_expr global_env bindings e2 in
    Rp_e_let (Rp_rec new_decl_list, new_e2)
  | Rp_e_cons_list (e1, e2) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    Rp_e_cons_list (e1, e2)
  | Rp_e_tuple e_list ->
    let e_list = List.map e_list ~f:(cc_expr global_env bindings) in
    Rp_e_tuple e_list

and cc_decl_non_rec global_env bindings name = function
  | Rp_e_fun (args, body) ->
    let free =
      StrSet.diff (get_free global_env body) (StrSet.of_list args) |> StrSet.to_list
    in
    let new_body = cc_expr global_env empty_bindings body in
    let new_fun = Rp_e_fun (free @ args, new_body) in
    let new_bind = create_app (Rp_e_ident name) free bindings in
    new_fun, Map.singleton (module String) name new_bind
  | expr -> cc_expr global_env bindings expr, empty_bindings

and cc_decl_rec global_env last_bindings decl_list =
  let names = List.map decl_list ~f:(fun (name, _) -> name) in
  let f1 (free_list, bindings) (name, expr) =
    match expr with
    | Rp_e_fun (args, body) ->
      let to_exclude = StrSet.union (StrSet.of_list names) (StrSet.of_list args) in
      let free = StrSet.diff (get_free global_env body) to_exclude |> StrSet.to_list in
      let bind = create_app (Rp_e_ident name) free last_bindings in
      let bindings = Map.update bindings name ~f:(fun _ -> bind) in
      free :: free_list, bindings
    | _ -> [] :: free_list, bindings
  in
  let free_list, bindings = List.fold decl_list ~init:([], last_bindings) ~f:f1 in
  let free_list = List.rev free_list in
  let to_fold = List.zip_exn decl_list free_list in
  let f1 decl_acc ((name, expr), free) =
    match expr with
    | Rp_e_fun (args, body) ->
      let new_body = cc_expr global_env empty_bindings body in
      let new_fun = Rp_e_fun (free @ args, new_body) in
      (name, new_fun) :: decl_acc
    | _ ->
      let new_expr = cc_expr global_env bindings expr in
      (name, new_expr) :: decl_acc
  in
  let new_decls = List.fold to_fold ~init:[] ~f:f1 in
  let new_decls = List.rev new_decls in
  new_decls, bindings
;;

let cc_toplevel global_env = function
  | Rp_non_rec (name, e) ->
    let new_decl, _ = cc_decl_non_rec global_env empty_bindings name e in
    let new_env = StrSet.add global_env name in
    new_env, Rp_non_rec (name, new_decl)
  | Rp_rec decl_list ->
    let names = List.map decl_list ~f:(fun (name, _) -> name) in
    let new_decls, _ = cc_decl_rec global_env empty_bindings decl_list in
    let new_env =
      List.fold names ~init:global_env ~f:(fun acc name -> StrSet.add acc name)
    in
    new_env, Rp_rec new_decls
;;

let std_names = List.fold Std_names.std_names ~init:StrSet.empty ~f:StrSet.add

let run_closure_conversion_program program =
  let rec helper last_env = function
    | [] -> []
    | hd :: tl ->
      let cur_env, cur_ast = cc_toplevel last_env hd in
      cur_ast :: helper cur_env tl
  in
  helper std_names program
;;
