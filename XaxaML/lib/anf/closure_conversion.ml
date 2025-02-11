(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Remove_patterns
open Common
open Base

let emptyBindings = Map.empty (module String)

let update_bindings last new1 =
  Map.merge_skewed last new1 ~combine:(fun ~key:_ _ v2 -> v2)
;;

let rec get_idents = function
  | Rp_p_any | Rp_p_const _ -> StrSet.empty
  | Rp_p_val ident -> StrSet.singleton ident
  | Rp_p_cons_list (p1, p2) -> StrSet.union (get_idents p1) (get_idents p2)
  | Rp_p_tuple (hd, tl) -> StrSet.union (get_idents hd) (get_idents_from_list tl)

and get_idents_from_list pat_list =
  List.fold pat_list ~init:StrSet.empty ~f:(fun acc p -> StrSet.union acc (get_idents p))
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
  | Rp_e_match (e1, case_list) ->
    let e1_free = get_free binded e1 in
    List.fold case_list ~init:e1_free ~f:(fun acc (pat, e) ->
      let idents = get_idents pat in
      let cur_bind = StrSet.union binded idents in
      let free = get_free cur_bind e in
      StrSet.union acc free)
  | Rp_e_cons_list (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | Rp_e_tuple (e1, e_list) ->
    let e1_free = get_free binded e1 in
    List.fold e_list ~init:e1_free ~f:(fun acc e -> StrSet.union acc (get_free binded e))
;;

let rec cc_expr global_env bindings = function
  | Rp_e_const _ as orig -> orig
  | Rp_e_ident ident as orig ->
    (match Map.find bindings ident with
     | Some vals_to_apply ->
       List.fold vals_to_apply ~init:orig ~f:(fun acc val1 ->
         Rp_e_app (acc, Rp_e_ident val1))
     | None -> orig)
  | Rp_e_ite (if1, then1, else1) ->
    let if1 = cc_expr global_env bindings if1 in
    let then1 = cc_expr global_env bindings then1 in
    let else1 = cc_expr global_env bindings else1 in
    Rp_e_ite (if1, then1, else1)
    (* only for anonymous functions *)
  | Rp_e_fun (args, body) as orig ->
    let bindings =
      StrSet.fold (StrSet.of_list args) ~init:bindings ~f:(fun acc a -> Map.remove acc a)
    in
    let free = get_free global_env orig |> StrSet.to_list in
    let new_body = cc_expr global_env bindings body in
    let new_fun = Rp_e_fun (free @ args, new_body) in
    let new_app =
      List.fold free ~init:new_fun ~f:(fun acc name -> Rp_e_app (acc, Rp_e_ident name))
    in
    new_app
  | Rp_e_app (e1, e2) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    Rp_e_app (e1, e2)
  | Rp_e_let (Rp_non_rec (name, e1), e2) ->
    let new_e1, bindings = cc_decl_non_rec global_env bindings name e1 in
    let new_e2 = cc_expr global_env bindings e2 in
    Rp_e_let (Rp_non_rec (name, new_e1), new_e2)
  | Rp_e_let (Rp_rec decl_list, e2) ->
    let new_decl_list, bindings = cc_decl_rec global_env bindings decl_list in
    let new_e2 = cc_expr global_env bindings e2 in
    Rp_e_let (Rp_rec new_decl_list, new_e2)
  | Rp_e_match (e, case_list) -> 
    let e = cc_expr global_env bindings e in
     
    let case_list = List.map case_list ~f:(fun (p, e) -> p * (cc_expr e)) in 
    Rp_e_match (e, case_list)
  | _ -> Rp_e_const Rp_c_unit

(* and cc_non_rec_fun global_env bindings args body =
  let free =
    StrSet.diff (get_free global_env body) (StrSet.of_list args) |> StrSet.to_list
  in
  let new_args = free @ args in
  let bindings = List.fold args ~init:bindings ~f:(fun acc a -> Map.remove acc a) in
  let new_body = cc_expr global_env bindings body in
  Rp_e_fun (new_args, new_body), free *)

and cc_decl_non_rec global_env bindings name = function
  | Rp_e_fun (args, body) ->
    let free =
      StrSet.diff (get_free global_env body) (StrSet.of_list args) |> StrSet.to_list
    in
    let bindings = List.fold args ~init:bindings ~f:(fun acc a -> Map.remove acc a) in
    let new_body = cc_expr global_env bindings body in
    Rp_e_fun (free @ args, new_body), Map.singleton (module String) name free
  | expr -> cc_expr global_env bindings expr, emptyBindings

and cc_decl_rec global_env bindings decl_list =
  let names = List.map decl_list ~f:(fun (name, _) -> name) in
  let global_env = StrSet.union (StrSet.of_list names) global_env in
  let f1 (free_list, bindings) (name, expr) =
    match expr with
    | Rp_e_fun (args, body) ->
      let free =
        StrSet.diff (get_free global_env body) (StrSet.of_list args) |> StrSet.to_list
      in
      let bindings = Map.update bindings name ~f:(fun _ -> free) in
      free :: free_list, bindings
    | _ -> [] :: free_list, bindings
  in
  let free_list, bindings = List.fold decl_list ~init:([], bindings) ~f:f1 in
  let free_list = List.rev free_list in
  let to_fold = List.zip_exn decl_list free_list in
  let f1 decl_acc ((name, expr), free) =
    match expr with
    | Rp_e_fun (args, body) ->
      let bindings = List.fold free ~init:bindings ~f:(fun acc a -> Map.remove acc a) in
      let new_body = cc_expr global_env bindings body in
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
  | Rp_expr e -> global_env, Rp_expr (cc_expr global_env emptyBindings e)
  | Rp_let_decl (Rp_non_rec (name, e)) ->
    let new_decl, _ = cc_decl_non_rec global_env emptyBindings name e in
    let new_env = StrSet.add global_env name in
    new_env, Rp_let_decl (Rp_non_rec (name, new_decl))
  | Rp_let_decl (Rp_rec decl_list) ->
    let names = List.map decl_list ~f:(fun (name, _) -> name) in
    let new_decls, _ = cc_decl_rec global_env emptyBindings decl_list in
    let new_env =
      List.fold names ~init:global_env ~f:(fun acc name -> StrSet.add acc name)
    in
    new_env, Rp_let_decl (Rp_rec new_decls)
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

let run_closure_conversion_expr expr = cc_expr std_names emptyBindings expr
