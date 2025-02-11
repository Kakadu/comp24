(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Base

module StrSet : sig
  type t

  val empty : t
  val singleton : string -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val add : t -> string -> t
  val find : t -> string -> bool
  val to_list : t -> string list
  val of_list : string list -> t
  val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a
  val diff : t -> t -> t
end = struct
  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst

  let find s str =
    match Set.binary_search s ~compare:String.compare `First_equal_to str with
    | Some _ -> true
    | None -> false
  ;;

  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end

let emptyBindings = Map.empty (module String)

let update_bindings last new1 =
  Map.merge_skewed last new1 ~combine:(fun ~key:_ _ v2 -> v2)
;;

let rec get_idents = function
  | P_typed (pat, _) -> get_idents pat
  | P_any | P_const _ -> StrSet.empty
  | P_val ident -> StrSet.singleton ident
  | P_cons_list (p1, p2) -> StrSet.union (get_idents p1) (get_idents p2)
  | P_tuple (hd, tl) -> StrSet.union (get_idents hd) (get_idents_from_list tl)

and get_idents_from_list pat_list =
  List.fold pat_list ~init:StrSet.empty ~f:(fun acc p -> StrSet.union acc (get_idents p))
;;

let rec get_free binded = function
  | E_typed (e, _) -> get_free binded e
  | E_const _ -> StrSet.empty
  | E_ident ident ->
    if StrSet.find binded ident then StrSet.empty else StrSet.singleton ident
  | E_ite (e1, e2, e3) ->
    StrSet.union_list [ get_free binded e1; get_free binded e2; get_free binded e3 ]
  | E_fun (first, other, e) ->
    let idents = get_idents_from_list (first :: other) in
    let binded = StrSet.union binded idents in
    get_free binded e
  | E_app (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | E_let (Non_rec (pat, _, e1), e2) ->
    let idents = get_idents pat in
    StrSet.union (get_free binded e1) (get_free (StrSet.union idents binded) e2)
  | E_let (Rec decls, e2) ->
    let idents, expr_list =
      List.fold_right decls ~init:(StrSet.empty, []) ~f:(fun (pat, _, e) (ids, exprs) ->
        StrSet.union ids (get_idents pat), e :: exprs)
    in
    let binded = StrSet.union binded idents in
    List.fold expr_list ~init:(get_free binded e2) ~f:(fun acc e ->
      StrSet.union acc (get_free binded e))
  | E_match (e1, case_list) ->
    let e1_free = get_free binded e1 in
    List.fold case_list ~init:e1_free ~f:(fun acc (pat, e) ->
      let idents = get_idents pat in
      let cur_bind = StrSet.union binded idents in
      let free = get_free cur_bind e in
      StrSet.union acc free)
  | E_cons_list (e1, e2) -> StrSet.union (get_free binded e1) (get_free binded e2)
  | E_tuple (e1, e_list) ->
    let e1_free = get_free binded e1 in
    List.fold e_list ~init:e1_free ~f:(fun acc e -> StrSet.union acc (get_free binded e))
;;

let rec cc_expr global_env bindings = function
  | E_typed (e, _) -> cc_expr global_env bindings e
  | E_const _ as orig -> orig
  | E_ident ident as orig ->
    (match Map.find bindings ident with
     | Some vals_to_apply ->
       List.fold vals_to_apply ~init:orig ~f:(fun acc val1 -> E_app (acc, E_ident val1))
     | None -> orig)
  | E_ite (if1, then1, else1) ->
    let if1 = cc_expr global_env bindings if1 in
    let then1 = cc_expr global_env bindings then1 in
    let else1 = cc_expr global_env bindings else1 in
    E_ite (if1, then1, else1)
    (* only for anonymous functions *)
  | E_fun (first_arg, other_args, body) as orig ->
    let args = first_arg :: other_args in
    let idents = get_idents_from_list args in
    let bindings = StrSet.fold idents ~init:bindings ~f:(fun acc a -> Map.remove acc a) in
    let free = get_free global_env orig |> StrSet.to_list in
    let free_patterns = List.map free ~f:(fun x -> P_val x) in
    let new_args = free_patterns @ args in
    let new_body = cc_expr global_env bindings body in
    let new_fun = E_fun (List.hd_exn new_args, List.tl_exn new_args, new_body) in
    let new_app =
      List.fold free ~init:new_fun ~f:(fun acc name -> E_app (acc, E_ident name))
    in
    new_app
  | E_app (e1, e2) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    E_app (e1, e2)
  | E_let (Non_rec (pat, _, e1), e2) ->
    let new_e1, bindings = cc_decl_non_rec global_env bindings (pat, e1) in
    let new_e2 = cc_expr global_env bindings e2 in
    E_let (Non_rec (pat, None, new_e1), new_e2)
  | E_let (Rec decl_list, e2) ->
    let new_decl_list, bindings = cc_decl_rec global_env bindings decl_list in
    let new_e2 = cc_expr global_env bindings e2 in
    E_let (Rec new_decl_list, new_e2)
  | _ -> E_const C_unit

and cc_non_rec_fun global_env bindings args body =
  let args_idents = get_idents_from_list args in
  let free = StrSet.diff (get_free global_env body) args_idents |> StrSet.to_list in
  let free_patterns = List.map free ~f:(fun x -> P_val x) in
  let new_args = free_patterns @ args in
  let bindings =
    StrSet.fold args_idents ~init:bindings ~f:(fun acc a -> Map.remove acc a)
  in
  let new_body = cc_expr global_env bindings body in
  E_fun (List.hd_exn new_args, List.tl_exn new_args, new_body), free

and cc_decl_non_rec global_env bindings = function
  | P_val ident, expr ->
    (match expr with
     | E_fun (first_arg, other_args, body) ->
       let new_expr, free =
         cc_non_rec_fun global_env bindings (first_arg :: other_args) body
       in
       new_expr, Map.singleton (module String) ident free
     | _ -> cc_expr global_env bindings expr, emptyBindings)
  | P_tuple (p_hd, p_tl), E_tuple (e_hd, e_tl) ->
    let new_expr_hd, new_mp_hd = cc_decl_non_rec global_env bindings (p_hd, e_hd) in
    let new_expr_tl, new_mp_tl =
      List.unzip
        (List.map2_exn p_tl e_tl ~f:(fun p e ->
           cc_decl_non_rec global_env bindings (p, e)))
    in
    let bindings = List.fold new_mp_tl ~init:new_mp_hd ~f:update_bindings in
    E_tuple (new_expr_hd, new_expr_tl), bindings
  | P_cons_list (p1, p2), E_cons_list (e1, e2) ->
    let new_e1, mp1 = cc_decl_non_rec global_env bindings (p1, e1) in
    let new_e2, mp2 = cc_decl_non_rec global_env bindings (p2, e2) in
    E_cons_list (new_e1, new_e2), update_bindings mp1 mp2
  | _, expr -> cc_expr global_env bindings expr, emptyBindings

and cc_decl_rec global_env bindings decl_list =
  let names_bodies =
    List.map decl_list ~f:(fun (pat, _, expr) ->
      match pat with
      | P_val name -> name, expr
      | _ -> "", expr)
  in
  let names = StrSet.of_list (fst (List.unzip names_bodies)) in
  let global_env = StrSet.union names global_env in
  let f1 (free_list, bindings) (name, expr) =
    match expr with
    | E_fun (first, other, body) ->
      let args_idents = get_idents_from_list (first :: other) in
      let free = StrSet.diff (get_free global_env body) args_idents |> StrSet.to_list in
      let bindings = Map.update bindings name ~f:(fun _ -> free) in
      free :: free_list, bindings
    | _ -> [] :: free_list, bindings
  in
  let free_list, bindings = List.fold names_bodies ~init:([], bindings) ~f:f1 in
  let free_list = List.rev free_list in
  let to_fold = List.zip_exn names_bodies free_list in
  let f1 decl_acc ((name, expr), free) =
    match expr with
    | E_fun (first, other, body) ->
      let free_patterns = List.map free ~f:(fun x -> P_val x) in
      let new_args = free_patterns @ (first :: other) in
      let bindings = List.fold free ~init:bindings ~f:(fun acc a -> Map.remove acc a) in
      let new_body = cc_expr global_env bindings body in
      let new_fun = E_fun (List.hd_exn new_args, List.tl_exn new_args, new_body) in
      (P_val name, None, new_fun) :: decl_acc
    | _ ->
      let new_expr = cc_expr global_env bindings expr in
      (P_val name, None, new_expr) :: decl_acc
  in
  let new_decls = List.fold to_fold ~init:[] ~f:f1 in
  let new_decls = List.rev new_decls in
  new_decls, bindings
;;

let toplevel global_env = function
  | Expr e -> global_env, Expr (cc_expr global_env emptyBindings e)
  | Let_decl (Non_rec (pat, _, e)) ->
    let idents = get_idents pat in
    let new_decl, _ = cc_decl_non_rec global_env emptyBindings (pat, e) in
    let new_env =
      StrSet.fold idents ~init:global_env ~f:(fun acc name -> StrSet.add acc name)
    in
    new_env, Let_decl (Non_rec (pat, None, new_decl))
  | Let_decl (Rec decl_list) ->
    let patterns = List.map decl_list ~f:(fun (p, _, _) -> p) in
    let idents = get_idents_from_list patterns in
    let new_decls, _ = cc_decl_rec global_env emptyBindings decl_list in
    let new_env =
      StrSet.fold idents ~init:global_env ~f:(fun acc name -> StrSet.add acc name)
    in
    new_env, Let_decl (Rec new_decls)
;;

let std_names = List.fold Std_names.std_names ~init:StrSet.empty ~f:StrSet.add

let run_closure_conversion_program program =
  let rec helper last_env = function
    | [] -> []
    | hd :: tl ->
      let cur_env, cur_ast = toplevel last_env hd in
      cur_ast :: helper cur_env tl
  in
  helper std_names program
;;

let run_closure_conversion_expr expr = cc_expr std_names emptyBindings expr
