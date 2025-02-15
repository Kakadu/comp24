(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open StatementInfer
open Help
open Stdlib_funs

let add_stdfun : std_fun -> (state, unit) t =
  fun (name, _, fun_tp, tp) ->
  match fun_tp with
  | UserFun ->
    let tvs = get_tv_from_tp SetString.empty tp in
    if SetString.is_empty tvs
    then write_flat_var_type name tp
    else write_var_type name (TFSchem (tvs, tp))
  | SystemFun -> return ()
;;

let add_all_std_funs = map_list add_stdfun stdlib_funs
let init_start_state = add_all_std_funs
let empty_state : state = MapString.empty, [], 0, SetString.empty
let (start_state : state), res = run init_start_state empty_state

let _check =
  match res with
  | Result.Error x ->
    let _ = Format.printf "Error during init start state for infer:\n\t%s\n" x in
    exit (-1)
  | _ -> ()
;;

(* Function to traverse type_name and collect TPoly names *)
let rec collect_uts_type_name (tp : Ast.type_name) (acc : SetString.t) : SetString.t =
  match tp with
  | TPoly name -> SetString.add name acc
  | TTuple lst ->
    List.fold_left (fun acc_child t -> collect_uts_type_name t acc_child) acc lst
  | TFunction (tp_arg, tp_ret) ->
    let acc = collect_uts_type_name tp_arg acc in
    collect_uts_type_name tp_ret acc
  | TList t -> collect_uts_type_name t acc
  | TInt | TUnit | TBool -> acc
;;

(* Function to traverse patterns and collect type_names *)
let rec collect_uts_pattern (pat : Ast.pattern) (acc : SetString.t) : SetString.t =
  match pat with
  | PCons (p1, p2) ->
    let acc = collect_uts_pattern p1 acc in
    collect_uts_pattern p2 acc
  | PTuple lst ->
    List.fold_left (fun acc_child p -> collect_uts_pattern p acc_child) acc lst
  | PConstraint (p, tp) ->
    let acc = collect_uts_type_name tp acc in
    collect_uts_pattern p acc
  | PConstant _ | PIdentifier _ | PWildCard -> acc
;;

(* Function to traverse expressions and collect type_names *)
let rec collect_uts_expr (expr : Ast.expr) (acc : SetString.t) : SetString.t =
  match expr with
  | EFunction (pat, e) ->
    let acc = collect_uts_pattern pat acc in
    collect_uts_expr e acc
  | EApplication (e1, e2) ->
    let acc = collect_uts_expr e1 acc in
    collect_uts_expr e2 acc
  | EIfThenElse (e1, e2, e3) ->
    let acc = collect_uts_expr e1 acc in
    let acc = collect_uts_expr e2 acc in
    collect_uts_expr e3 acc
  | ELetIn (_flag, pat, e1, e2) ->
    let acc = collect_uts_pattern pat acc in
    let acc = collect_uts_expr e1 acc in
    collect_uts_expr e2 acc
  | ETuple lst -> List.fold_left (fun acc_child e -> collect_uts_expr e acc_child) acc lst
  | EMatch (exp, branches) ->
    let acc = collect_uts_expr exp acc in
    List.fold_left (fun acc_child (_, e) -> collect_uts_expr e acc_child) acc branches
  | EConstraint (e, tp) ->
    let acc = collect_uts_type_name tp acc in
    collect_uts_expr e acc
  | EIdentifier _ | EConstant _ -> acc
;;

(* Function to traverse single_let and collect type_names *)
let collect_uts_single_let (single : Ast.single_let) (acc : SetString.t) : SetString.t =
  match single with
  | DLet (pat, expr) ->
    let acc = collect_uts_pattern pat acc in
    collect_uts_expr expr acc
;;

(* Function to traverse let_declaration and collect type_names *)
let collect_uts_let_declaration (decl : Ast.let_declaration) (acc : SetString.t)
  : SetString.t
  =
  match decl with
  | DSingleLet (_, single) -> collect_uts_single_let single acc
  | DMutualRecDecl (_, singles) ->
    List.fold_left
      (fun acc_single single -> collect_uts_single_let single acc_single)
      acc
      singles
;;

let get_all_used_type_names declarations =
  List.fold_left
    (fun acc decl -> collect_uts_let_declaration decl acc)
    SetString.empty
    declarations
;;

let init_used_type_names declarations = write_uts (get_all_used_type_names declarations)
