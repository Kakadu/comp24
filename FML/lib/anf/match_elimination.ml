(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base
open Me_ast
open Common
open StateMonad

let get_new_id n name = String.concat [ name; "_me"; Int.to_string n ]

let const_to_pe_const = function
  | CInt a -> Me_Cint a
  | CBool a -> Me_CBool a
;;

let rec pattern_remove pat =
  match pat with
  | PUnit -> [ "()" ]
  | PAny -> []
  | PConst _ -> []
  | PIdentifier id -> [ id ]
  | PNill -> [ "[]" ]
  | PTuple pats -> List.concat_map ~f:pattern_remove pats
  | PCons (hd, tl) -> pattern_remove hd @ pattern_remove tl
  | PConstraint (p, _) -> pattern_remove p
;;

let rec pattern_bindings expr pat =
  match pat with
  | PIdentifier id when String.(id <> "_") -> [ id, expr ]
  | PIdentifier _ -> []
  | PAny -> []
  | PConst _ -> []
  | PUnit -> []
  | PNill -> []
  | PCons (hd, tl) ->
    let hd_expr = Me_EApp (Me_EIdentifier "hd_list_get", expr) in
    let tl_expr = Me_EApp (Me_EIdentifier "tl_list_get", expr) in
    pattern_bindings hd_expr hd @ pattern_bindings tl_expr tl
  | PTuple pats ->
    List.mapi pats ~f:(fun i p ->
      let ith_expr =
        Me_EApp (Me_EIdentifier "tuple_get", Me_ETuple [ expr; Me_EConst (Me_Cint i) ])
      in
      pattern_bindings ith_expr p)
    |> List.concat
  | PConstraint (p, _) -> pattern_bindings expr p

let rec_flags : Ast.rec_flag -> Me_ast.rec_flag = function
  | Rec -> Rec
  | NoRec -> NoRec
;;

let rec expr_to_mexpr expr =
  match expr with
  | EUnit -> return Me_EUnit
  | ENill -> return Me_ENill
  | EConstraint (e, _) -> expr_to_mexpr e
  | EConst c -> return @@ Me_EConst (const_to_pe_const c)
  | EIdentifier id -> return @@ Me_EIdentifier id
  | EApplication (f, arg) ->
    let* f' = expr_to_mexpr f in
    let* arg' = expr_to_mexpr arg in
    return @@ Me_EApp (f', arg')
  | EFun (pat, body) ->
    let ids = pattern_remove pat in
    let* body' = expr_to_mexpr body in
    return @@ Me_EFun (ids, body')
  | ELetIn (rec_flag, pat, e1, e2) ->
    let ids = pattern_remove pat in
    let* e1' = expr_to_mexpr e1 in
    let* e2' = expr_to_mexpr e2 in
    let rec_flag = rec_flags rec_flag in
    (match ids with
     | [ id ] -> return @@ Me_ELet (rec_flag, id, e1', e2')
     | ids_list ->
       let transformed_e =
         List.mapi ids_list ~f:(fun i id ->
           let get_expr =
             EApplication (EApplication (EIdentifier "tuple_get", e1), EConst (CInt i))
           in
           PIdentifier id, get_expr)
       in
       let final_expr =
         List.fold_right transformed_e ~init:e2 ~f:(fun (pat, expr) acc ->
           ELetIn (NoRec, pat, expr, acc))
       in
       expr_to_mexpr final_expr)
  | ETuple exprs ->
    let* exprs' =
      RList.fold_left exprs ~init:(return []) ~f:(fun acc e ->
        let* e' = expr_to_mexpr e in
        return (acc @ [ e' ]))
    in
    return @@ Me_ETuple exprs'
  | EIf (cond, then_, else_) ->
    let* cond' = expr_to_mexpr cond in
    let* then_' = expr_to_mexpr then_ in
    let* else_' = expr_to_mexpr else_ in
    return @@ Me_EIf (cond', then_', else_')
  | ECons (hd, tl) ->
    let* hd' = expr_to_mexpr hd in
    let* tl' = expr_to_mexpr tl in
    return @@ Me_ECons (hd', tl')
  | EMatch (e, branches) -> desugar_match e branches

and desugar_match e branches =
  let* e' = expr_to_mexpr e in
  match branches with
  | [] -> failwith "Empty match expression"
  | (pat, expr_rhs) :: rest ->
    let* expr_rhs' = expr_to_mexpr expr_rhs in
    let rec pattern_to_condition expr pat =
      match pat with
      | PAny -> return @@ Me_EConst (Me_CBool true)
      | PUnit ->
        return
        @@ Me_EIf
             ( Me_EApp (Me_EIdentifier "is_unit", expr)
             , Me_EConst (Me_CBool true)
             , Me_EConst (Me_CBool false) )
      | PConst c ->
        return
        @@ Me_EApp (Me_EApp (Me_EIdentifier "(=)", expr), Me_EConst (const_to_pe_const c))
      | PIdentifier _ -> return @@ Me_EConst (Me_CBool true)
      | PNill -> return @@ Me_EApp (Me_EIdentifier "is_empty", expr)
      | PCons (hd, tl) ->
        let hd_expr = Me_EApp (Me_EIdentifier "hd_list_get", expr) in
        let tl_expr = Me_EApp (Me_EIdentifier "tl_list_get", expr) in
        let* cond_hd = pattern_to_condition hd_expr hd in
        let* cond_tl = pattern_to_condition tl_expr tl in
        let is_cons_check = Me_EApp (Me_EIdentifier "is_cons", expr) in
        let comb =
          match cond_hd, cond_tl with
          (* Если hd или tl — это PIdentifier или PAny, то if true then ... else false, что избыточно *)
          | Me_EConst (Me_CBool true), Me_EConst (Me_CBool true) -> is_cons_check
          | Me_EConst (Me_CBool true), cond ->
            Me_EIf (is_cons_check, cond, Me_EConst (Me_CBool false))
          | cond, Me_EConst (Me_CBool true) ->
            Me_EIf (is_cons_check, cond, Me_EConst (Me_CBool false))
          | cond1, cond2 ->
            Me_EIf
              ( is_cons_check
              , Me_EIf (cond1, cond2, Me_EConst (Me_CBool false))
              , Me_EConst (Me_CBool false) )
        in
        return comb
      | PTuple pats ->
        let* conds =
          RList.fold_left
            (List.mapi pats ~f:(fun i p -> i, p))
            ~init:(return [])
            ~f:(fun acc (i, p) ->
              let ith_expr =
                Me_EApp
                  (Me_EIdentifier "tuple_get", Me_ETuple [ expr; Me_EConst (Me_Cint i) ])
              in
              let* cond = pattern_to_condition ith_expr p in
              return (acc @ [ cond ]))
        in
        return
        @@ List.fold_right conds ~init:(Me_EConst (Me_CBool true)) ~f:(fun c acc -> 
          Me_EIf (c, acc, Me_EConst (Me_CBool false)))
      | PConstraint (p, _) -> pattern_to_condition expr p
    in
    let* id_num = fresh in
    let tmp_var = get_new_id id_num "match_tmp" in
    let bound_expr, bind_expr_opt =
      match e' with
      | Me_EIdentifier _ -> e', None
      | _ -> Me_EIdentifier tmp_var, Some (tmp_var, e')
    in
    let* cond = pattern_to_condition bound_expr pat in
    let bindings = pattern_bindings bound_expr pat in
    let bound_rhs =
      List.fold_right bindings ~init:expr_rhs' ~f:(fun (id, expr) acc ->
        Me_ELet (NoRec, id, expr, acc))
    in
    let* rest_expr =
      match rest with
      | [] ->
        return @@ Me_EIdentifier "fail"
      | _ ->
        let new_e =
          match bind_expr_opt with
          | None -> e
          | Some (name, _) -> EIdentifier name
        in
        desugar_match new_e rest
    in
    (match bind_expr_opt with
     | None -> return @@ Me_EIf (cond, bound_rhs, rest_expr)
     | Some (var, expr) ->
       return @@ Me_ELet (NoRec, var, expr, Me_EIf (cond, bound_rhs, rest_expr)))
;;

let decl_to_pe_decl decls =
  let process_binding pat expr =
    let ids = pattern_remove pat in
    match ids with
    | [ id ] ->
      let* e' = expr_to_mexpr expr in
      return [ id, e' ]
    | _ ->
      let* tmp_id_num = fresh in
      let tmp_var = get_new_id tmp_id_num "tmp" in
      let* e' = expr_to_mexpr expr in
      let tmp_expr = Me_EIdentifier tmp_var in
      let bindings =
        pattern_bindings tmp_expr pat |> List.map ~f:(fun (id, expr) -> id, expr)
      in
      return ((tmp_var, e') :: bindings)
  in
  match decls with
  | NoRecDecl decls ->
    let* converted =
      RList.fold_left decls ~init:(return []) ~f:(fun acc (DDeclaration (pat, expr)) ->
        let* bindings = process_binding pat expr in
        return (acc @ bindings))
    in
    return @@ Me_Nonrec converted
  | RecDecl decls ->
    let* converted =
      RList.fold_left decls ~init:(return []) ~f:(fun acc (DDeclaration (pat, expr)) ->
        let ids = pattern_remove pat in
        match ids with
        | [ id ] ->
          let* e' = expr_to_mexpr expr in
          return ((id, e') :: acc)
        | _ -> failwith "Simple patterns on rec, otherwise it's crazt")
    in
    return @@ Me_Rec (List.rev converted)
;;

let match_elimination prog =
  StateMonad.run
    (RList.fold_left prog ~init:(return []) ~f:(fun acc decl ->
       let* d = decl_to_pe_decl decl in
       return (acc @ [ d ])))
;;
