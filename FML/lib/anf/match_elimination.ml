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

let hd_list_get expr = Me_EApp (Me_EIdentifier "hd_list_get", expr)
let tl_list_get expr = Me_EApp (Me_EIdentifier "tl_list_get", expr)

let tuple_get expr i =
  Me_EApp (Me_EApp (Me_EIdentifier "tuple_get", expr), Me_EConst (Me_Cint i))
;;

let is_empty expr = Me_EApp (Me_EIdentifier "is_empty", expr)
let is_cons expr = Me_EApp (Me_EIdentifier "is_cons", expr)

let is_equal e c =
  Me_EApp (Me_EApp (Me_EIdentifier "( = )", e), Me_EConst (const_to_pe_const c))
;;

let rec pattern_remove = function
  | PUnit -> [ "()" ]
  | PAny -> []
  | PConst _ -> []
  | PIdentifier id -> [ id ]
  | PNill -> [ "[]" ]
  | PTuple pats -> List.concat_map ~f:pattern_remove pats
  | PCons (hd, tl) -> pattern_remove hd @ pattern_remove tl
  | PConstraint (p, _) -> pattern_remove p
;;

let rec pattern_bindings expr = function
  | PIdentifier id -> [ id, expr ]
  | PAny | PConst _ | PUnit | PNill -> []
  | PCons (hd, tl) ->
    pattern_bindings (hd_list_get expr) hd @ pattern_bindings (tl_list_get expr) tl
  | PTuple ps -> List.concat_mapi ps ~f:(fun i p -> pattern_bindings (tuple_get expr i) p)
  | PConstraint (p, _) -> pattern_bindings expr p
;;

let rec pattern_checks expr = function
  | PNill -> [ is_empty expr ]
  | PCons (h, tl) ->
    [ is_cons expr ]
    @ pattern_checks (hd_list_get expr) h
    @ pattern_checks (tl_list_get expr) tl
  | PConstraint (p, _) -> pattern_checks expr p
  | PConst c -> [ is_equal expr c ]
  | PUnit | PIdentifier _ | PAny -> []
  | PTuple ps -> List.concat_mapi ps ~f:(fun i p -> pattern_checks (tuple_get expr i) p)
;;

let rec_flags : Ast.rec_flag -> Me_ast.rec_flag = function
  | Rec -> Rec
  | NoRec -> NoRec
;;

let rec expr_to_mexpr = function
  | EUnit -> return Me_EUnit
  | ENill -> return Me_ENill
  | EConstraint (e, _) -> expr_to_mexpr e
  | EConst c -> return @@ Me_EConst (const_to_pe_const c)
  | EIdentifier id -> return @@ Me_EIdentifier id
  | EApplication (f, arg) ->
    let* f' = expr_to_mexpr f in
    let* arg' = expr_to_mexpr arg in
    return @@ Me_EApp (f', arg')
  | EFun _ as e ->
    let rev_pats, expr =
      let rec helper acc = function
        | EFun (pat, body) -> helper (pat :: acc) body
        | expr -> acc, expr
      in
      helper [] e
    in
    let* expr = expr_to_mexpr expr in
    let* args, checks, expr =
      RList.fold_left
        rev_pats
        ~init:(return ([], [], expr))
        ~f:(fun (args, checks, expr) pat ->
          let rec remove_annotations = function
            | PConstraint (p, _) -> remove_annotations p
            | p -> p
          in
          let pat = remove_annotations pat in
          match pat with
          | PTuple _ | PCons _ ->
            let* id_num = fresh in
            let arg_id = get_new_id id_num "me" in
            let bindings = pattern_bindings (Me_EIdentifier arg_id) pat in
            let expr =
              List.fold_right bindings ~init:expr ~f:(fun (name, e) expr ->
                Me_ELet (NoRec, name, e, expr))
            in
            let new_checks = pattern_checks (Me_EIdentifier arg_id) pat in
            return (arg_id :: args, new_checks @ checks, expr)
          | PIdentifier id -> return (id :: args, checks, expr)
          | PNill | PConst _ ->
            let* id_num = fresh in
            let arg_id = get_new_id id_num "me" in
            return
              (arg_id :: args, pattern_checks (Me_EIdentifier arg_id) pat @ checks, expr)
          | PAny -> return ("_" :: args, checks, expr)
          | PUnit -> return ("()" :: args, checks, expr)
          | PConstraint _ -> return (args, checks, expr))
    in
    (match checks with
     | [] -> return @@ Me_EFun (args, expr)
     | h :: tl ->
       let check =
         List.fold tl ~init:h ~f:(fun acc c ->
           Me_EApp (Me_EApp (Me_EIdentifier "( && )", acc), c))
       in
       return @@ Me_EFun (args, Me_EIf (check, expr, Me_EIdentifier "fail")))
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

and desugar_match expr branches =
  let* expr' = expr_to_mexpr expr in
  List.fold_right branches ~init:(return @@ Me_EIdentifier "fail") ~f:(fun (p, e) acc ->
    let check =
      match pattern_checks expr' p with
      | [] -> Me_EConst (Me_CBool true)
      | h :: tl ->
        List.fold_right tl ~init:h ~f:(fun c acc ->
          Me_EApp (Me_EApp (Me_EIdentifier "( && )", acc), c))
    in
    let bindings = pattern_bindings expr' p in
    let* e' = expr_to_mexpr e in
    let expr =
      List.fold_right bindings ~init:e' ~f:(fun (name, e) acc ->
        Me_ELet (NoRec, name, e, acc))
    in
    let* acc = acc in
    return @@ Me_EIf (check, expr, acc))
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
      let bindings = pattern_bindings tmp_expr pat in
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
