(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Common
open StateMonad

let get_new_id n name = String.concat [ name; "_ac"; Int.to_string n ]

let rec ac_pattern env bindings = function
  | PIdentifier name as pat ->
    if StrSet.find env name
    then
      let* fr = fresh in
      let id = get_new_id fr name in
      return
        (StrSet.add env id, StrMap.update bindings name ~f:(fun _ -> id), PIdentifier id)
    else return (StrSet.add env name, bindings, pat)
  | PConstraint (pat, _) -> ac_pattern env bindings pat
  | PCons (hd, tl) ->
    let* env', bindings', hd_pat = ac_pattern env bindings hd in
    let* env'', bindings'', tl_pat = ac_pattern env' bindings' tl in
    return (env'', bindings'', PCons (hd_pat, tl_pat))
  | PTuple ps ->
    let* env, bindings, ps =
      RList.fold_left
        ps
        ~init:(return (env, bindings, []))
        ~f:(fun (e, b, ps) p ->
          let* env, bindings, pat = ac_pattern e b p in
          return (env, bindings, pat :: ps))
    in
    return (env, bindings, PTuple (List.rev ps))
  | pat -> return (env, bindings, pat)
;;

let rec ac_expr env bindings = function
  | EConst c -> return @@ EConst c
  | ENill -> return ENill
  | EUnit -> return EUnit
  | EConstraint (e, t) ->
    let* expr = ac_expr env bindings e in
    return @@ EConstraint (expr, t)
  | EIdentifier id as expr ->
    (match StrMap.find bindings id with
     | Some new_id -> return @@ EIdentifier new_id
     | None -> return expr)
  | ELetIn (NoRec, pat, e1, e2) ->
    let* e1' = ac_expr env bindings e1 in
    let* env', bindings', pat' = ac_pattern env bindings pat in
    let* e2' = ac_expr env' bindings' e2 in
    return @@ ELetIn (NoRec, pat', e1', e2')
  | ELetIn (Rec, pat, e1, e2) ->
    let* env', bindings', pat' = ac_pattern env bindings pat in
    let* e1' = ac_expr env' bindings' e1 in
    let* e2' = ac_expr env' bindings' e2 in
    return @@ ELetIn (Rec, pat', e1', e2')
  | EApplication (e1, e2) ->
    let* e1' = ac_expr env bindings e1 in
    let* e2' = ac_expr env bindings e2 in
    return (EApplication (e1', e2'))
  | ECons (e1, e2) ->
    let* e1' = ac_expr env bindings e1 in
    let* e2' = ac_expr env bindings e2 in
    return (ECons (e1', e2'))
  | EFun (pat, expr) ->
    let* env', bindings', pat' = ac_pattern env bindings pat in
    let* expr' = ac_expr env' bindings' expr in
    return @@ EFun (pat', expr')
  | EIf (e_if, e_then, e_else) ->
    let* e_if' = ac_expr env bindings e_if in
    let* e_then' = ac_expr env bindings e_then in
    let* e_else' = ac_expr env bindings e_else in
    return @@ EIf (e_if', e_then', e_else')
  | ETuple exprs ->
    let* l =
      RList.fold_left exprs ~init:(return []) ~f:(fun acc expr ->
        let* e = ac_expr env bindings expr in
        return @@ (e :: acc))
    in
    return @@ ETuple (List.rev l)
  | EMatch (expr, cases) ->
    let* expr' = ac_expr env bindings expr in
    let* cases' =
      RList.fold_left cases ~init:(return []) ~f:(fun acc (pat, expr) ->
        let* env', bindings', pat' = ac_pattern env bindings pat in
        let* expr' = ac_expr env' bindings' expr in
        return ((pat', expr') :: acc))
    in
    return @@ EMatch (expr', List.rev cases')
;;

let ac_decls env bindings = function
  | NoRecDecl decls ->
    let* env, bindings, decls =
      RList.fold_left
        decls
        ~init:(return (env, bindings, []))
        ~f:(fun (new_env, new_bindings, decl_list) (DDeclaration (pat, expr)) ->
          let* env', bindings', pat' = ac_pattern new_env new_bindings pat in
          let* expr' = ac_expr env bindings expr in
          return (env', bindings', DDeclaration (pat', expr') :: decl_list))
    in
    return (env, bindings, NoRecDecl (List.rev decls))
  | RecDecl decls ->
    let* env', bindings', decls' =
      RList.fold_left
        decls
        ~init:(return (env, bindings, []))
        ~f:(fun (new_env, new_bindings, acc_decls) (DDeclaration (pat, expr)) ->
          let* env', bindings', pat' = ac_pattern new_env new_bindings pat in
          return (env', bindings', DDeclaration (pat', expr) :: acc_decls))
    in
    let* env, bindings, decls =
      RList.fold_left
        decls'
        ~init:(return (env', bindings', []))
        ~f:(fun (env, bindings, acc_decls) (DDeclaration (pat, expr)) ->
          let* expr' = ac_expr env bindings expr in
          return (env', bindings', DDeclaration (pat, expr') :: acc_decls))
    in
    return (env, bindings, RecDecl decls)
;;

let ac_program ast =
  run
    (let* _, _, decls =
       RList.fold_left
         ast
         ~init:(return (StrSet.of_list builtins, StrMap.empty, []))
         ~f:(fun (env, bindings, decls) decl ->
           let* new_env, new_bindings, new_decl = ac_decls env bindings decl in
           return (new_env, new_bindings, new_decl :: decls))
     in
     return @@ List.rev decls)
;;
