(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Llast
open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Common.IdentifierSearcher
open IdentifierSubstitutor

let get_new_ll_name = Common.NameCreator.get_new_name "ll"

let rec ll_expression env replacement_map lifted = function
  | EConstant const -> return @@ (LEConstant const, lifted)
  | EIdentifier id -> ll_identifier replacement_map lifted id
  | EEmptyList -> return (LEEmptyList, lifted)
  | EFun ((p, ps), body) -> ll_fun env replacement_map lifted (p, ps) body
  | ELetIn (case, cases, expr) ->
    ll_let_in env replacement_map lifted (case :: cases, expr)
  | ERecLetIn (case, cases, expr) ->
    ll_let_rec_in env replacement_map lifted (case :: cases, expr)
  | EApplication (func, fst_arg, args) ->
    ll_application env replacement_map lifted (func, fst_arg, args)
  | EIfThenElse (c, b1, b2) -> ll_if_then_else env replacement_map lifted (c, b1, b2)
  | EListConstructor (l, r) -> ll_list_constructor env replacement_map lifted (l, r)
  | ETuple (e1, e2, es) -> ll_tuple env replacement_map lifted (e1, e2, es)
  | EMatchWith (e, c, cs) -> ll_match_with env replacement_map lifted (e, c, cs)
  | ETyped (e, t) -> ll_typed_expr env replacement_map lifted (e, t)
  | EFunction _ -> return @@ (LEIdentifier (Id ""), lifted)

and ll_fun env replacement_map lifted (p, ps) body =
  let* name = get_new_ll_name env in
  let pattern_identifiers = get_pattern_identifiers_from_list (p :: ps) in
  let new_replacement_map = remove_keys_from_map pattern_identifiers replacement_map in
  let* lifted_body, lifted_exprs = ll_expression env new_replacement_map lifted body in
  let new_expr = LEIdentifier (Id name) in
  let lifted_expr = LDOrdinary ((PVar (Id name), p :: ps, lifted_body), []) in
  return @@ (new_expr, lifted_expr :: lifted_exprs)

and ll_let_in env replacement_map lifted (cases, body) =
  let* lifted_cases, lifted1, final_replacement_map =
    List.fold_left
      (fun acc (p, e) ->
         let* acc_cases, acc_lifted, acc_replacement_map = acc in
         match p, e with
         | PVar id, EFun ((fp, fps), fbody) ->
           let* lifted_fbody, lifted_exprs =
             ll_fun env replacement_map acc_lifted (fp, fps) fbody
           in
           let ident =
             match lifted_fbody with
             | LEIdentifier ik -> ik
             | _ -> Id ""
           in
           let updated_replacement_map = IdentifierMap.add id ident acc_replacement_map in
           return (acc_cases, lifted_exprs, updated_replacement_map)
         | _ ->
           let* lifted_e, lifted_next = ll_expression env replacement_map acc_lifted e in
           return ((p, lifted_e) :: acc_cases, lifted_next, acc_replacement_map))
      (return ([], lifted, replacement_map))
      cases
  in
  let* lifted_body, lifted2 = ll_expression env final_replacement_map lifted1 body in
  match lifted_cases with
  | hd :: tl -> return @@ (LELetIn (hd, tl, lifted_body), lifted2)
  | _ -> return @@ (lifted_body, lifted2)

and ll_let_rec_in env replacement_map lifted (cases, body) =
  let* lifted1, final_replacement_map =
    List.fold_left
      (fun acc (p, e) ->
         let* acc_lifted, acc_replacement_map = acc in
         match p, e with
         | PVar id, EFun ((fp, fps), fbody) ->
           let* lifted_fbody, lifted_exprs =
             ll_fun env replacement_map acc_lifted (fp, fps) fbody
           in
           let ident =
             match lifted_fbody with
             | LEIdentifier ik -> ik
             | _ -> Id ""
           in
           let updated_replacement_map = IdentifierMap.add id ident acc_replacement_map in
           return (lifted_exprs, updated_replacement_map)
         | PVar name, _ ->
           let* lifted_e, lifted_next = ll_expression env replacement_map acc_lifted e in
           let* new_name = get_new_ll_name env in
           let acc_replacement_map = IdentifierMap.add name (Id new_name) acc_replacement_map in
           return
             (LDOrdinary ((PVar (Id new_name), [], lifted_e), []) :: lifted_next, acc_replacement_map)
         | _ -> return (acc_lifted, acc_replacement_map))
      (return ([], replacement_map))
      cases
  in
  let all_cases, all_subcases =
    List.fold_left
      (fun (acc_cases, acc_subcases) decl ->
         match decl with
         | LDOrdinary (main_case, subcases) ->
           let substitute_case (p, ps, expr) =
             p, ps, substitute_identifiers_ll final_replacement_map expr
           in
           let new_main_case = substitute_case main_case in
           let new_subcases = List.map substitute_case subcases in
           new_main_case :: acc_cases, new_subcases @ acc_subcases
         | _ -> acc_cases, acc_subcases)
      ([], [])
      lifted1
  in
  let* rec_decl =
    match all_cases with
    | [] -> return lifted1
    | main_case :: rest_cases ->
      return [ LDRecursive (main_case, rest_cases @ all_subcases) ]
  in
  let lifted1 = rec_decl @ lifted in
  let* lifted_body, lifted2 = ll_expression env final_replacement_map lifted1 body in
  return @@ (lifted_body, lifted2)

and ll_identifier replacement_map lifted id =
  let new_id =
    match IdentifierMap.find_opt id replacement_map with
    | Some name -> LEIdentifier name
    | None -> LEIdentifier id
  in
  return @@ (new_id, lifted)

and ll_application env replacement_map lifted (func, fst_arg, args) =
  let* lifted_func, lifted1 = ll_expression env replacement_map lifted func in
  let* lifted_fst_arg, lifted2 = ll_expression env replacement_map lifted1 fst_arg in
  let* lifted_args, lifted3 =
    List.fold_right
      (fun arg acc ->
         let* acc_exprs, acc_lifted = acc in
         let* lifted_arg, lifted_next = ll_expression env replacement_map acc_lifted arg in
         return (lifted_arg :: acc_exprs, lifted_next))
      args
      (return ([], lifted2))
  in
  let ll_appl = LEApplication (lifted_func, lifted_fst_arg, lifted_args) in
  return @@ (ll_appl, lifted3)

and ll_if_then_else env replacement_map lifted (c, b1, b2) =
  let* l_c, lifted1 = ll_expression env replacement_map lifted c in
  let* l_b1, lifted2 = ll_expression env replacement_map lifted1 b1 in
  let* l_b2, lifted3 =
    match b2 with
    | Some expr ->
      let* l_b2, lifted3 = ll_expression env replacement_map lifted2 expr in
      return @@ (Some l_b2, lifted3)
    | None -> return (None, lifted2)
  in
  return @@ (LEIfThenElse (l_c, l_b1, l_b2), lifted3)

and ll_list_constructor env replacement_map lifted (hd, tl) =
  let* l_hd, lifted1 = ll_expression env replacement_map lifted hd in
  let* l_tl, lifted2 = ll_expression env replacement_map lifted1 tl in
  return @@ (LEListConstructor (l_hd, l_tl), lifted2)

and ll_tuple env replacement_map lifted (e1, e2, es) =
  let* l_e1, lifted1 = ll_expression env replacement_map lifted e1 in
  let* l_e2, lifted2 = ll_expression env replacement_map lifted1 e2 in
  let* l_es, lifted3 =
    List.fold_left
      (fun acc e ->
         let* acc_es, acc_lifted = acc in
         let* l_e, new_acc_lifted = ll_expression env replacement_map acc_lifted e in
         return (l_e :: acc_es, new_acc_lifted))
      (return @@ ([], lifted2))
      es
  in
  let l_es = List.rev l_es in
  return @@ (LETuple (l_e1, l_e2, l_es), lifted3)

and ll_match_with env replacement_map lifted (expr, (p, e), cases) =
  let* l_expr, lifted1 = ll_expression env replacement_map lifted expr in
  let* l_case, lifted2 =
    let* l_e, lifted2 = ll_expression env replacement_map lifted1 e in
    return ((p, l_e), lifted2)
  in
  let* lifted_cases, lifted3 =
    List.fold_right
      (fun (p, e) acc ->
         let* acc_cases, acc_lifted = acc in
         let* l_e, lifted_next = ll_expression env replacement_map acc_lifted e in
         return ((p, l_e) :: acc_cases, lifted_next))
      cases
      (return ([], lifted2))
  in
  return @@ (LEMatchWith (l_expr, l_case, lifted_cases), lifted3)

and ll_typed_expr env replacement_map lifted (e, t) =
  let* l_e, lifted1 = ll_expression env replacement_map lifted e in
  return @@ (LETyped (l_e, t), lifted1)
;;
