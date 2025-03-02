(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open StateMonad
open StateMonad.Syntax
open IdentifierStructs
open IdentifierSearcher
open IdentifierSubstitutor

let rec transform_fun = function
  | EFun ((p1, ps1), EFun ((p2, ps2), body)) ->
    transform_fun (EFun ((p1, ps1 @ (p2 :: ps2)), body))
  | e -> e
;;

let get_new_arg_name = NameCreator.get_new_name "cc"

let transform_let_in env = function
  | ELetIn (case, cases, expr) ->
    let main_patterns_identifiers = get_pattern_identifiers_from_cases (case :: cases) in
    let patterns_to_update = IdentifierSet.empty in
    let patterns_to_update =
      List.fold_left
        (fun acc (_, case_expr) ->
           let ce_free_vars = get_expr_free_vars case_expr in
           let inner_expr_free_vars = get_expr_free_vars expr in
           let predicate1 v = IdentifierSet.mem v main_patterns_identifiers in
           let predicate2 v = IdentifierSet.mem v inner_expr_free_vars in
           IdentifierSet.fold
             (fun v acc ->
                if predicate1 v && predicate2 v then IdentifierSet.add v acc else acc)
             ce_free_vars
             acc)
        patterns_to_update
        (case :: cases)
    in
    let* updated_cases, replacement_map =
      List.fold_left
        (fun acc (p, expr) ->
           let* acc_cases, acc_replacement_map = acc in
           let update_pattern pattern =
             let rec aux pattern acc_replacement_map =
               match pattern with
               | PVar (Id name) when IdentifierSet.mem (Id name) patterns_to_update ->
                 let* new_name = get_new_arg_name env in
                 return
                   ( PVar (Id new_name)
                   , IdentifierMap.add (Id name) (Id new_name) acc_replacement_map )
               | PTuple (p1, p2, ps) ->
                 let* new_p1, map1 = aux p1 acc_replacement_map in
                 let* new_p2, map2 = aux p2 map1 in
                 let* new_ps, final_map =
                   List.fold_left
                     (fun acc p ->
                        let* acc_ps, acc_map = acc in
                        let* new_p, new_map = aux p acc_map in
                        return
                          ( new_p :: acc_ps
                          , IdentifierMap.union (fun _ v _ -> Some v) acc_map new_map ))
                     (return ([], map2))
                     ps
                 in
                 return (PTuple (new_p1, new_p2, List.rev new_ps), final_map)
               | PListConstructor (p1, p2) ->
                 let* new_p1, map1 = aux p1 acc_replacement_map in
                 let* new_p2, final_map = aux p2 map1 in
                 return (PListConstructor (new_p1, new_p2), final_map)
               | _ -> return (pattern, acc_replacement_map)
             in
             aux pattern acc_replacement_map
           in
           let* new_pattern, updated_map = update_pattern p in
           return ((new_pattern, expr) :: acc_cases, updated_map))
        (return ([], IdentifierMap.empty))
        (case :: cases)
    in
    let updated_cases = List.rev updated_cases in
    let updated_expr = substitute_identifiers replacement_map expr in
    return @@ ELetIn (List.hd updated_cases, List.tl updated_cases, updated_expr)
  | expr -> return expr
;;
