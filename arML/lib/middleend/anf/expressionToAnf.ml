(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Ast.AbstractSyntaxTree
open PatternMatchingElim.Pmfast
open Anftree

let get_new_anf_name = Common.NameCreator.get_new_name "i"

let rec expression_to_complex env = function
  | PMFConstant c -> return @@ (CAtom (IConstant c), [], env)
  | PMFIdentifier i -> return @@ (CAtom (IIdentifier i), [], env)
  | PMFEmptyList -> return @@ (CAtom IEmptyList, [], env)
  | PMFLetIn ((name, expr1), expr2) ->
    let extended_env = IdentifierSet.add name env in
    let* expr1', lets_in1, env1 = expression_to_complex extended_env expr1 in
    let* expr2', lets_in2, env2 = expression_to_complex env1 expr2 in
    let lets_in3 = [ name, expr1' ] in
    let final_lets_in = List.concat [ lets_in1; lets_in3; lets_in2 ] in
    return @@ (expr2', final_lets_in, env2)
  | PMFApplication (func, arg, args) ->
    let* args, lets_in, env =
      List.fold_right
        (fun arg acc ->
          let* arg_acc, lets_in_acc, env_acc = acc in
          match arg with
          | PMFConstant c -> return @@ (IConstant c :: arg_acc, lets_in_acc, env_acc)
          | PMFIdentifier v -> return @@ (IIdentifier v :: arg_acc, lets_in_acc, env_acc)
          | _ ->
            let* new_name = get_new_anf_name env_acc in
            let new_name = Id new_name in
            let env_acc = IdentifierSet.add new_name env_acc in
            let* arg', lets_in', env' = expression_to_complex env_acc arg in
            let new_lets_in = [ new_name, arg' ] in
            let final_lets_in = List.concat [ lets_in'; new_lets_in; lets_in_acc ] in
            let new_atom = IIdentifier new_name in
            return (new_atom :: arg_acc, final_lets_in, env'))
        (func :: arg :: args)
        (return ([], [], env))
    in
    (match args with
     | hd :: md :: tl -> return @@ (CApplication (hd, md, tl), lets_in, env)
     | _ -> assert false)
  | PMFIfThenElse (c, b1, b2) ->
    let* b1', env = expression_to_anf env b1 in
    let* b2', env =
      match b2 with
      | Some b2 -> expression_to_anf env b2
      | None -> return @@ (AComplex (CAtom (IConstant CUnit)), env)
    in
    let* c', lets_in, env =
      match c with
      | PMFConstant c -> return (IConstant c, [], env)
      | PMFIdentifier v -> return (IIdentifier v, [], env)
      | _ ->
        let* new_name = get_new_anf_name env in
        let new_name = Id new_name in
        let env = IdentifierSet.add new_name env in
        let* c', lets_in, env = expression_to_complex env c in
        let new_lets_in = [ new_name, c' ] in
        let final_lets_in = List.concat [ lets_in; new_lets_in ] in
        return @@ (IIdentifier new_name, final_lets_in, env)
    in
    return @@ (CIfThenElse (c', b1', b2'), lets_in, env)
  | PMFListConstructor (l, r) ->
    let convert_to_immut c env =
      match c with
      | PMFConstant c -> return (IConstant c, [], env)
      | PMFIdentifier v -> return (IIdentifier v, [], env)
      | _ ->
        let* new_name = get_new_anf_name env in
        let new_name = Id new_name in
        let env = IdentifierSet.add new_name env in
        let* c', lets_in, env = expression_to_complex env c in
        let new_lets_in = [ new_name, c' ] in
        let final_lets_in = List.concat [ lets_in; new_lets_in ] in
        return @@ (IIdentifier new_name, final_lets_in, env)
    in
    let* l', lets_in1, env = convert_to_immut l env in
    let* r', lets_in2, env = convert_to_immut r env in
    let final_lets_in = List.concat [ lets_in1; lets_in2 ] in
    return @@ (CListConstructor (l', r'), final_lets_in, env)
  | PMFTuple (e1, e2, es) ->
    let convert_to_immut c env =
      match c with
      | PMFConstant c -> return (IConstant c, [], env)
      | PMFIdentifier v -> return (IIdentifier v, [], env)
      | _ ->
        let* new_name = get_new_anf_name env in
        let new_name = Id new_name in
        let env = IdentifierSet.add new_name env in
        let* c', lets_in, env = expression_to_complex env c in
        let new_lets_in = [ new_name, c' ] in
        let final_lets_in = List.concat [ lets_in; new_lets_in ] in
        return @@ (IIdentifier new_name, final_lets_in, env)
    in
    let* e1', lets_in1, env = convert_to_immut e1 env in
    let* e2', lets_in2, env = convert_to_immut e2 env in
    let* e3', lets_in3, env =
      List.fold_right
        (fun e acc ->
          let* acc_e, acc_lets_in, env = acc in
          let* e', lets_in', env = convert_to_immut e env in
          return @@ (e' :: acc_e, lets_in' :: acc_lets_in, env))
        es
        (return ([], [], env))
    in
    let lets_in3 = List.concat lets_in3 in
    let final_lets_in = List.concat [ lets_in1; lets_in2; lets_in3 ] in
    return @@ (CAtom (ITuple (e1', e2', e3')), final_lets_in, env)
  | PMFTyped (e, t) ->
    let* e', lets_in, env = expression_to_complex env e in
    return @@ (CTyped (e', t), lets_in, env)

and expression_to_anf env expr =
  let* expr', lets_in, env' = expression_to_complex env expr in
  let* expr'' =
    List.fold_right
      (fun case acc ->
        let* acc = acc in
        let name, expr = case in
        return @@ ALetIn (name, expr, acc))
      lets_in
      (return (AComplex expr'))
  in
  return (expr'', env')
;;
