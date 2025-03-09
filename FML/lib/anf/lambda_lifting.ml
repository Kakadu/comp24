(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Pe_ast
open Common
open Common.MonadCounter

let rec ll_expr env = function
  | Pe_EUnit -> return ([], Pe_EUnit)
  | Pe_ENill -> return ([], Pe_ENill)
  | Pe_EConst _ as v -> return ([], v)
  | Pe_EIdentifier id as v ->
    (match Map.find env id with
     | Some x -> return ([], Pe_EIdentifier x)
     | None -> return ([], v))
  | Pe_EApp (e1, e2) ->
    let* str1, e1 = ll_expr env e1 in
    let* sl2, e2 = ll_expr env e2 in
    return (str1 @ sl2, Pe_EApp (e1, e2))
  | Pe_EIf (e1, e2, e3) ->
    let* str1, e1 = ll_expr env e1 in
    let* str2, e2 = ll_expr env e2 in
    let* str3, e3 = ll_expr env e3 in
    return (str1 @ str2 @ str3, Pe_EIf (e1, e2, e3))
  | Pe_EFun (args, body) ->
    let* fresh = fresh >>| get_id in
    let new_env = List.fold args ~init:env ~f:Map.remove in
    let* _, body = ll_expr new_env body in
    return ([ Pe_Nonrec [ fresh, Pe_EFun (args, body) ] ], Pe_EIdentifier fresh)
  | Pe_ECons (e1, e2) ->
    let* str1, e1 = ll_expr env e1 in
    let* str2, e2 = ll_expr env e2 in
    return (str1 @ str2, Pe_ECons (e1, e2))
  | Pe_ETuple e_list ->
    let* t = map e_list ~f:(ll_expr env) in
    let str, el = List.unzip t in
    return (List.concat str, Pe_ETuple el)
  | Pe_ELet (Rec, name, e1, e2) ->
    let* fresh_name = fresh >>| get_id in
    let env = Map.set env ~key:name ~data:fresh_name in
    let* str1, e1 = ll_inner env e1 in
    let* str2, e2 = ll_expr env e2 in
    return (str1 @ [ Pe_Rec [ fresh_name, e1 ] ] @ str2, e2)
  | Pe_ELet (NoRec, name, e1, e2) ->
    let* str1, e1 = ll_inner env e1 in
    (match e1 with
     | Pe_EFun _ ->
       let* fresh_name = fresh >>| get_id in
       let bindings = Map.set env ~key:name ~data:fresh_name in
       let* str2, e2 = ll_expr bindings e2 in
       return (str1 @ [ Pe_Nonrec [ fresh_name, e1 ] ] @ str2, e2)
     | _ ->
       let* str2, e2 = ll_expr env e2 in
       return (str1 @ str2, Pe_ELet (NoRec, name, e1, e2)))

and ll_inner env = function
  | Pe_EFun (args, body) ->
    let env = List.fold args ~init:env ~f:Map.remove in
    let* str, body = ll_expr env body in
    return (str, Pe_EFun (args, body))
  | e ->
    let* str, e = ll_expr env e in
    return (str, e)
;;

let ll_str_item = function
  | Pe_Nonrec bindings ->
    let* lifted_bindings =
      map bindings ~f:(fun (name, e) ->
        let* str, new_e = ll_inner empty e in
        return (str, (name, new_e)))
    in
    let strs, new_bindings = List.unzip lifted_bindings in
    return (List.concat strs @ [ Pe_Nonrec new_bindings ])
  | Pe_Rec bindings ->
    let* lifted_bindings =
      map bindings ~f:(fun (name, e) ->
        let* str, new_e = ll_inner empty e in
        return (str, (name, new_e)))
    in
    let strs, new_bindings = List.unzip lifted_bindings in
    return (List.concat strs @ [ Pe_Rec new_bindings ])
;;

let ll_structure structure =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* str1 = ll_str_item hd in
      let* str2 = helper tl in
      return @@ str1 @ str2
  in
  helper structure
;;

let run_ll bindings init_num p = run (ll_structure p) bindings init_num
