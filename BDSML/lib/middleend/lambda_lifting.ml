(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Reduced_ast
open Middleend_utils
open Monads
module ApplyMover = Map.Make (String)

let rec ll_expr =
  let lift_name = fresh >>| fun i -> "lifted_" ^ Int.to_string i in
  function
  | RExp_ident _ as s -> return ([], s)
  | RExp_constant _ as s -> return ([], s)
  | RExp_apply (e1, e2) ->
    let+ d1, e1 = ll_expr e1
    and+ d2, e2 = ll_expr e2 in
    d1 @ d2, RExp_apply (e1, e2)
  | RExp_if (e1, e2, e3) ->
    let* d1, e1 = ll_expr e1 in
    let* d2, e2 = ll_expr e2 in
    (match e3 with
     | Some e3 ->
       let+ d3, e3 = ll_expr e3 in
       d1 @ d2 @ d3, RExp_if (e1, e2, Some e3)
     | None -> return (d1 @ d2, RExp_if (e1, e2, None)))
  | RExp_tuple el ->
    let+ el' = map ll_expr el in
    let ds, es = List.split el' in
    List.concat ds, RExp_tuple es
  | RExp_construct (s, e) ->
    (match e with
     | Some e ->
       let+ d, e' = ll_expr e in
       d, RExp_construct (s, Some e')
     | None -> return ([], RExp_construct (s, None)))
  | RExp_fun (sl, e) ->
    let* f = lift_name in
    let+ d, e' = ll_expr e in
    d @ [ RStr_value (f, RExp_fun (sl, e')) ], RExp_ident f
  | RExp_let (s, e1, e2) ->
    let* d1, e1' = ll_expr e1 in
    (match e1' with
     | RExp_fun _ ->
       let* ns = lift_name in
       let+ d2, e2' = ll_expr e2 in
       d1 @ [ RStr_value (ns, e1') ] @ d2, e2'
     | _ ->
       let+ d2, e2' = ll_expr e2 in
       d1 @ d2, RExp_let (s, e1', e2'))
  | RExp_let_rec (l, e) ->
    let names, el = List.split l in
    let* el' =
      map
        (function
          | RExp_fun (args, exp) ->
            let+ rstr, exp = ll_expr exp in
            rstr, RExp_fun (args, exp)
          | _ as e -> ll_expr e)
        el
    in
    let+ d, e' = ll_expr e in
    let ds, es = List.split el' in
    List.concat ds @ d @ [ RStr_value_rec (List.combine names es) ], e'
;;

let ll_struct_item = function
  | RStr_eval e ->
    let+ d, e' = ll_expr e in
    d @ [ RStr_eval e' ]
  | RStr_value (s, RExp_fun (args, e)) ->
    let+ d, e' = ll_expr e in
    d @ [ RStr_value (s, RExp_fun (args, e')) ]
  | RStr_value (s, e) ->
    let+ d, e' = ll_expr e in
    d @ [ RStr_value (s, e') ]
  | RStr_value_rec l ->
    let names, el = List.split l in
    let+ el' =
      map
        (function
          | RExp_fun (args, exp) ->
            let+ rstrs, exp = ll_expr exp in
            rstrs, RExp_fun (args, exp)
          | _ as exp -> ll_expr exp)
        el
    in
    let ds, es = List.split el' in
    let dl = List.map2 (fun name e -> name, e) names es in
    List.concat ds @ [ RStr_value_rec dl ]
;;

let ll_struct structure =
  List.fold_left
    (fun acc hd ->
      let+ decls1 = ll_struct_item hd
      and+ decls2 = acc in
      decls2 @ decls1)
    (return [])
    structure
;;

(*Must be used after alpha_conversion, pattern removing and closure_conversion*)
let ll structure = run (ll_struct structure) 0
