(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Pattern_remover
open Reduced_ast
open Middleend_utils

open
  Utils.Counter_monad.Make
    (Int)
    (struct
      type t = error
    end)

open Base

let rec ll_expr = function
  | RExp_ident s -> return ([], RExp_ident s)
  | RExp_constant c -> return ([], RExp_constant c)
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
  | RExp_fun (_, e) ->
    let+ d, e' = ll_expr e
    and+ f = fresh >>| fun i -> "lifted_" ^ Int.to_string i in
    d @ [ RStr_value (f, e') ], RExp_ident f
  | RExp_tuple el ->
    let+ el' = map ll_expr el in
    let ds, es = List.unzip el' in
    List.concat ds, RExp_tuple es
  | RExp_construct (s, e) ->
    (match e with
     | Some e ->
       let+ d, e' = ll_expr e in
       d, RExp_construct (s, Some e')
     | None -> return ([], RExp_construct (s, None)))
  | RExp_let (s, e1, e2) ->
    let+ d1, e1' = ll_expr e1
    and+ d2, e2' = ll_expr e2 in
    d1 @ d2 @ [ RStr_value (s, e1') ], e2'
  | RExp_let_rec (l, e) ->
    let names, el = List.unzip l in
    let+ el' = map ll_expr el
    and+ d, e' = ll_expr e in
    let ds, es = List.unzip el' in
    let dl = List.map2_exn names es ~f:(fun name e -> name, e) in
    List.concat ds @ d @ [ RStr_value_rec dl ], e'
;;

let ll_struct_item = function
  | RStr_eval e ->
    let+ d, e' = ll_expr e in
    d @ [ RStr_eval e' ]
  | RStr_value (s, e) ->
    let+ d, e' = ll_expr e in
    d @ [ RStr_value (s, e') ]
  | RStr_value_rec l ->
    let names, el = List.unzip l in
    let+ el' = map ll_expr el in
    let ds, es = List.unzip el' in
    let dl = List.map2_exn names es ~f:(fun name e -> name, e) in
    List.concat ds @ [ RStr_value_rec dl ]
;;

let ll_struct structure =
  List.fold structure ~init:(return []) ~f:(fun acc hd ->
    let+ decls1 = ll_struct_item hd
    and+ decls2 = acc in
    decls1 @ decls2)
;;

(*Must be used after alpha_conversion and pattern removing*)
let ll structure =
  match run (ll_struct structure) 0 with
  | Result.Ok s -> s
  | Result.Error s -> [ RStr_eval (fun_exception @@ exp_to_string s) ]
;;
