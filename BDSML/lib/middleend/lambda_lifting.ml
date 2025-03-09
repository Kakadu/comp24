(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Reduced_ast
open Middleend_utils
open Monads
open Base

let rec ll_expr bindings =
  let lift_name = fresh >>| fun i -> "lifted_" ^ Int.to_string i in
  function
  | RExp_ident s ->
    (match Map.find bindings s with
     | Some ns -> return ([], RExp_ident ns)
     | None -> return ([], RExp_ident s))
  | RExp_constant c -> return ([], RExp_constant c)
  | RExp_apply (e1, e2) ->
    let+ d1, e1 = ll_expr bindings e1
    and+ d2, e2 = ll_expr bindings e2 in
    d1 @ d2, RExp_apply (e1, e2)
  | RExp_if (e1, e2, e3) ->
    let* d1, e1 = ll_expr bindings e1 in
    let* d2, e2 = ll_expr bindings e2 in
    (match e3 with
     | Some e3 ->
       let+ d3, e3 = ll_expr bindings e3 in
       d1 @ d2 @ d3, RExp_if (e1, e2, Some e3)
     | None -> return (d1 @ d2, RExp_if (e1, e2, None)))
  | RExp_tuple el ->
    let+ el' = map (ll_expr bindings) el in
    let ds, es = List.unzip el' in
    List.concat ds, RExp_tuple es
  | RExp_construct (s, e) ->
    (match e with
     | Some e ->
       let+ d, e' = ll_expr bindings e in
       d, RExp_construct (s, Some e')
     | None -> return ([], RExp_construct (s, None)))
  | RExp_fun (sl, e) ->
    let* f = lift_name in
    let bindings = List.fold sl ~init:bindings ~f:Map.remove in
    let+ d, e' = ll_expr bindings e in
    d @ [ RStr_value (f, RExp_fun (sl, e')) ], RExp_ident f
  | RExp_let (s, e1, e2) ->
    let* d1, e1' = ll_expr bindings e1 in
    (match e1' with
     | RExp_fun _ ->
       let* ns = lift_name in
       let bindings' = Map.update bindings s ~f:(fun _ -> ns) in
       let* d2, e2' = ll_expr bindings' e2 in
       return (d1 @ [ RStr_value (ns, e1') ] @ d2, e2')
     | _ ->
       let+ d2, e2' = ll_expr bindings e2 in
       d1 @ d2, RExp_let (s, e1', e2'))
  | RExp_let_rec (l, e) ->
    let names, el = List.unzip l in
    let* el' = map (ll_expr bindings) el in
    let* bindings' =
      List.fold names ~init:(return bindings) ~f:(fun acc name ->
        let* acc = acc in
        let* fresh_name = lift_name in
        return @@ Map.update acc name ~f:(fun _ -> fresh_name))
    in
    let+ d, e' = ll_expr bindings' e in
    let ds, es = List.unzip el' in
    let dl =
      List.map2_exn names es ~f:(fun name e ->
        ( (match Map.find bindings' name with
           | Some s -> s
           | None -> name)
        , e ))
    in
    List.concat ds @ d @ [ RStr_value_rec dl ], e'
;;

let ll_struct_item =
  let empty_bindings = Map.empty (module String) in
  function
  | RStr_eval e ->
    let+ d, e' = ll_expr empty_bindings e in
    d @ [ RStr_eval e' ]
  | RStr_value (s, e) ->
    let+ d, e' = ll_expr empty_bindings e in
    d @ [ RStr_value (s, e') ]
  | RStr_value_rec l ->
    let names, el = List.unzip l in
    let+ el' = map (ll_expr empty_bindings) el in
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

(*Must be used after alpha_conversion, pattern removing and closure_conversion*)
let ll structure = run (ll_struct structure) 0
