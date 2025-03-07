(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module UsedIdents = Set.Make (String)
open Reduced_ast

let rec closure_exp = function
  | RExp_ident x as exp
    when Option.is_none
         @@ List.find_opt
              (fun (_, _, n) -> x = n)
              Utils.Predefined_ops.predefine_operators -> exp, UsedIdents.singleton x
  | RExp_fun (args, exp) ->
    let exp, vars = closure_exp exp in
    let new_args = UsedIdents.diff vars @@ UsedIdents.of_list args in
    let new_args_list = UsedIdents.to_list @@ new_args in
    let exp =
      List.fold_left
        (fun exp el -> RExp_apply (exp, RExp_ident el))
        (RExp_fun (new_args_list @ args, exp))
        new_args_list
    in
    exp, new_args
  | RExp_let (name, v, exp) ->
    let exp, vars = closure_exp exp in
    let vars = UsedIdents.remove name vars in
    let v, vars2 = closure_exp v in
    let vars = UsedIdents.union vars vars2 in
    RExp_let (name, v, exp), vars
  | RExp_let_rec (binds, exp) ->
    let (new_vars, vars2), binds =
      List.fold_left_map
        (fun (new_vars, vars) (name, exp) ->
          let exp, vars2 = closure_exp exp in
          (UsedIdents.add name new_vars, UsedIdents.union vars vars2), (name, exp))
        (UsedIdents.empty, UsedIdents.empty)
        binds
    in
    let exp, vars = closure_exp exp in
    let vars = UsedIdents.union vars vars2 in
    let vars = UsedIdents.diff vars new_vars in
    RExp_let_rec (binds, exp), vars
  | RExp_apply (l, r) ->
    let l, vars1 = closure_exp l
    and r, vars2 = closure_exp r in
    RExp_apply (l, r), UsedIdents.union vars1 vars2
  | RExp_tuple l ->
    let vars, l =
      List.fold_left_map
        (fun vars el ->
          let exp, vars2 = closure_exp el in
          UsedIdents.union vars vars2, exp)
        UsedIdents.empty
        l
    in
    RExp_tuple l, vars
  | RExp_construct (name, Some exp) ->
    let exp, vars = closure_exp exp in
    RExp_construct (name, Some exp), vars
  | RExp_if (i, t, e) ->
    let ie, iv = closure_exp i
    and te, tv = closure_exp t
    and ee, ev =
      match e with
      | Some e ->
        let e, v = closure_exp e in
        Some e, v
      | _ -> None, UsedIdents.empty
    in
    let vars = UsedIdents.union (UsedIdents.union iv tv) ev in
    RExp_if (ie, te, ee), vars
  | _ as exp -> exp, UsedIdents.empty
;;

let closure_struct = function
  | RStr_eval e ->
    let e, _ = closure_exp e in
    RStr_eval e
  | RStr_value (name, exp) ->
    let e, _ = closure_exp exp in
    RStr_value (name, e)
  | RStr_value_rec binds ->
    RStr_value_rec (List.map (fun (name, e) -> name, fst @@ closure_exp e) binds)
;;

let closure_convert = List.map closure_struct
