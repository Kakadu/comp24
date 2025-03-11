(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module UsedIdents = Set.Make (String)
module TopDecl = Set.Make (String)
module ApplyPromote = Map.Make (String)
open Reduced_ast

let rec closure_exp apps env = function
  | RExp_ident x
    when (Option.is_none @@ TopDecl.find_opt x env)
         && (Option.is_none
             @@ List.find_opt
                  (fun Utils.Predefined_ops.{ alt_name } -> x = alt_name)
                  Utils.Predefined_ops.predefine_operators) ->
    let promote_apply x =
      match ApplyPromote.find_opt x apps with
      | Some apply ->
        List.fold_left (fun n arg -> RExp_apply (n, RExp_ident arg)) (RExp_ident x) apply
      | None -> RExp_ident x
    in
    promote_apply x, UsedIdents.singleton x
  | RExp_fun (args, exp) ->
    let exp, vars = closure_exp apps env exp in
    let new_args = UsedIdents.diff vars @@ UsedIdents.of_list args in
    let new_args_list = UsedIdents.to_list new_args in
    let exp =
      List.fold_left
        (fun exp el -> RExp_apply (exp, RExp_ident el))
        (RExp_fun (new_args_list @ args, exp))
        new_args_list
    in
    exp, new_args
  | RExp_let (name, v, exp) ->
    let exp, vars = closure_exp apps env exp in
    let vars = UsedIdents.remove name vars in
    let v, vars2 = closure_exp apps env v in
    let vars = UsedIdents.union vars vars2 in
    RExp_let (name, v, exp), vars
  | RExp_let_rec (binds, exp) ->
    let (new_vars, vars2), data =
      List.fold_left_map
        (fun (new_vars, vars) (name, exp) ->
          match exp with
          | RExp_fun (args, exp) ->
            let exp, vars2 = closure_exp apps env exp in
            let new_args = UsedIdents.diff vars2 @@ UsedIdents.of_list (name :: args) in
            let new_args_list = UsedIdents.to_list new_args in
            ( (UsedIdents.add name new_vars, UsedIdents.union vars new_args)
            , ((name, RExp_fun (new_args_list @ args, exp)), (name, new_args_list)) )
          | _ as exp ->
            let exp, vars2 = closure_exp apps env exp in
            ( (UsedIdents.add name new_vars, UsedIdents.union vars vars2)
            , ((name, exp), (name, TopDecl.to_list vars2)) ))
        (UsedIdents.empty, UsedIdents.empty)
        binds
    in
    let binds, new_apps = List.split data in
    let add_to_apps apps_init =
      List.fold_left (fun acc (key, v) -> ApplyPromote.add key v acc) apps_init new_apps
    in
    let last_apps = add_to_apps ApplyPromote.empty in
    let binds =
      List.map
        (fun (name, exp) ->
          match exp with
          | RExp_fun (args, exp) ->
            let exp, _ = closure_exp last_apps env exp in
            name, RExp_fun (args, exp)
          | _ as exp -> name, fst @@ closure_exp last_apps env exp)
        binds
    in
    let apps = add_to_apps apps in
    let exp, vars = closure_exp apps env exp in
    let vars = UsedIdents.union vars vars2 in
    let vars = UsedIdents.diff vars new_vars in
    RExp_let_rec (binds, exp), vars
  | RExp_apply (l, r) ->
    let l, vars1 = closure_exp apps env l
    and r, vars2 = closure_exp apps env r in
    RExp_apply (l, r), UsedIdents.union vars1 vars2
  | RExp_tuple l ->
    let vars, l =
      List.fold_left_map
        (fun vars el ->
          let exp, vars2 = closure_exp apps env el in
          UsedIdents.union vars vars2, exp)
        UsedIdents.empty
        l
    in
    RExp_tuple l, vars
  | RExp_construct (name, Some exp) ->
    let exp, vars = closure_exp apps env exp in
    RExp_construct (name, Some exp), vars
  | RExp_if (i, t, e) ->
    let ie, iv = closure_exp apps env i
    and te, tv = closure_exp apps env t
    and ee, ev =
      match e with
      | Some e ->
        let e, v = closure_exp apps env e in
        Some e, v
      | _ -> None, UsedIdents.empty
    in
    let vars = UsedIdents.union (UsedIdents.union iv tv) ev in
    RExp_if (ie, te, ee), vars
  | _ as exp -> exp, UsedIdents.empty
;;

let closure_struct env =
  let apps = ApplyPromote.empty in
  function
  | RStr_eval e ->
    let e, _ = closure_exp apps env e in
    env, RStr_eval e
  | RStr_value (name, exp) ->
    let env = TopDecl.add name env in
    let e, _ = closure_exp apps env exp in
    env, RStr_value (name, e)
  | RStr_value_rec binds ->
    let env = List.fold_left (fun env (name, _) -> TopDecl.add name env) env binds in
    ( env
    , RStr_value_rec
        (List.map (fun (name, e) -> name, fst @@ closure_exp apps env e) binds) )
;;

let closure_convert program =
  snd @@ List.fold_left_map (fun env el -> closure_struct env el) TopDecl.empty program
;;
