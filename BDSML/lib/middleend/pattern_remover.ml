(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Reduced_ast
open Parser.Ast

exception Unimplemented

open
  Utils.Counter_monad.Make
    (Int)
    (struct
      type t = error
    end)

let fresh_var =
  let+ f = fresh in
  "__reserved_" ^ Int.to_string f
;;

let user_var name =
  match name.[0] with
  | '(' -> name
  | _ -> "__var_" ^ name
;;

let two_arg_fun_helper f a b = RExp_apply (RExp_apply (RExp_ident f, a), b)

let fun_get_n tup n =
  two_arg_fun_helper "__get_from_tuple" tup @@ RExp_constant (Const_int n)
;;

let disassemble_constructor name v =
  two_arg_fun_helper "__disassemble" (RExp_ident name) v
;;

let fun_exception name =
  RExp_apply (RExp_ident "__exception", RExp_constant (Const_string name))
;;

let same_constructor l r = two_arg_fun_helper "__same_cons" l r

let rec pattern_binder rexp = function
  | Pat_var v -> return [ user_var v, rexp ]
  | Pat_type (p, _) -> pattern_binder rexp p
  | Pat_tuple l ->
    let* fv = fresh_var in
    let+ res =
      map
        (fun (i, p) -> pattern_binder (fun_get_n (RExp_ident fv) i) p)
        (List.mapi (fun i n -> i, n) l)
    in
    (fv, rexp) :: List.flatten res
  | Pat_or _ -> fail (Invalid_pattern "or pattern cannot be bind")
  | Pat_construct (name, Some v) -> pattern_binder (disassemble_constructor name rexp) v
  | _ -> return []
;;

let rec let_bindings_unpack r bindings =
  let bindings_unpack = function
    | Pat_binding (p, exp) ->
      (match r with
       | Recursive -> fail (Invalid_ast "pat binding in let rec")
       | Nonrecursive ->
         let* rexp = expr_to_rexpr exp in
         pattern_binder rexp p)
    | Val_binding (name, pats, exp) ->
      (match pats with
       | [] ->
         let+ rexp = expr_to_rexpr exp in
         [ user_var name, rexp ]
       | _ ->
         let+ rexp = fun_to_rfun pats exp in
         [ user_var name, rexp ])
  in
  let+ unpacked =
    fold_left
      (fun l b ->
        let+ unpacked = bindings_unpack b in
        l @ unpacked)
      (return [])
      bindings
  in
  unpacked

and fun_to_rfun pats exp =
  let* name_unpacked =
    map
      (fun p ->
        let* nname = fresh_var in
        let+ unpacked = pattern_binder (RExp_ident nname) p in
        nname, unpacked)
      pats
  in
  let args, unpacked = List.split name_unpacked in
  let+ rexp = expr_to_rexpr exp in
  let body =
    List.fold_left
      (fun exp (name, v) -> RExp_let (name, v, exp))
      rexp
      (List.rev @@ List.concat unpacked)
  in
  RExp_fun (args, body)

and match_cases exp cases =
  let rec construct_bool exp = function
    | Pat_constant c -> two_arg_fun_helper "( = )" (RExp_constant c) exp
    | Pat_tuple t ->
      let res, _ =
        List.fold_left
          (fun (before, counter) el ->
            ( two_arg_fun_helper
                "( || )"
                before
                (construct_bool (fun_get_n exp counter) el)
            , counter + 1 ))
          (RExp_constant (Const_bool true), 0)
          t
      in
      res
    | _ -> RExp_constant (Const_bool true)
  in
  let unpack_case before case =
    let* unpacked = pattern_binder exp case.left in
    let+ left_exp = expr_to_rexpr case.right in
    let let_exp =
      List.fold_left (fun acc (name, v) -> RExp_let (name, v, acc)) left_exp
      @@ List.rev unpacked
    in
    RExp_if (construct_bool exp case.left, let_exp, Some before)
  in
  fold_left unpack_case (return @@ fun_exception "Match_failure") @@ List.rev cases

and expr_to_rexpr = function
  | Exp_ident x -> return (RExp_ident (user_var x))
  | Exp_constant x -> return (RExp_constant x)
  | Exp_type (e, _) -> expr_to_rexpr e
  | Exp_let (r, bindings, exp) ->
    let* unpacked = let_bindings_unpack r bindings in
    (match r with
     | Recursive ->
       let+ rexpr = expr_to_rexpr exp in
       RExp_let_rec (unpacked, rexpr)
     | Nonrecursive ->
       let rexpr = expr_to_rexpr exp in
       fold_left (fun exp (name, v) -> return @@ RExp_let (name, v, exp)) rexpr
       @@ List.rev unpacked)
  | Exp_fun (pats, exp) -> fun_to_rfun pats exp
  | Exp_apply (l, r) ->
    let+ l = expr_to_rexpr l
    and+ r = expr_to_rexpr r in
    RExp_apply (l, r)
  | Exp_match (exp, cases) ->
    let* rexp = expr_to_rexpr exp in
    match_cases rexp cases
  | Exp_tuple l ->
    let+ res = map expr_to_rexpr l in
    RExp_tuple res
  | Exp_construct (name, Some e) ->
    let+ res = expr_to_rexpr e in
    RExp_construct (name, Some res)
  | Exp_construct (name, None) -> return @@ RExp_construct (name, None)
  | Exp_if (i, t, e) ->
    let+ i = expr_to_rexpr i
    and+ t = expr_to_rexpr t
    and+ e =
      match e with
      | Some e ->
        let+ e = expr_to_rexpr e in
        Some e
      | None -> return None
    in
    RExp_if (i, t, e)
  | _ -> raise Unimplemented
;;

let ast_to_rast prog =
  let transform_struct = function
    | Str_eval e ->
      let+ e = expr_to_rexpr e in
      [ RStr_eval e ]
    | Str_value (r, bindings) ->
      let+ unpacked = let_bindings_unpack r bindings in
      (match r with
       | Recursive -> [ RStr_value_rec unpacked ]
       | Nonrecursive -> List.map (fun (name, exp) -> RStr_value (name, exp)) unpacked)
  in
  let+ rast = map transform_struct prog in
  List.concat rast
;;

let remove_patterns structure =
  match run (ast_to_rast structure) 0 with
  | Result.Ok s -> s
  | Result.Error _ -> []
;;
