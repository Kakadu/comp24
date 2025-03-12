(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Reduced_ast
open Anf_ast
open Middleend_utils.Monads

let fresh_var = fresh >>| Int.to_string >>| ( ^ ) "__anf_"
let invalid_prev res = Invalid_previous_result ("anf", res)
let atom a = CExp_atom a
let complex c = LComplex c
let let_in name v exp = LLet_in (name, v, exp)

type let_part = string * cexpr

let let_part_to_lexp parts final =
  List.fold_right (fun (name, v) lexp -> let_in name v lexp) parts final
;;

let rec rexp_to_aexp : rexpr -> (let_part list * aexpr) t = function
  | RExp_ident name -> return ([], AExp_ident name)
  | RExp_constant c -> return ([], AExp_constant c)
  | RExp_tuple l ->
    let+ lexps, aexps =
      fold_left
        (fun (lexps, aexps) rexp ->
          let+ lexp, aexp = rexp_to_aexp rexp in
          lexps @ lexp, aexp :: aexps)
        (return ([], []))
        l
    in
    let aexps = List.rev aexps in
    lexps, AExp_tuple aexps
  | RExp_construct (name, Some e) ->
    let+ lexps, aexp = rexp_to_aexp e in
    lexps, AExp_construct (name, Some aexp)
  | RExp_construct (name, None) -> return ([], AExp_construct (name, None))
  | RExp_let (name, exp1, exp2) ->
    let+ lexps1, cexp = rexp_to_cexp exp1
    and+ lexps2, aexp = rexp_to_aexp exp2 in
    lexps1 @ lexps2 @ [ name, cexp ], aexp
  | RExp_let_rec _ -> fail @@ invalid_prev "unexpected \"let rec in\" after ll"
  | RExp_fun _ -> fail @@ invalid_prev "unexpected \"fun\" after ll"
  | _ as e ->
    let+ lexps, cexp = rexp_to_cexp e
    and+ var = fresh_var in
    lexps @ [ var, cexp ], AExp_ident var

and rexp_to_cexp : rexpr -> (let_part list * cexpr) t = function
  | (RExp_tuple _ | RExp_ident _ | RExp_constant _ | RExp_construct _) as e ->
    let+ lexp, aexp = rexp_to_aexp e in
    lexp, atom aexp
  | RExp_apply _ as exp ->
    let rec helper args = function
      | RExp_apply (l, r) -> helper (r :: args) l
      | RExp_ident fname -> return (fname, args)
      | _ -> fail @@ invalid_prev "invalid apply after ll"
    in
    let* f, args = helper [] exp in
    let+ data =
      map
        (fun el ->
          let+ lexp, exp = rexp_to_aexp el in
          lexp, exp)
        args
    in
    let lexps, args = List.split data in
    let lexps = List.flatten lexps in
    lexps, CExp_apply (f, args)
  | RExp_if (i, t, e) ->
    let+ lexp1, i = rexp_to_aexp i
    and+ t = rexp_to_lexp t
    and+ e =
      match e with
      | Some e -> rexp_to_lexp e
      | None -> return (complex @@ atom (AExp_constant Const_unit))
    in
    lexp1, CExp_if (i, t, e)
  | RExp_let (name, exp1, exp2) ->
    let+ lexps1, cexp = rexp_to_cexp exp1
    and+ lexps2, cexp2 = rexp_to_cexp exp2 in
    lexps1 @ [ name, cexp ] @ lexps2, cexp2
  | RExp_let_rec _ -> fail @@ invalid_prev "unexpected \"let rec in\" after ll"
  | RExp_fun _ -> fail @@ invalid_prev "unexpected \"fun\" after ll"

and rexp_to_lexp rexp : lexpr t =
  let+ lexps, cexp = rexp_to_cexp rexp in
  let_part_to_lexp lexps (complex cexp)
;;

let struct_to_anf = function
  | RStr_eval e ->
    let+ lexp = rexp_to_lexp e in
    AbsStr_value (Utils.Predefined_ops.var_nothing, lexp)
  | RStr_value (name, RExp_fun (args, rexp)) ->
    let+ lexp = rexp_to_lexp rexp in
    AbsStr_func (name, args, lexp)
  | RStr_value (name, rexp) ->
    let+ lexp = rexp_to_lexp rexp in
    AbsStr_value (name, lexp)
  | RStr_value_rec rexps ->
    let+ funs =
      map
        (fun (name, exp) ->
          let rec helper acc = function
            | RExp_fun (args, rexp) ->
              let+ lexp = rexp_to_lexp rexp in
              let rem_args =
                let rec helper2 l r =
                  match l, r with
                  | (cur_lexp, cexp) :: tl, arg :: tl2 ->
                    cur_lexp @ [ arg, cexp ] @ helper2 tl tl2
                  | _, _ -> []
                in
                helper2 acc args
              in
              let lexp = let_part_to_lexp rem_args lexp in
              name, List.filter (( <> ) name) args, lexp
            | RExp_apply (l, r) ->
              let* lexps, cexp = rexp_to_cexp r in
              helper ((lexps, cexp) :: acc) l
            | _ -> fail @@ invalid_prev "BDSML doesn't support vars in \"let rec\""
          in
          helper [] exp)
        rexps
    in
    AbsStr_value_rec funs
;;

let rast_to_anf rast = run (map struct_to_anf rast) 0
