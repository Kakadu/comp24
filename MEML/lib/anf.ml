(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anfast
open Base
open Ast
open Llast
open Name_helper

let anf_const = function
  | CInt i -> AInt i
  | CBool b -> ABool b
  | CNil -> ANil
;;

let rec create_let acc = function
  | [] -> acc
  | (n, lle) :: tl -> ALetIn (n, lle, create_let acc tl)
;;

let rec anf_pattern = function
  | PVar (v, _) -> AVar v
  | PWild -> AVar "_"
  | PTuple t ->
    let anf_t =
      List.rev @@ List.fold ~init:[] ~f:(fun acc le -> anf_pattern le :: acc) t
    in
    ATuple anf_t
  | PCon (l, r) ->
    let anf_l = anf_pattern l in
    let anf_r = anf_pattern r in
    AList (anf_l, anf_r)
  | PConst c -> AConst (anf_const c)
;;

let rec anf_llexpression new_letin gvars = function
  | LLVar v -> AVar v, new_letin, gvars
  | LLConst c -> AConst (anf_const c), new_letin, gvars
  | LLVars (l, r) ->
    let anf_l, new_letin, gvars = anf_llexpression new_letin gvars l in
    let anf_r, new_letin, gvars = anf_llexpression new_letin gvars r in
    AVars (anf_l, anf_r), new_letin, gvars
  | LLIfElse (i, t, e) ->
    let anf_i, new_letin, gvars = anf_llexpression new_letin gvars i in
    let anf_t, t_letin, gvars = anf_llexpression [] gvars t in
    let then_let_in = create_let anf_t (List.rev t_letin) in
    let anf_e, e_letin, gvars = anf_llexpression [] gvars e in
    let else_let_in = create_let anf_e (List.rev e_letin) in
    let let_in_if =
      create_let (AIfElse (anf_i, then_let_in, else_let_in)) (List.rev new_letin)
    in
    let_in_if, [], gvars
  | LLEbinOp (op, l, r) ->
    let new_name = get_uniq_name gvars "bin_op" in
    let gvars = Set.add gvars new_name in
    let anf_l, new_letin, gvars = anf_llexpression new_letin gvars l in
    let anf_r, new_letin, gvars = anf_llexpression new_letin gvars r in
    let anf_binop = ABinOp (op, anf_l, anf_r) in
    let new_letin = (new_name, anf_binop) :: new_letin in
    AVar new_name, new_letin, gvars
  | LLApp (l, LLApp (rl, rr)) ->
    let new_name = get_uniq_name gvars "app" in
    let gvars = Set.add gvars new_name in
    let anf_l, new_letin, gvars = anf_llexpression new_letin gvars l in
    let anf_rl, new_letin, gvars = anf_llexpression new_letin gvars rl in
    let anf_rr, new_letin, gvars = anf_llexpression new_letin gvars rr in
    let anf_app = AApp (anf_rl, anf_rr) in
    let new_letin = (new_name, anf_app) :: new_letin in
    AApp (anf_l, AVar new_name), new_letin, gvars
  | LLApp (l, r) ->
    let anf_l, new_letin, gvars = anf_llexpression new_letin gvars l in
    let anf_r, new_letin, gvars = anf_llexpression new_letin gvars r in
    AApp (anf_l, anf_r), new_letin, gvars
  | LLMatch (m, b) ->
    let anf_m, new_lets, gvars = anf_llexpression new_letin gvars m in
    let rec convert_match new_lets gvars expr = function
      | [] -> failwith "empty_match"
      | [ (_, body) ] -> anf_llexpression new_lets gvars body
      | (pattern, body) :: rest ->
        let anf_then, new_lets, fvars = anf_llexpression new_lets gvars body in
        let anf_else, new_lets, fvars = convert_match new_lets fvars expr rest in
        (* Рекурсивно строим цепочку if-else *)
        ( AIfElse (ABinOp (Eq, expr, anf_pattern pattern), anf_then, anf_else)
        , new_lets
        , fvars )
    in
    convert_match new_lets gvars anf_m b
  | LLLetIn (n, e, ine) ->
    let anf_e, new_letin, gvars = anf_llexpression new_letin gvars e in
    let anf_ine, new_letin, gvars = anf_llexpression new_letin gvars ine in
    ALetIn (n, anf_e, anf_ine), new_letin, gvars
  | LLPatLetIn _ | LLTuple _ | LLList _ -> failwith ""
;;

let name_pattern = function
  | PVar (v, _) -> v
  | PWild -> "_"
  | _ -> failwith ""
;;

let anf_llbindings gvars = function
  | LLLet (r, n, args, lle) ->
    let gvars = Set.add gvars n in
    let str_args =
      List.rev
      @@ List.fold
           ~init:[]
           ~f:(fun acc p ->
             let n = name_pattern p in
             n :: acc)
           args
    in
    let ae, new_letin, _ = anf_llexpression [] gvars lle in
    let new_body = create_let ae (List.rev new_letin) in
    ALet (r, n, str_args, new_body), gvars
  | _ -> failwith ""
;;

let anf statment =
  let gvars = set_empty in
  let anf_ast, _ =
    List.fold
      ~init:([], gvars)
      ~f:(fun (acc, gvars) llbindings ->
        let abindings, gvars = anf_llbindings gvars llbindings in
        abindings :: acc, gvars)
      statment
  in
  List.rev anf_ast
;;
