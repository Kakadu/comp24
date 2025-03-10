(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anfast
open Base
open Ast
open Llast
open Name_helper
open Pattern_helper

let anf_const = function
  | CInt i -> AInt i
  | CBool b -> ABool b
  | CNil -> ANil
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

let rec anf_llexpression new_anf_let gvars fvars lvars = function
  | LLVar v ->
    let new_vars =
      match Map.find lvars v with
      | Some p -> AVar p
      | None ->
        (match Map.find gvars v with
         | Some p -> AVar p
         | None -> AVar v)
    in
    new_vars, new_anf_let, fvars
  | LLConst c -> AConst (anf_const c), new_anf_let, fvars
  | LLIfElse (i, t, e) ->
    let check_i, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars i in
    let check_t, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars t in
    let check_e, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars e in
    AIfElse (check_i, check_t, check_e), new_anf_let, fvars
  | LLEbinOp (op, e1, LLApp (l, r)) ->
    let name_app = get_uniq_name fvars "app" in
    let new_fvars = Set.add fvars name_app in
    let gvars = Map.set gvars ~key:name_app ~data:name_app in
    let anf_l, new_anf_let, new_fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars l
    in
    let anf_r, new_anf_let, fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars r
    in
    let app = AApp (anf_l, anf_r) in
    let new_anf_let = ALet (Notrec, name_app, [], app) :: new_anf_let in
    anf_llexpression new_anf_let gvars fvars lvars (LLEbinOp (op, e1, LLVar name_app))
  | LLEbinOp (op, e1, e2) ->
    let new_name = get_uniq_name fvars "bin_op" in
    let new_fvars = Set.add fvars new_name in
    let gvars = Map.set gvars ~key:new_name ~data:new_name in
    let anf_e1, new_anf_let, new_fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars e1
    in
    let anf_e2, new_anf_let, fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars e2
    in
    let anf_binop = ABinOp (op, anf_e1, anf_e2) in
    let new_anf_let = ALet (Notrec, new_name, [], anf_binop) :: new_anf_let in
    AVar new_name, new_anf_let, fvars
  | LLApp (l, LLApp (rl, rr)) ->
    let name_app = get_uniq_name fvars "app" in
    let new_fvars = Set.add fvars name_app in
    let gvars = Map.set gvars ~key:name_app ~data:name_app in
    let anf_rl, new_anf_let, new_fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars rl
    in
    let anf_rr, new_anf_let, fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars rr
    in
    let app = AApp (anf_rl, anf_rr) in
    let new_anf_let = ALet (Notrec, name_app, [], app) :: new_anf_let in
    let check_l, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars l in
    AApp (check_l, AVar name_app), new_anf_let, fvars
  | LLApp (l, r) ->
    let check_l, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars l in
    let check_r, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars r in
    AApp (check_l, check_r), new_anf_let, fvars
  | LLPatLetIn (n, e, ine) ->
    let anf_e1, new_anf_let, new_fvars =
      anf_llexpression new_anf_let gvars fvars lvars e
    in
    let anf_e2, new_anf_let, fvars =
      anf_llexpression new_anf_let gvars new_fvars lvars ine
    in
    APatLetIn (n, anf_e1, anf_e2), new_anf_let, fvars
  | LLTuple t ->
    let anf_t, new_anf_let, fvars =
      List.fold
        ~init:([], new_anf_let, fvars)
        ~f:(fun (acc, acc_new_anf_let, fvars) c ->
          let anf_, new_let, fvars =
            anf_llexpression acc_new_anf_let gvars fvars lvars c
          in
          anf_ :: acc, new_let, fvars)
        t
    in
    ATuple (List.rev anf_t), new_anf_let, fvars
  | LLMatch (m, b) ->
    let anf_m, new_lets, fvars = anf_llexpression new_anf_let gvars fvars lvars m in
    let rec convert_match new_lets fvars expr = function
      | [] -> failwith "empty_match"
      | [ (_, body) ] -> anf_llexpression new_lets gvars fvars lvars body
      | (pattern, body) :: rest ->
        let anf_then, new_lets, fvars =
          anf_llexpression new_lets gvars fvars lvars body
        in
        let anf_else, new_lets, fvars = convert_match new_lets fvars expr rest in
        (* Рекурсивно строим цепочку if-else *)
        ( AIfElse (ABinOp (Eq, expr, anf_pattern pattern), anf_then, anf_else)
        , new_lets
        , fvars )
    in
    convert_match new_lets fvars anf_m b
  | LLList (hd, tl) ->
    let anf_hd, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars hd in
    let anf_tl, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars tl in
    AList (anf_hd, anf_tl), new_anf_let, fvars
  | LLVars (l, r) ->
    let anf_l, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars l in
    let anf_r, new_anf_let, fvars = anf_llexpression new_anf_let gvars fvars lvars r in
    AVars (anf_l, anf_r), new_anf_let, fvars
;;

let lift_anf_bindings gvars fvars = function
  | LLLet (r, n, args, e) ->
    let new_lets, old_lets, gvars, fvars =
      let new_names, gvars, fvars =
        if Poly.( = ) r Rec && Poly.( = ) n "()"
        then update_name "unit" gvars fvars
        else if Poly.( = ) r Rec
        then update_name n gvars fvars
        else n, gvars, fvars
      in
      let new_args, lvars, new_fvars =
        List.fold
          ~init:([], map_empty, fvars)
          ~f:(fun (acc_args, acc_lvars, acc_fvars) arg ->
            let new_arg, new_lvars, new_fvars =
              uniq_pattern_name_and_skip_unit acc_lvars acc_fvars arg
            in
            new_arg :: acc_args, new_lvars, new_fvars)
          args
      in
      let new_args =
        List.fold ~init:[] ~f:(fun acc arg -> acc @ pattern_to_string_list arg)
        @@ List.rev new_args
      in
      let old_lets, new_lets, update_fvars =
        anf_llexpression [] gvars new_fvars lvars e
      in
      let fvars = Set.union fvars (Set.diff update_fvars new_fvars) in
      let new_names, gvars, fvars =
        if Poly.( <> ) r Rec && Poly.( = ) n "()"
        then update_name "unit" gvars fvars
        else if Poly.( <> ) r Rec
        then update_name n gvars fvars
        else new_names, gvars, fvars
      in
      new_lets, ALet (r, new_names, new_args, old_lets), gvars, fvars
    in
    List.rev new_lets @ [ old_lets ], gvars, fvars
  | _ -> failwith ""
;;

let lift_statments statments =
  List.fold
    ~init:([], map_empty, set_empty)
    ~f:(fun (acc, gvars, fvars) anf_binding ->
      let anf_bindings, gvars, fvars = lift_anf_bindings gvars fvars anf_binding in
      acc @ anf_bindings, gvars, fvars)
    statments
;;

let rec find_free gvars new_app = function
  | AVar v ->
    let a, gvars =
      match Map.find new_app v with
      | Some p ->
        List.fold
          ~init:(AVar v, Set.add gvars v)
          ~f:(fun (acc, gvars) a -> AApp (acc, AVar a), Set.add gvars a)
          p
      | None -> AVar v, Set.add gvars v
    in
    a, gvars
  | AConst c -> AConst c, gvars
  | AApp (e1, e2) ->
    let new_e1, update_gvars = find_free gvars new_app e1 in
    let new_e2, update_gvars = find_free update_gvars new_app e2 in
    AApp (new_e1, new_e2), update_gvars
  | ABinOp (op, e1, e2) ->
    let new_e1, new_gvars = find_free gvars new_app e1 in
    let new_e2, new_gvars = find_free new_gvars new_app e2 in
    ABinOp (op, new_e1, new_e2), new_gvars
  | AIfElse (i, t, e) ->
    let new_i, new_gvars = find_free gvars new_app i in
    let new_t, new_gvars = find_free new_gvars new_app t in
    let new_e, new_gvars = find_free new_gvars new_app e in
    AIfElse (new_i, new_t, new_e), new_gvars
  | ATuple t ->
    let new_t, new_gvars =
      List.fold
        ~init:([], gvars)
        ~f:(fun (acc_t, acc_gvars) e ->
          let new_e, new_gvars = find_free acc_gvars new_app e in
          new_e :: acc_t, new_gvars)
        t
    in
    ATuple (List.rev new_t), new_gvars
  | AVars (hd, tl) ->
    let new_hd, new_gvars = find_free gvars new_app hd in
    let new_tl, new_gvars = find_free new_gvars new_app tl in
    AVars (new_hd, new_tl), new_gvars
  | AList (hd, tl) ->
    let new_hd, new_gvars = find_free gvars new_app hd in
    let new_tl, new_gvars = find_free new_gvars new_app tl in
    AList (new_hd, new_tl), new_gvars
  | APatLetIn (names, anf_e, anf_ein) ->
    let new_anf_e, new_gvars = find_free gvars new_app anf_e in
    let new_anf_ein, new_gvars = find_free new_gvars new_app anf_ein in
    let str_names = pattern_to_string_list names in
    let new_gvars =
      List.fold ~init:new_gvars ~f:(fun new_gvars n -> Set.remove new_gvars n) str_names
    in
    APatLetIn (names, new_anf_e, new_anf_ein), new_gvars
;;

let add_free gvars add_app = function
  | ALet (r, n, args, anf_e) ->
    let new_anf_bindings =
      let new_args_str = if Poly.( = ) r Rec then n :: args else args in
      let new_gvars =
        List.fold ~init:gvars ~f:(fun acc_gvars n -> Set.add acc_gvars n) new_args_str
      in
      let new_body, new_gvars_args = find_free new_gvars add_app anf_e in
      let update_args = Set.diff new_gvars_args new_gvars in
      let new_args =
        List.fold
          ~init:args
          ~f:(fun acc_args arg -> arg :: acc_args)
          (List.rev @@ Set.to_list update_args)
      in
      let new_app =
        if not (Set.is_empty update_args)
        then Map.set add_app ~key:n ~data:(Set.to_list update_args)
        else add_app
      in
      let new_gvars = Set.add gvars n in
      (r, n, new_args, new_body), new_gvars, new_app
    in
    let (r, n, a, anf_e), b, c = new_anf_bindings in
    ALet (r, n, a, anf_e), b, c
  | ALetPat (names, anf_e) ->
    let new_anf_bindings =
      let new_body, _ = find_free gvars add_app anf_e in
      let new_gvars =
        List.fold ~init:gvars ~f:(fun acc_gvars n -> Set.add acc_gvars n)
        @@ pattern_to_string_list names
      in
      (names, new_body), new_gvars, add_app
    in
    let new_let_pat, new_gvars, new_app = new_anf_bindings in
    ALetPat new_let_pat, new_gvars, new_app
  | AExpression _ -> failwith ""
;;

let anf statments =
  let a, _, _ = lift_statments statments in
  let gvars = Set.add set_empty "print_int" in
  let gvars = Set.add gvars "()" in
  let b, _, _ =
    List.fold
      ~init:([], gvars, map_empty)
      ~f:(fun (acc, gvars, new_app) anf_binding ->
        let a, b, c = add_free gvars new_app anf_binding in
        a :: acc, b, c)
      a
  in
  List.rev b
;;
