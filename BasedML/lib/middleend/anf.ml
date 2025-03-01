(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Anf_ast
open Llast
open Ast

module COUNTERMONAD = struct
  let return x : _ = fun st -> st, Result.ok x

  let ( >>= ) l r : _ =
    fun st ->
    let st, x = l st in
    match x with
    | Result.Ok x -> r x st
    | Result.Error s -> st, Result.error s
  ;;

  let ( let* ) = ( >>= )
  let get st = st, Result.ok st
  let put s _oldstate = s, Result.ok ()
  let run f = f
  let fail err st = st, Result.error err
end

open COUNTERMONAD

type prefix =
  | IfThenElse
  | Tuple
  | Application
  | Constraint

let prefix_to_string = function
  | IfThenElse -> "anf_ifthenelse_"
  | Tuple -> "anf_tuple_"
  | Application -> "anf_app_"
  | Constraint -> "anf_constraint_"
;;

let get_new_num =
  let* i = get in
  let* () = put (i + 1) in
  return i
;;

(** Variable Generation *)
let rec new_name prefix global =
  let* new_num = get_new_num in
  let name_candidate =
    Base.String.concat [ prefix_to_string prefix; Base.Int.to_string new_num ]
  in
  if Base.Set.mem global name_candidate
  then new_name prefix global
  else return name_candidate
;;

let rec anf ctx llexpr expr_with_hole =
  let anf_list env llexprs cont =
    let rec helper acc = function
      | [] -> cont (List.rev acc)
      | h :: tl -> anf env h (fun imm -> helper (imm :: acc) tl)
    in
    helper [] llexprs
  in
  match llexpr with
  | LLConstant const ->
    let imm_const =
      match const with
      | CBool b -> ImmBool b
      | CInt i -> ImmInt i
      | CUnit -> ImmUnit
      | CNil -> ImmNil
    in
    expr_with_hole imm_const
  | LLIdentifier id -> expr_with_hole (ImmIdentifier id)
  | LLConstraint (llexp, typ) ->
    anf ctx llexp (fun imm ->
      let* fresh_name = new_name Constraint ctx in
      let imm_id = ImmIdentifier fresh_name in
      let* aexp = expr_with_hole imm_id in
      return (ALetIn (fresh_name, CImmExpr (ImmConstraint (imm, typ)), aexp)))
  | LLIfThenElse (guard, then_branch, else_branch) ->
    anf ctx guard (fun imm_guard ->
      let* then_aexpr =
        anf ctx then_branch (fun imm_then -> return (ACExpr (CImmExpr imm_then)))
      in
      let* else_aexpr =
        anf ctx else_branch (fun imm_else -> return (ACExpr (CImmExpr imm_else)))
      in
      let* fresh_name = new_name IfThenElse ctx in
      let imm_id = ImmIdentifier fresh_name in
      let* aexp = expr_with_hole imm_id in
      return (ALetIn (fresh_name, CIfThenElse (imm_guard, then_aexpr, else_aexpr), aexp)))
  | LLTuple elems ->
    anf_list ctx elems (fun list ->
      let* fresh_name = new_name Tuple ctx in
      let imm_id = ImmIdentifier fresh_name in
      let* aexp = expr_with_hole imm_id in
      return (ALetIn (fresh_name, CImmExpr (ImmTuple list), aexp)))
  | LLApplication (left, right) ->
    let rec sep_llapp exp cont =
      match exp with
      | LLApplication (left, right) ->
        sep_llapp left (fun rest_llexp -> cont (right :: rest_llexp))
      | llexp -> cont [], llexp
    in
    let rest, llexp = sep_llapp (LLApplication (left, right)) Fun.id in
    anf ctx llexp (fun imm_exp ->
      anf_list ctx rest (fun imm_rest ->
        let* fresh_name = new_name Application ctx in
        let imm_id = ImmIdentifier fresh_name in
        let* aexp = expr_with_hole imm_id in
        let rec build_app = function
          | [ h; tl ] -> return (CApplication (CImmExpr h, CImmExpr tl))
          | h :: tl ->
            let* rest = build_app tl in
            return (CApplication (CImmExpr h, rest))
          | [] -> fail "Error while building application"
        in
        let* built_app = build_app (imm_exp :: List.rev imm_rest) in
        return (ALetIn (fresh_name, built_app, aexp))))
  | LLLetIn (_, PIdentifier id, outer, inner) ->
    let new_env = Lambda_lifting.collect_bindings_from_pat (PIdentifier id) in
    anf new_env outer (fun imm_outer ->
      let* aexp = anf new_env inner expr_with_hole in
      return (ALetIn (id, CImmExpr imm_outer, aexp)))
  | _ -> "Error: unexpected expression" |> fail
;;

let rec map1 f = function
  | [] -> return []
  | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
;;

let anf_decl env =
  let collect_bindings acc pat =
    Base.Set.union acc (Lambda_lifting.collect_bindings_from_pat pat)
  in
  let process_lllet = function
    | LLLet (PIdentifier id, args, e) ->
      let* new_args =
        map1
          (function
            | PIdentifier x -> x |> return
            | _ -> fail "Error: unexpected arguments")
          args
      in
      let* aexp =
        anf (List.fold_left collect_bindings env args) e (fun ie ->
          return (ACExpr (CImmExpr ie)))
      in
      return (ALet (id, new_args, aexp))
    | _ -> fail "Error: unexpected lllet"
  in
  function
  | LLDSingleLet (rec_flag, LLLet (pat, args, e)) ->
    let* anf_let = process_lllet (LLLet (pat, args, e)) in
    return (ADSingleLet (rec_flag, anf_let))
  | LLDMutualRecDecl (_, decls) ->
    let* anf_decls = map1 process_lllet decls in
    return (ADMutualRecDecl anf_decls)
;;

let transform decls =
  let collect_bindings decls =
    List.fold_left
      (fun acc -> function
        | LLDSingleLet (_, LLLet (pat, _, _)) ->
          Base.Set.union acc (Lambda_lifting.collect_bindings_from_pat pat)
        | LLDMutualRecDecl (_, decls) ->
          List.fold_left
            (fun acc (LLLet (pat, _, _)) ->
              Base.Set.union acc (Lambda_lifting.collect_bindings_from_pat pat))
            acc
            decls)
      (Base.Set.empty (module Base.String))
      decls
  in
  let ctx = collect_bindings decls in
  List.map
    (fun decl ->
      let _, transformed = run (anf_decl ctx decl) 0 in
      transformed)
    decls
;;
