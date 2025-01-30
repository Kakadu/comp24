(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Anf_ast
open Llast
open Ast

module COUNTERMONAD = struct
  type state = int

  let return value state = state, value

  let ( >>= ) =
    fun m f state ->
    let value, st = m state in
    f st value
  ;;

  let ( let* ) = ( >>= )
  let bind = ( >>= )
  let get state = state, state
  let put state _ = state, ()
  let run f start_state = f start_state
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
  match Base.Set.mem global name_candidate with
  | true -> new_name prefix global
  | false -> return name_candidate
;;

let rec anf ctx llexpr expr_with_hole =
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
      let* _ = expr_with_hole imm_id in
      return (ACExpr (CConstraint (imm, typ))))
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
      return
        (ALetIn
           (PIdentifier fresh_name, CIfThenElse (imm_guard, then_aexpr, else_aexpr), aexp)))
  | LLTuple elems ->
    let anf_list env llexprs cont =
      let rec helper acc = function
        | [] -> cont (List.rev acc)
        | h :: tl -> anf env h (fun imm -> helper (imm :: acc) tl)
      in
      helper [] llexprs
    in
    anf_list ctx elems (fun list ->
      let* fresh_name = new_name Tuple ctx in
      let imm_id = ImmIdentifier fresh_name in
      let* aexp = expr_with_hole imm_id in
      return (ALetIn (PIdentifier fresh_name, CTuple list, aexp)))
  | LLApplication (left_exp, right_exp) ->
    anf ctx left_exp (fun imm_left ->
      anf ctx right_exp (fun imm_right ->
        let* fresh_name = new_name Application ctx in
        let imm_id = ImmIdentifier fresh_name in
        let* aexp = expr_with_hole imm_id in
        return (ALetIn (PIdentifier fresh_name, CApplication (imm_left, imm_right), aexp))))
  | LLMatch (pat, cases) ->
    let rec convert_cases cases acc =
      match cases with
      | (pat, exp) :: tl ->
        let* aexpr = anf ctx exp (fun timm -> return (ACExpr (CImmExpr timm))) in
        convert_cases tl ((pat, aexpr) :: acc)
      | [] -> return (List.rev acc)
    in
    let* cases = convert_cases cases [] in
    return (ACExpr (CMatch (pat, cases)))
  | LLLetIn (_, pat, outer, inner) ->
    let new_env = Lambda_lifting.collect_bindings_from_pat pat in
    anf new_env outer (fun imm_outer ->
      let* aexp = anf new_env inner expr_with_hole in
      return (ALetIn (pat, CImmExpr imm_outer, aexp)))
;;

let rec map1 f = function
  | [] -> return []
  | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
;;

let anf_decl env =
  let rec init_ctx pat_list acc =
    match pat_list with
    | pat :: tl ->
      init_ctx tl (Base.Set.union acc (Lambda_lifting.collect_bindings_from_pat pat))
    | [] -> acc
  in
  let proccess_lllet = function
    | LLLet (pat, args, e) ->
      let* aexp = anf (init_ctx args env) e (fun ie -> return (ACExpr (CImmExpr ie))) in
      return (ALet (pat, args, aexp))
  in
  function
  | LLDSingleLet (rec_flag, LLLet (pat, args, e)) ->
    let* anf_let = proccess_lllet (LLLet (pat, args, e)) in
    return (ADSingleLet (rec_flag, anf_let))
  | LLDMutualRecDecl (rec_flag, decls) ->
    let* anf_decls = map1 proccess_lllet decls in
    return (ADMutualRecDecl (rec_flag, anf_decls))
;;

let transform decls =
  let rec init_ctx decls acc =
    match decls with
    | [] -> acc
    | LLDSingleLet (_, LLLet (pat, _, _)) :: tl ->
      init_ctx tl (Lambda_lifting.collect_bindings_from_pat pat)
    | LLDMutualRecDecl (_, decls) :: tl_decls ->
      let new_acc =
        List.fold_left
          (fun acc (LLLet (pat, _, _)) ->
             Base.Set.union acc (Lambda_lifting.collect_bindings_from_pat pat))
          (Base.Set.empty (module Base.String))
          decls
      in
      init_ctx tl_decls new_acc
  in
  let ctx = init_ctx decls ((module Base.String) |> Base.Set.empty) in
  List.map
    (fun decl ->
       match run (anf_decl ctx decl) 0 with
       | _, transformed -> transformed)
    decls
;;
