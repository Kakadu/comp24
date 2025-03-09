(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ll_ast
open Anf_ast
open Common
open Common.Counter_Monad

let gen_name = gen_name "anf"

let fold_app e =
  let rec helper e k =
    match e with
    | LLApp (l, r) -> helper l (fun args -> k (r :: args))
    | e -> e, List.rev (k [])
  in
  helper e (fun x -> x)
;;

let rec anf_llexpr e expr_with_hole =
  let anf_many es k =
    let rec helper acc = function
      | h :: tl -> anf_llexpr h (fun imm -> helper (imm :: acc) tl)
      | [] -> k (List.rev acc)
    in
    helper [] es
  in
  match e with
  | LLConst c -> expr_with_hole (ImmConst c)
  | LLVar id -> expr_with_hole (ImmVar id)
  | LLTuple (e1, e2, es) ->
    anf_llexpr e1 (fun imm1 ->
      anf_llexpr e2 (fun imm2 ->
        anf_many es (fun imms ->
          let* name = gen_name in
          let* ae = expr_with_hole (ImmVar name) in
          return @@ ALetIn (name, CImm (ImmTuple (imm1, imm2, imms)), ae))))
  | LLList es ->
    anf_many es (fun imms ->
      let* name = gen_name in
      let* ae = expr_with_hole (ImmVar name) in
      return @@ ALetIn (name, CImm (ImmList imms), ae))
  | LLBranch (cond, t, f) ->
    anf_llexpr cond (fun imm ->
      let* name = gen_name in
      let* body = expr_with_hole (ImmVar name) in
      let* t_aexpr = anf_llexpr t (fun imm -> return @@ ACExpr (CImm imm)) in
      let* f_aexpr = anf_llexpr f (fun imm -> return @@ ACExpr (CImm imm)) in
      return (ALetIn (name, CBranch (imm, t_aexpr, f_aexpr), body)))
  | LLLetIn (id, e1, e2) ->
    anf_llexpr e1 (fun imm ->
      let* ae = anf_llexpr e2 expr_with_hole in
      return @@ ALetIn (id, CImm imm, ae))
  | LLApp _ ->
    let e, args = fold_app e in
    anf_llexpr e (fun imm ->
      anf_many args (fun imms ->
        let* name = gen_name in
        let* ae = expr_with_hole (ImmVar name) in
        let app = CApp (imm, imms) in
        return @@ ALetIn (name, app, ae)))
  | LLMatch _ -> fail "Match is not implemented yet"
;;

let anf_decl = function
  | LLDLet (is_rec, id, args, e) ->
    let* e = anf_llexpr e (fun imm -> return @@ ACExpr (CImm imm)) in
    return @@ ADLet (is_rec, id, args, e)
  | LLDMutualLet _ -> fail "Not implemented"
;;

let anf_program prog =
  run
  @@ List.fold_right prog ~init:(return []) ~f:(fun decl acc ->
    let* acc = acc in
    let* decl = anf_decl decl in
    return (decl :: acc))
;;
