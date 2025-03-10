(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ll_ast
open Anf_ast
open Common
open Common.Counter_Monad

let gen_name = gen_name "anf"

let get_bindings prog =
  List.fold
    prog
    ~init:(Set.empty (module String))
    ~f:(fun acc decl ->
      match decl with
      | LLDLet (_, id, _, _) -> Set.add acc id
      | LLDMutualLet decls ->
        List.fold decls ~init:acc ~f:(fun acc (id, _, _) -> Set.add acc id))
;;

let fold_app e =
  let rec helper e k =
    match e with
    | LLApp (l, r) -> helper l (fun args -> k (r :: args))
    | e -> e, List.rev (k [])
  in
  helper e (fun x -> x)
;;

let anf_llexpr e expr_with_hole bindings =
  let gen_name = gen_name bindings in
  let rec helper e expr_with_hole =
    match e with
    | LLConst c -> expr_with_hole (ImmConst c)
    | LLVar id -> expr_with_hole (ImmVar id)
    | LLTuple (e1, e2, es) ->
      helper e1 (fun imm1 ->
        helper e2 (fun imm2 ->
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
      helper cond (fun imm ->
        let* name = gen_name in
        let* body = expr_with_hole (ImmVar name) in
        let* t_aexpr = helper t (fun imm -> return @@ ACExpr (CImm imm)) in
        let* f_aexpr = helper f (fun imm -> return @@ ACExpr (CImm imm)) in
        return (ALetIn (name, CBranch (imm, t_aexpr, f_aexpr), body)))
    | LLLetIn (id, e1, e2) ->
      helper e1 (fun imm ->
        let* ae = helper e2 expr_with_hole in
        return @@ ALetIn (id, CImm imm, ae))
    | LLApp _ ->
      let e, args = fold_app e in
      helper e (fun imm ->
        anf_many args (fun imms ->
          let* name = gen_name in
          let* ae = expr_with_hole (ImmVar name) in
          let app = CApp (imm, imms) in
          return @@ ALetIn (name, app, ae)))
    | LLMatch _ -> fail "Match is not implemented yet"
  and anf_many es k =
    let rec fold acc = function
      | h :: tl -> helper h (fun imm -> fold (imm :: acc) tl)
      | [] -> k (List.rev acc)
    in
    fold [] es
  in
  helper e expr_with_hole
;;

let rec anf_decl decl bindings =
  match decl with
  | LLDLet (is_rec, id, args, e) ->
    let* e = anf_llexpr e (fun imm -> return @@ ACExpr (CImm imm)) bindings in
    return @@ ADLet (is_rec, id, args, e)
  | LLDMutualLet decls ->
    let* decls =
      List.fold_right decls ~init:(return []) ~f:(fun (id, args, e) decls ->
        let* decls = decls in
        let* decl = anf_decl (LLDLet (Rec, id, args, e)) bindings in
        match decl with
        | ADLet (_, id, args, e) -> return ((id, args, e) :: decls)
        | _ -> fail "Unreachable")
    in
    return (ADMutualLet decls)
;;

let anf_program prog =
  run
  @@ List.fold_right prog ~init:(return []) ~f:(fun decl acc ->
    let* acc = acc in
    let* decl = anf_decl decl (get_bindings prog) in
    return (decl :: acc))
;;
