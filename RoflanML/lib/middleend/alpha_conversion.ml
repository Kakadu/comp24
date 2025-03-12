(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Common
open Common.Counter_Monad
open Roflanml_stdlib

let gen_name = gen_name "alpha"

let rec find_vars_pattern pat =
  match pat with
  | PWild | PEmpty | PConst _ -> Set.empty (module String)
  | PVar x -> Set.singleton (module String) x
  | PCons (p1, p2, ps) | PTuple (p1, p2, ps) ->
    Set.union_list
      (module String)
      [ find_vars_pattern p1
      ; find_vars_pattern p2
      ; List.fold
          ps
          ~init:(Set.empty (module String))
          ~f:(fun acc p -> Set.union acc (find_vars_pattern p))
      ]
    (* According to OCaml rules, all patterns in POr must have same variables set *)
  | POr (p1, _, _) -> find_vars_pattern p1
;;

let alpha_convert =
  let rec alpha_convert_pattern p renames =
    match p with
    | PWild -> PWild
    | PEmpty -> PEmpty
    | PConst _ as p -> p
    | PVar id ->
      (match Map.find renames id with
       | Some id -> PVar id
       | None -> p)
    | PCons (p1, p2, ps) ->
      let p1 = alpha_convert_pattern p1 renames in
      let p2 = alpha_convert_pattern p2 renames in
      let ps = List.map ps ~f:(fun p -> alpha_convert_pattern p renames) in
      PCons (p1, p2, ps)
    | POr (p1, p2, ps) ->
      let p1 = alpha_convert_pattern p1 renames in
      let p2 = alpha_convert_pattern p2 renames in
      let ps = List.map ps ~f:(fun p -> alpha_convert_pattern p renames) in
      POr (p1, p2, ps)
    | PTuple (p1, p2, ps) ->
      let p1 = alpha_convert_pattern p1 renames in
      let p2 = alpha_convert_pattern p2 renames in
      let ps = List.map ps ~f:(fun p -> alpha_convert_pattern p renames) in
      PTuple (p1, p2, ps)
  in
  let rec alpha_convert_expr e bindings renames =
    match e with
    | EConst _ as e -> return e
    | EVar id ->
      (match Map.find renames id with
       | Some id -> return (EVar id)
       | None -> return e)
    | ETuple (e1, e2, es) ->
      let* e1 = alpha_convert_expr e1 bindings renames in
      let* e2 = alpha_convert_expr e2 bindings renames in
      let* es =
        List.fold_right es ~init:(return []) ~f:(fun e acc ->
          let* acc = acc in
          let* e = alpha_convert_expr e bindings renames in
          return (e :: acc))
      in
      return (ETuple (e1, e2, es))
    | EList es ->
      let* es =
        List.fold_right es ~init:(return []) ~f:(fun e acc ->
          let* acc = acc in
          let* e = alpha_convert_expr e bindings renames in
          return (e :: acc))
      in
      return (EList es)
    | EBranch (cond, t, f) ->
      let* cond = alpha_convert_expr cond bindings renames in
      let* t = alpha_convert_expr t bindings renames in
      let* f = alpha_convert_expr f bindings renames in
      return (EBranch (cond, t, f))
    | EMatch (e, cases) ->
      let* e = alpha_convert_expr e bindings renames in
      let* cases =
        List.fold_right cases ~init:(return []) ~f:(fun (p, e) acc ->
          let* acc = acc in
          let vars = find_vars_pattern p in
          let* bindings, renames =
            Set.fold
              vars
              ~init:(return (bindings, renames))
              ~f:(fun acc var ->
                let* bindings, renames = acc in
                if Set.mem bindings var
                then
                  let* new_name = gen_name bindings in
                  return
                    ( Set.add bindings new_name
                    , Map.update renames var ~f:(fun _ -> new_name) )
                else return (bindings, renames))
          in
          let p = alpha_convert_pattern p renames in
          let* e = alpha_convert_expr e bindings renames in
          return ((p, e) :: acc))
      in
      return (EMatch (e, cases))
    | ELetIn (is_rec, id, e1, e2) ->
      let* new_id, new_renames =
        if Set.mem bindings id
        then
          let* new_name = gen_name bindings in
          return (new_name, Map.update renames id ~f:(fun _ -> new_name))
        else return (id, renames)
      in
      let bindings, renames =
        match is_rec with
        | Rec -> Set.add bindings id, new_renames
        | NonRec -> bindings, renames
      in
      let* e1 = alpha_convert_expr e1 bindings renames in
      let bindings, renames = Set.add bindings id, new_renames in
      let* e2 = alpha_convert_expr e2 bindings renames in
      return (ELetIn (is_rec, new_id, e1, e2))
    | EFun ((id, ty), e) ->
      let* id, renames =
        if Set.mem bindings id
        then
          let* new_name = gen_name bindings in
          return (new_name, Map.update renames id ~f:(fun _ -> new_name))
        else return (id, renames)
      in
      let bindings = Set.add bindings id in
      let* e = alpha_convert_expr e bindings renames in
      return (EFun ((id, ty), e))
    | EApp (e1, e2) ->
      let* e1 = alpha_convert_expr e1 bindings renames in
      let* e2 = alpha_convert_expr e2 bindings renames in
      return (EApp (e1, e2))
  in
  let rec alpha_convert_decl decl bindings renames =
    match decl with
    | DLet (is_rec, id, e) ->
      let* new_id, new_renames =
        if Set.mem bindings id
        then
          let* new_name = gen_name bindings in
          return (new_name, Map.update renames id ~f:(fun _ -> new_name))
        else return (id, renames)
      in
      let bindings, renames =
        match is_rec with
        | Rec -> Set.add bindings id, new_renames
        | NonRec -> bindings, renames
      in
      let* e = alpha_convert_expr e bindings renames in
      return (DLet (is_rec, new_id, e), new_renames)
    | DMutualLet (_, decls) ->
      let* decls, renames =
        List.fold_right
          decls
          ~init:(return ([], renames))
          ~f:(fun (id, e) acc ->
            let* acc, renames = acc in
            let* decl, renames =
              alpha_convert_decl (DLet (Rec, id, e)) bindings renames
            in
            match decl with
            | DLet (_, id, e) -> return ((id, e) :: acc, renames)
            | _ -> fail "Unreachable")
      in
      return (DMutualLet (Rec, decls), renames)
  in
  alpha_convert_decl
;;

let alpha_convert_program prog =
  run
  @@
  let bindings = RoflanML_Stdlib.default |> Map.keys |> Set.of_list (module String) in
  let bindings = Set.add bindings "_start" in
  let renames = Map.empty (module String) in
  let* prog, _, _ =
    List.fold_left
      prog
      ~init:(return ([], bindings, renames))
      ~f:(fun acc decl ->
        let* acc, bindings, renames = acc in
        let* decl, renames = alpha_convert decl bindings renames in
        let bindings =
          match decl with
          | DLet (_, id, _) -> Set.add bindings id
          | DMutualLet (_, decls) ->
            List.fold decls ~init:bindings ~f:(fun acc (id, _) -> Set.add acc id)
        in
        return (decl :: acc, bindings, renames))
  in
  return (List.rev prog)
;;
