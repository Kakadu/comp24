(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open TypeTree
open TypeUtils

(* Get a new type variable *)
let fresh_var = fresh >>| fun name -> TVar name

let infer_const = function
  | Ast.CInt _ -> TGround GTInt
  | Ast.CBool _ -> TGround GTBool
  | Ast.CChar _ -> TGround GTChar
  | Ast.CString _ -> TGround GTString
  | Ast.CUnit -> TGround GTUnit
;;

let infer_id env id =
  (* '_' - reserved for expressions whose result is not important to us. *)
  match id with
  | "_" ->
    let* fv = fresh_var in
    return (Substitution.empty, fv)
  | _ -> TypeEnv.lookup_env env id
;;

(* Type definition to real type mapping *)

let get_ground_type_by_defenition = function
  | Ast.GTDInt -> GTInt
  | Ast.GTDBool -> GTBool
  | Ast.GTDChar -> GTChar
  | Ast.GTDString -> GTString
  | Ast.GTDUnit -> GTUnit
;;

let rec get_type_by_defenition = function
  | Ast.TDGround td -> return @@ TGround (get_ground_type_by_defenition td)
  | Ast.TDArrow (l, r) ->
    let* l_ty = get_type_by_defenition l in
    let* r_ty = get_type_by_defenition r in
    return (l_ty @-> r_ty)
  | Ast.TDTuple (fst, snd, other) ->
    let* fst_ty = get_type_by_defenition fst in
    let* snd_ty = get_type_by_defenition snd in
    let rec process_others acc = function
      | [] -> return acc
      | x :: xs ->
        let* x_ty = get_type_by_defenition x in
        process_others (x_ty :: acc) xs
    in
    let* other_tys = process_others [] other in
    return (TTuple (fst_ty :: snd_ty :: List.rev other_tys))
  | Ast.TDList ty ->
    let* ty' = get_type_by_defenition ty in
    return (TList ty')
  | Ast.TDPolymorphic _ ->
    let* fv = fresh_var in
    return fv

(* ---------------- *)
