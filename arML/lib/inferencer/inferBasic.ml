(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open Ast.AbstractSyntaxTree
open TypeTree
open TypeUtils

(* Get a new type variable *)
let fresh_var = fresh >>| fun name -> TVar name

let infer_const = function
  | CInt _ -> TGround GTInt
  | CBool _ -> TGround GTBool
  | CChar _ -> TGround GTChar
  | CString _ -> TGround GTString
  | CUnit -> TGround GTUnit
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
  | GTDInt -> GTInt
  | GTDBool -> GTBool
  | GTDChar -> GTChar
  | GTDString -> GTString
  | GTDUnit -> GTUnit
;;

let rec get_type_by_defenition = function
  | TDGround td -> return @@ TGround (get_ground_type_by_defenition td)
  | TDArrow (l, r) ->
    let* l_ty = get_type_by_defenition l in
    let* r_ty = get_type_by_defenition r in
    return (l_ty @-> r_ty)
  | TDTuple (fst, snd, other) ->
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
  | TDList ty ->
    let* ty' = get_type_by_defenition ty in
    return (TList ty')
  | TDPolymorphic _ ->
    let* fv = fresh_var in
    return fv

(* ---------------- *)
