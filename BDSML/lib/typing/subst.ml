(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Types
open Monads
module VarMap = Map.Make (TVarId)

type t = type_val VarMap.t

let fold_left (map : t) init f =
  VarMap.fold
    (fun key data acc ->
      let* acc = acc in
      f acc (key, data))
    map
    init
;;

let empty : t = VarMap.empty
let mapping (k : TVarId.t) v = if occurs_in k v then fail Occurs_check else return (k, v)

let singleton k v : t Monads.t =
  let+ k, v = mapping k v in
  VarMap.singleton k v
;;

let apply (sub : t) =
  let rec helper = function
    | TVar b as ty ->
      (match VarMap.find_opt b sub with
       | None -> ty
       | Some x -> x)
    | TArrow (l, r) -> TArrow (helper l, helper r)
    | TTuple m -> TTuple (List.map helper m)
    | TConstructor (Some t, name) -> TConstructor (Some (helper t), name)
    | _ as x -> x
  in
  helper
;;

let rec unify l r =
  match l, r with
  | TBase a, TBase b when a = b -> return empty
  | TVar a, TVar b when a = b -> return empty
  | TVar b, t | t, TVar b -> singleton b t
  | TArrow (l1, r1), TArrow (l2, r2) ->
    let* subs1 = unify l1 l2 in
    let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
    compose subs1 subs2
  | TConstructor (Some t1, _), TConstructor (Some t2, _) -> unify t1 t2
  | TConstructor _, TConstructor _ -> return empty
  | TUnit, TUnit -> return empty
  | TTuple l, TTuple r ->
    Monads.fold_left
      (fun sub (l, r) ->
        let* sub2 = unify l r in
        compose sub sub2)
      (return empty)
    @@ List.combine l r
  | _ -> fail (Unification_failed (l, r))

and extend s (k, v) =
  match VarMap.find_opt k s with
  | None ->
    let v = apply s v in
    let* acc_map = singleton k v in
    fold_left s (return acc_map) (fun acc (k, v) ->
      let v = apply acc_map v in
      let+ k, v = mapping k v in
      VarMap.add k v acc)
  | Some v2 ->
    let* s2 = unify v v2 in
    compose s s2

and compose s1 s2 = fold_left s2 (return s1) extend

let compose_all ss =
  List.fold_left
    (fun acc ss ->
      let* acc = acc in
      compose acc ss)
    (return empty)
    ss
;;

let remove var (sub : t) : t = VarMap.remove var sub
