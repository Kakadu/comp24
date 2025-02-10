(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open TypeTree
open TypeErrors

(* A substitution is an map, where the first element of each list element is what needs to be replaced,
    the second is what it should be replaced with. *)
type t = (int, typ, Base.Int.comparator_witness) Base.Map.t

let empty = Base.Map.empty (module Base.Int)

let singleton k v =
  if OccursChecker.occurs_check k v
  then fail Occurs_check
  else return (Base.Map.singleton (module Base.Int) k v)
;;

let find sub k = Base.Map.find sub k
let remove sub k = Base.Map.remove sub k

(* Replace all type variables in a type with values ​​from the substitution. *)
let apply sub =
  let rec helper = function
    | TVar n ->
      (match find sub n with
       | None -> TVar n
       | Some v -> v)
    | TArr (left, right) -> TArr ((helper left), (helper right))
    | TList typ -> TList (helper typ)
    | TTuple t_list -> TTuple (Base.List.map t_list ~f:helper)
    | other -> other
  in
  helper
;;

(* Try to unify two types into a single type. *)
let rec unify l r =
  match l, r with
  | TGround l, TGround r when l = r -> return empty
  | TVar a, TVar b when a = b -> return empty
  | TVar a, t | t, TVar a -> singleton a t
  | TArr (left1, right1), TArr (left2, right2) ->
    let* sub1 = unify left1 left2 in
    let* sub2 = unify (apply sub1 right1) (apply sub1 right2) in
    compose sub1 sub2
  | TList typ1, TList typ2 -> unify typ1 typ2
  | TTuple t_list1, TTuple t_list2 ->
    (match
       Base.List.fold2 t_list1 t_list2 ~init:(return empty) ~f:(fun acc it1 it2 ->
           let* sub1 = acc in
           let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
           compose sub1 sub2)
     with
     | Ok r -> r
     | _ -> fail (Unification_failed (l, r)))
  | _ -> fail (Unification_failed (l, r))

(* Expanding the substitution with a new key-value. *)
and extend k v sub =
  match find sub k with
  | None ->
    let v = apply sub v in
    let* new_sub = singleton k v in
    let f1 ~key ~data acc =
      let* acc = acc in
      let new_data = apply new_sub data in
      return (Base.Map.update acc key ~f:(fun _ -> new_data))
    in
    Base.Map.fold sub ~init:(return new_sub) ~f:f1
  | Some vl ->
    let* new_sub = unify v vl in
    compose sub new_sub

(* Two substitution's composition. *)
and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

(* Composition of an arbitrary number of substitutions. *)
let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
