open Monads
open Helpers
open Types
module VarMap = Map.Make (VarId)

type t = type_val VarMap.t

let fold_left (map : t) init f =
  VarMap.fold
    (fun key data acc ->
      let* acc = acc in
      f acc (key, data))
    map
    init
;;

let empty = VarMap.empty
let mapping k v = if occurs_in k v then fail Occurs_check else return (k, v)

let singleton k v =
  let+ k, v = mapping k v in
  VarMap.singleton k v
;;

(** substitute type of var instead of var *)
let apply map =
  let rec helper = function
    | TVar b as ty ->
      (match VarMap.find_opt b map with
       | None -> ty
       | Some x -> x)
    | TArrow (l, r) -> TArrow (helper l, helper r)
    | TBase _ as x -> x
    | TTuple m -> TTuple (List.map helper m)
    | _ -> raise (Unimplemented "apply")
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
    compose_maps subs1 subs2
  | _ -> fail (Unification_failed (l, r))

(** add type to map *)
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
    compose_maps s s2

and compose_maps s1 s2 = fold_left s2 (return s1) extend

let compose_all ss =
  List.fold_left
    (fun acc ss ->
      let* acc = acc in
      compose_maps acc ss)
    (return empty)
    ss
;;
