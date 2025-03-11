(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Pe_ast

module StrMap = struct
  type 'a t = (string, 'a, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let singleton str = Map.singleton (module String) str
  let find m str = Map.find m str
  let add = Map.add
  let update = Map.update
  let merge_two fst snd = Map.merge_skewed fst snd ~combine:(fun ~key:_ _ v2 -> v2)
end

let builtins =
  [ "( + )"
  ; "( - )"
  ; "( / )"
  ; "( * )"
  ; "( < )"
  ; "( > )"
  ; "( <= )"
  ; "( >= )"
  ; "( <> )"
  ; "( = )"
  ; "( != )"
  ; "( && )"
  ; "( || )"
  ; "not"
  ; "print_int"
  ; "#list_head"
  ; "#list_tail"
  ; "#tuple_element"
  ; "#is_empty"
  ; "#fail_match"
  ]
;;

let make_apply op expr1 expr2 = Pe_EApp (Pe_EApp (Pe_EIdentifier op, expr1), expr2)

module StrSet = struct
  open Base

  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst
  let find s str = Set.mem s str
  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end

type bindings = (int, Int.comparator_witness) Set.t

let contains ng id =
  match Set.find ng ~f:(Int.equal id) with
  | Some _ -> true
  | None -> false
;;

module MonadCounter = struct
  open Base

  type 'a t = bindings * int -> bindings * int * 'a

  let return x (binds, var) = binds, var, x

  let fresh (binds, var) =
    let rec helper num = if contains binds num then helper (num + 1) else num in
    let next = helper var in
    binds, next + 1, next
  ;;

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun t ->
    let binds, var, x = m t in
    f x (binds, var)
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun t ->
    let binds, var, x = m t in
    binds, var, f x
  ;;

  let run (m : 'a t) binds start = m (binds, start)

  let map (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let* xs =
      List.fold xs ~init:(return []) ~f:(fun acc x ->
        let* acc = acc in
        let* x = f x in
        return (x :: acc))
    in
    return @@ List.rev xs
  ;;

  let fold_left (xs : 'a list) ~(init : 'b t) ~(f : 'b -> 'a -> 'b t) : 'b t =
    List.fold xs ~init ~f:(fun acc x ->
      let* acc = acc in
      f acc x)
  ;;

  let fold_right xs ~init ~f =
    List.fold_right xs ~init ~f:(fun x acc ->
      let* acc = acc in
      f x acc)
  ;;
end

let rec get_binds_pat = function
  | PConstraint (pat, _) -> get_binds_pat pat
  | PAny | PConst _ | PNill | PUnit -> StrSet.empty
  | PIdentifier ident -> StrSet.singleton ident
  | PCons (p1, p2) -> StrSet.union (get_binds_pat p1) (get_binds_pat p2)
  | PTuple pl ->
    Base.List.fold pl ~init:StrSet.empty ~f:(fun acc p ->
      StrSet.union acc (get_binds_pat p))
;;

let make_condition checks e1 e2 =
  let cond =
    List.fold (List.tl_exn checks) ~init:(List.hd_exn checks) ~f:(fun acc a ->
      make_apply "( && )" acc a)
  in
  Pe_EIf (cond, e1, e2)
;;

let get_id i = "a" ^ Int.to_string i
let empty = Base.Map.empty (module Base.String)
