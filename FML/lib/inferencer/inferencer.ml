(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* open Ast *)
open Typedtree
open Inf_errors

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> f v s
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> s, Base.Result.return @@ f v
  ;;

  let return v last = last, Base.Result.return v
  let fail e state = state, Base.Result.fail e
  let bind x ~f = x >>= f
  let fresh last = last + 1, Result.Ok last (* Get new state *)

  module Syntax = struct
    let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)
  end

  module RMap = struct
    (* Classic map folding. *)
    let fold_left mp ~init ~f =
      let open Syntax in
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    (* Classic list folding. *)
    let fold_left lt ~init ~f =
      let open Syntax in
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TFunction (l, r) -> occurs_in v l || occurs_in v r
    | TList t -> occurs_in v t
    | TTuple ts -> List.fold_left (fun acc item -> acc || occurs_in v item) false ts
    | _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar b -> TVarSet.add b acc
      | TFunction (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple ts -> List.fold_left helper acc ts
      | TBool | TInt -> acc
    in
    helper TVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val find : t -> int -> typ option
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  (* A substitution is an map, where the first element of each list element is what needs to be replaced,
     the second is what it should be replaced with. *)
  type t = (int, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  (* Creates a pair if no error occurs. *)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find sub k = Base.Map.find sub k
  let remove sub k = Base.Map.remove sub k

  (* Replace all type variables in a type with values ​​from the substitution. *)
  let apply sub =
    let rec helper = function
      | TVar n ->
        (match find sub n with
         | None -> tvar n
         | Some v -> v)
      | TFunction (l, r) -> tfunction (helper l) (helper r)
      | TList typ -> tlist (helper typ)
      | TTuple t_list -> ttuple (Base.List.map t_list ~f:helper)
      | other -> other
    in
    helper
  ;;

  (* Try to unify two types into a single type. *)
  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TFunction (left1, right1), TFunction (left2, right2) ->
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
       | _ -> fail (`Unification_failed (l, r)))
    | _ -> fail (`Unification_failed (l, r))

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
end
