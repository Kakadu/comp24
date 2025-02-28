(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base

module StrSet = struct
  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst

  let find s str =
    match Set.binary_search s ~compare:String.compare `First_equal_to str with
    | Some _ -> true
    | None -> false
  ;;

  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end

module MonadCounter = struct
  type 'a t = int -> int * 'a

  let return x var = var, x
  let fresh var = var + 1, var

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun var ->
    let var, x = m var in
    f x var
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun var ->
    let var, x = m var in
    var, f x
  ;;

  let run m start = m start

  module RList = struct
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
  end
end

module MonadCounterError = struct
  open Base

  type ('a, 'e) t = int -> int * ('a, 'e) Result.t

  let return x var = var, Result.return x
  let fail e var = var, Result.fail e
  let fresh var = var + 1, Result.return var

  let ( >>= ) (m : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    fun var ->
    match m var with
    | var, Result.Error err -> var, Result.fail err
    | var, Result.Ok x -> f x var
  ;;

  let bind = ( >>= )

  let ( >>| ) (m : ('a, 'e) t) (f : 'a -> 'b) : ('b, 'e) t =
    fun var ->
    match m var with
    | var, Result.Error err -> var, Result.fail err
    | var, Result.Ok x -> var, Result.return (f x)
  ;;

  let ( let* ) = bind

  module RMap = struct
    let fold mp ~init ~f =
      Map.fold mp ~init ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        f acc k v)
    ;;
  end

  module RList = struct
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;

    let map (xs : 'a list) ~(f : 'a -> ('b, 'e) t) : ('b list, 'e) t =
      let* xs =
        List.fold xs ~init:(return []) ~f:(fun acc x ->
          let* acc = acc in
          let* x = f x in
          return (x :: acc))
      in
      return @@ List.rev xs
    ;;
  end

  let run init m = snd (m init)
end
