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

module MonadCounter : sig
  type 'a t

  val return : 'a -> 'a t
  val fresh : int t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  include Base.Monad.Infix with type 'a t := 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> int -> int * 'a

  module RList : sig
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end
end = struct
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
