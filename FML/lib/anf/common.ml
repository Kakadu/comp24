(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base

module StrMap = struct
  type 'a t = (string, 'a, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let singleton str = Map.singleton (module String) str
  let add = Map.add
  let update = Map.update
  let find = Map.find
  let merge_two fst snd = Map.merge_skewed fst snd ~combine:(fun ~key:_ _ v2 -> v2)
end

let builtins =
  [ "( + )"
  ; "( - )"
  ; "( > )"
  ; "( <= )"
  ; "( && )"
  ; "( / )"
  ; "( * )"
  ; "( < )"
  ; "( >= )"
  ; "( <> )"
  ; "( = )"
  ; "( != )"
  ; "( || )"
  ; "not"
  ; "print_int"
  ; "tuple_get"
  ; "is_empty"
  ; "is_cons"
  ; "hd_list_get"
  ; "tl_list_get"
  ; "fail_match"
  ; "_start"
  ]
;;

module StrSet = struct
  open Base

  type t = (string, String.comparator_witness) Set.t

  let add = Set.add
  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
  let union_list lst = Set.union_list (module String) lst
  let find = Set.mem
end

module StateMonad : sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> 'a
end = struct
  type 'a t = int -> int * 'a (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    let s', v' = m s in
    f v' s'
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    let s', x = m s in
    s', f x
  ;;

  let return v last = last, v
  let bind x ~f = x >>= f
  let fresh last = last + 1, last (* Get new state *)
  let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)

  module RMap = struct
    (* Classic map folding. *)
    let fold_left mp ~init ~f =
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    (* Classic list folding. *)
    let fold_left lt ~init ~f =
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;

    let fold_right lt ~init ~f =
      Base.List.fold_right lt ~init ~f:(fun item acc ->
        let* acc = acc in
        f item acc)
    ;;
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end
