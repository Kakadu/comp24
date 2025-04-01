(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Pe_ast
open Common

module StateMonad : sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

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
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end

open StateMonad


let const_to_pe_const = function
  | CInt a -> Pe_Cint a
  | CBool a -> Pe_CBool a

let rec expr_to_mexpr = function
  | EUnit -> return @@ Pe_EUnit
  | ENill -> return @@ Pe_ENill
  | EConstraint (a, _) -> (expr_to_mexpr a)
  | EConst a -> return @@ Pe_EConst (const_to_pe_const a)
  | EIdentifier a -> return @@ Pe_EIdentifier a
  | EApplication (a, b) -> 
    let* a = expr_to_mexpr a in
    let* b = expr_to_mexpr b in 
    return @@ Pe_EApp (a, b)
  | _ -> failwith "not impl"
  