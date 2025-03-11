[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

(** Identifiers *)
module Id = struct
  module T = struct
    type t = I of string
    [@@deriving show {with_path= false}, ord, sexp_of, eq]
  end

  include T
  include Comparator.Make (T)
end

(** List containing at least 1 element *)
module List1 = struct
  type 'a t = 'a * 'a list [@@deriving show {with_path= false}]

  let of_list_exn : 'a list -> 'a t = function
    | hd :: tl ->
        (hd, tl)
    | [] ->
        raise (Invalid_argument "empty list")

  let to_list : 'a t -> 'a list = fun (hd, tl) -> hd :: tl

  let map ((hd, tl) : 'a t) ~(f : 'a -> 'b) : 'b t = (f hd, List.map tl ~f)

  let cons (hd : 'a) ((fst, snd) : 'a t) : 'a t = (hd, fst :: snd)
end

(** List containing at least 2 elements *)
module List2 = struct
  type 'a t = 'a * 'a * 'a list
  [@@deriving show {with_path= false}, ord, sexp_of, eq]

  let of_list_exn : 'a list -> 'a t = function
    | fst :: snd :: tl ->
        (fst, snd, tl)
    | _ :: [] | [] ->
        raise (Invalid_argument "not enough elements")

  let to_list : 'a t -> 'a list = fun (fst, snd, tl) -> fst :: snd :: tl

  let map ((fst, snd, tl) : 'a t) ~(f : 'a -> 'b) : 'b t =
    (f fst, f snd, List.map tl ~f)
end

(* https://ocaml.org/manual/5.0/lex.html#sss:lex-ops-symbols *)
let is_op_first_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '<' | '>' | '@' | '^' | '|' | '%'
    ->
      true
  | _ ->
      false

let is_op_char = function
  | ch when is_op_first_char ch ->
      true
  | '~' | '!' | '?' | ':' | '.' ->
      true
  | _ ->
      false

module type MONAD = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t

  val return : 'a -> 'a t
end

(** Makes State-Error monad *)
module MakeSEMonad (StateT : T) (ErrorT : T) : sig
  include MONAD

  val run : 'a t -> StateT.t -> StateT.t * ('a, ErrorT.t) Result.t
  val fail : ErrorT.t -> 'a t

  val put : StateT.t -> unit t
  val get : StateT.t t
end = struct
  type 'a t = StateT.t -> StateT.t * ('a, ErrorT.t) Result.t

  let run m = m
  let return x st = (st, Ok x)
  let fail err st = (st, Error err)

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) st =
    let st, x = m st in
    match x with Ok x -> f x st | Error err -> fail err st

  let ( let* ) = ( >>= )

  let ( >>| ) m f = m >>= fun x -> return (f x)
  let ( *> ) m1 m2 = m1 >>= fun _ -> m2

  let put st _ = (st, Ok ())
  let get st = (st, Ok st)
end
