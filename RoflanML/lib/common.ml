(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

module Counter_Monad = struct
  type ('a, 'err) t = int -> int * ('a, 'err) Result.t

  let return : 'a -> ('a, 'err) t = fun x st -> st, Result.Ok x
  let fail : 'err -> ('a, 'err) t = fun err st -> st, Result.Error err

  let ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t =
    fun m f st ->
    let st, r = m st in
    match r with
    | Result.Ok a -> f a st
    | Result.Error err -> fail err st
  ;;

  let ( let* ) = ( >>= )
  let fresh st = st + 1, Result.Ok st
  let run m = snd (m 0)
end

let gen_name prefix =
  let open Counter_Monad in
  let fresh_id = fresh >>= fun x -> return (Int.to_string x) in
  let* id = fresh_id in
  return (prefix ^ "_" ^ id)
;;

module Middleend_Common = struct
  open Ast

  let uncurry e =
    let rec helper args = function
      | EFun (x, e) -> helper (x :: args) e
      | e -> args, e
    in
    let args, e = helper [] e in
    List.rev args, e
  ;;

  let curry args e = Base.List.fold_right args ~init:e ~f:(fun arg acc -> EFun (arg, acc))
end
