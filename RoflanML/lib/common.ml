(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Base

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
