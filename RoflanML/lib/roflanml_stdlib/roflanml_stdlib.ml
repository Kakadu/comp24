(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Typing

module RoflanML_Stdlib = struct
  let default =
    let stdlib = Map.empty (module String) in
    let stdlib =
      List.fold [ "+"; "-"; "*"; "/" ] ~init:stdlib ~f:(fun stdlib op ->
        Map.update stdlib op ~f:(fun _ ->
          TArrow (TBase BInt, TArrow (TBase BInt, TBase BInt))))
    in
    let stdlib =
      List.fold [ "="; "<>"; ">"; ">="; "<"; "<=" ] ~init:stdlib ~f:(fun stdlib op ->
        Map.update stdlib op ~f:(fun _ -> TArrow (TVar 0, TArrow (TVar 0, TBase BBool))))
    in
    let stdlib =
      List.fold
        [ "print_int", BInt; "print_bool", BBool ]
        ~init:stdlib
        ~f:(fun stdlib (op, ty) ->
          Map.update stdlib op ~f:(fun _ -> TArrow (TBase ty, TBase BUnit)))
    in
    stdlib
  ;;
end
