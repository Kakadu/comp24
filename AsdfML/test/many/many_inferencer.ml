(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Test.Utils

let () =
  let s = In_channel.input_all Stdlib.stdin in
  infer s (Format.printf "%a" Lib.Tast.pp_toplevel_types)
;;
