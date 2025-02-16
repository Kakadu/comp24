(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let dbg fmt =
  if Sys.getenv_opt "DEBUG" |> Option.value ~default:"false" |> bool_of_string
  then Format.printf fmt
  else Format.ifprintf Format.std_formatter fmt
;;

open Base

let pp_list ?(op = "(") ?(cl = ")") ?(sep = ", ") fmt pp_inner list =
  let open Format in
  fprintf fmt "%s" op;
  pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "%s" sep) pp_inner fmt list;
  fprintf fmt "%s" cl
;;

let set_to_string xs =
  xs |> Set.to_list |> String.concat ~sep:", " |> Format.asprintf "{%s}"
;;

let list_to_string xs = xs |> String.concat ~sep:"; " |> Format.asprintf "[%s]"

let foldi_right list ~init ~f =
  let len = List.length list - 1 in
  snd (List.fold_right list ~init:(len, init) ~f:(fun v (i, acc) -> i - 1, f i acc v))
;;

let index_list list = List.mapi list ~f:(fun i v -> i, v)
