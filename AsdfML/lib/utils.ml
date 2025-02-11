(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let dbg fmt =
  if Sys.getenv_opt "DEBUG" |> Option.value ~default:"false" |> bool_of_string
  then Format.printf fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let pp_list ?(op = "(") ?(cl = ")") ?(sep = ", ") fmt pp_inner list =
  let open Format in
  fprintf fmt "%s" op;
  pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "%s" sep) pp_inner fmt list;
  fprintf fmt "%s" cl
;;
