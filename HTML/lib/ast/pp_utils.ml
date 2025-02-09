(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

let pp_el parens_pred pp_e fmt e =
  let s =
    match parens_pred e with
    | true -> format_of_string "(%a)"
    | false -> format_of_string "%a"
  in
  fprintf fmt s pp_e e
;;

let pp_list parens_pred pp_e fmt delimiter =
  pp_print_list
    ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
    (fun fmt value -> pp_el parens_pred pp_e fmt value)
    fmt
;;

let pp_tuple parens_pred pp_e delim fmt value_list =
  pp_list parens_pred pp_e fmt delim value_list
;;
