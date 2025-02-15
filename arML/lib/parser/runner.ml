(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Angstrom

let parse_expression input =
  parse_string
    ~consume:All
    (many1 (skip_wspace *> Expression.parse_expression <* skip_wspace))
    input
;;

let parse_program input =
  let sep = option () (skip_wspace *> string ";;" *> return ()) in
  parse_string
    ~consume:All
    (many1 (Declaration.parse_declaration <* sep <* skip_wspace))
    input
;;
