(** Copyright 2023-2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Ast
open Angstrom
open Utils

let rec_remove_parents m = fix (fun t -> remove_parents t <|> m)


let parse_ident =
  let* name = parse_ident_name in
  
;;
