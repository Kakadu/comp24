(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common
open Expr
open Pattern
open Ty

(**
  [let P1 = E1 and P2 = E2 and ...]
  [let rec ValName1 PArg1 = E1 and P1 = E2 and ...]
*)
let parse_str_let =
  parse_let_binding parse_expression parse_pattern parse_ty
  >>| fun (rec_flag, bindings) -> Str_value (rec_flag, bindings)

(** [('a, 'b, 'c)] *)
let parse_type_params =
  let comma = ws *> char ',' *> ws in
  let parse_multiple =
    char '(' *> ws *> sep_by1 comma parse_type_var <* ws <* char ')'
  in
  let parse_single = parse_type_var >>| fun var -> [var] in
  parse_single <|> parse_multiple

(** [exception Some_exc of string] *)

let parse_structure : structure t =
  let parse_structure_item =
    (*
       XXX: parsing of let structure items seems to require 2 passes.
       first it tries to parse let expression, fails when doesn't find `in`,
       then starts all over again trying to parse let structure item.

       we probably should use lookahead to check for `in`
    *)
    ws *> choice [(parse_expression >>| fun e -> Str_eval e); parse_str_let]
  in
  let semicolons = ws *> option () (string ";;" *> return ()) in
  sep_by semicolons parse_structure_item <* semicolons <* ws
