[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle

module Format = Stdlib.Format

type err = ParseError | SimplError of MSimpl.err

let run' (ir : [`Simpl | `SimplOpt | `CLess | `Anf]) s =
  let globals =
    IdSet.of_list
      [I "+"; I "-"; I "*"; I "/"; I "<"; I "<="; I "="; I "print_int"]
  in

  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseError) ~f:return
  in

  let* sim =
    MSimpl.from_structure structure |> map_error ~f:(fun err -> SimplError err)
  in
  let opt = MOpt.opt sim in
  let cls = MCLess.from_simpl ~globals opt in

  let print = PPrint.ToChannel.pretty 1. 50 stdout in
  ( match ir with
  | `Simpl ->
      LPrint.pp_expr (MSimpl.to_expr sim) |> print
  | `SimplOpt ->
      LPrint.pp_expr (MSimpl.to_expr opt) |> print
  | `CLess ->
      LPrint.pp_structure (MCLess.to_structure cls) |> print
  | `Anf ->
      let anf = MAnf.from_cless cls in
      LPrint.pp_structure (MAnf.to_structure anf) |> print ) ;

  return ()

let run ir s =
  match run' ir s with
  | Error ParseError ->
      print_endline "syntax error"
  | Error (SimplError err) ->
      MSimpl.pp_err Format.std_formatter err
  | Ok _ ->
      ()
