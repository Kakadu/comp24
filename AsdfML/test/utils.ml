(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib.Inferencer
open Lib.Pp_typing
open Base
open Lib.Pp_ast
open Lib.Tast
open Lib

let test_parser code =
  match Parser.parse_program code ~print_ast:true with
  | Ok _ -> ()
  | Error e -> print_endline e
;;

let test_inferencer code =
  let open Format in
  let pa = false in
  let ast = Result.ok_or_failwith (Parser.parse_program ~print_ast:pa code) in
  match inference_program ast with
  | Ok t ->
    printf
      "%s"
      (t
       |> List.map ~f:(function TDLet (ty, _, pat, _) ->
         asprintf "%a: %a" pp_pattern pat pp_ty ty)
       |> String.concat ~sep:"\n")
  | Error e -> eprintf "%a" pp_error e
;;
