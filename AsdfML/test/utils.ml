(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib.Inferencer
open Lib.Pp_typing
open Base

let test_parser code =
  match Lib.Parser.parse_program code ~print_ast:true with
  | Ok _ -> ()
  | Error e -> print_endline e
;;

let test_inferencer code =
  let open Format in
  let pa = false in
  let ast = Result.ok_or_failwith (Lib.Parser.parse_program ~print_ast:pa code) in
  match inference_program ast with
  | Ok t ->
    printf
      "%s"
      (t
       |> List.map ~f:(fun (s, t) -> asprintf "%s: %a" s pp_typ t)
       |> String.concat ~sep:"\n")
  | Error e -> eprintf "%a" pp_error e
;;
