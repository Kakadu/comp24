(* Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Roflanml_lib
open Roflanml_lib.Typing
open Roflanml_lib.Closure_conversion
open Roflanml_lib.Lambda_lifting
open Roflanml_lib.Anf_ast
open Roflanml_lib.Anf
open Roflanml_lib.Ast_to_str

let () =
  let open Result in
  let ( let* ) = ( >>= ) in
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let prog =
    let* prog = Parser.parse input in
    let* _ =
      match Typechecker.typecheck prog with
      | Ok _ -> return prog
      | Error err ->
        Stdlib.Format.printf "%a" pp_error err;
        fail "Typecheck error"
    in
    let prog = close_program prog in
    let* prog = lift_program prog in
    anf_program prog
  in
  match prog with
  | Ok prog ->
    List.iter prog ~f:(fun decl ->
      Stdlib.Format.printf "%s\n" (ast_to_str (anf_to_ast decl)))
  | Error err -> Stdlib.print_endline err
;;
