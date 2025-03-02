(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast_test_utils = struct
  open Base.Result
  open Common.Errors

  let ( let* ) = ( >>= )

  let ( let*! ) x f =
    let* x = x in
    let* ast = f x in
    return ast
  ;;

  let ( let+! ) x f =
    let*! x = x in
    return @@ f x
  ;;

  let ( let* ) x f =
    let* x = x in
    let* _ = Inferencer.check_program x in
    let* ast = f x in
    let* _ = Inferencer.check_program ast in
    return ast
  ;;

  let ( let+ ) x f =
    let* x = x in
    return @@ f x
  ;;

  let print_error = function
    | Parser e -> Parser.PP.pp_error Format.std_formatter e
    | Infer e -> Inferencer.PP.pp_error Format.std_formatter e
    | Alpha_converter (Illegal_state_error s) -> Format.print_string s
  ;;

  let print_result ast_printer = function
    | Ok ast' -> ast_printer ast'
    | Error err -> print_error err
  ;;
end
