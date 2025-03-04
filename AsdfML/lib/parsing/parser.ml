(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lexing
open Lexer
open Base

(* Prints the line number and character number where the error occurred.*)
let error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.asprintf "Line %d Pos %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_program ?(print_ast = false) (code : string)
  : (Ast.definition list, string) result
  =
  let lexbuf = Lexing.from_string code in
  try
    let ast = Menhir.program Lexer.token lexbuf in
    if print_ast then Format.printf "%a" Pp_ast.pp_program ast;
    Ok ast
  with
  | SyntaxError msg ->
    let error_msg = Format.sprintf "%s: %s@." (error_position lexbuf) msg in
    Error error_msg
  | Menhir.Error ->
    let error_msg =
      Format.sprintf
        "%s: syntax error\nlex_buffer:\n%s"
        (error_position lexbuf)
        (Bytes.to_string lexbuf.lex_buffer)
    in
    Error error_msg
;;
