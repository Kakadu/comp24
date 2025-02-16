(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let prohibited_ops = [ "|"; "->" ]
let first_unop_strings = [ "?"; "~"; "!" ]

let suffix_unop_strings =
  [ "$"; "&"; "*"; "+"; "-"; "/"; "="; ">"; "@"; "^"; "|"; "%"; "<" ]
;;

let base_unops = [ "-"; "+" ]

let first_binop_strings =
  [ "$"; "&"; "*"; "+"; "-"; "/"; "="; ">"; "@"; "^"; "|"; "%"; "<"; "#" ]
;;

let suffix_binop_strings =
  [ "$"
  ; "&"
  ; "*"
  ; "+"
  ; "-"
  ; "/"
  ; "="
  ; ">"
  ; "@"
  ; "^"
  ; "|"
  ; "%"
  ; "<"
  ; "!"
  ; "."
  ; ":"
  ; "?"
  ; "~"
  ]
;;

let base_binops = [ "+"; "-"; "*"; "/"; "<="; "<"; ">="; ">"; "=="; "!="; "&&"; "||" ]
let str_to_list x = Base.String.to_list x |> List.map Base.Char.to_string

let is_op first_strings suffix_strings x =
  match str_to_list x with
  | c :: tl when List.mem c first_strings ->
    List.for_all (fun c -> List.mem c suffix_strings) tl
  | _ -> false
;;

let is_unop = is_op first_unop_strings suffix_unop_strings
let is_binop = is_op first_binop_strings suffix_binop_strings
