(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf_ast
open String

let pp_anf_immexpr = function
  | AId id -> id
  | AInt i -> string_of_int i
  | ABool b -> string_of_bool b
  | AUnit -> "()"
;;

let rec pp_anf_aexpr tab ae =
  let next_tab = concat "" [ "\t"; tab ] in
  let pp_anf_cexpr tab = function
    | CImmExpr imm -> pp_anf_immexpr imm
    | ANot e -> concat "" [ "not "; pp_anf_immexpr e ]
    | AOr (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "||"; pp_anf_immexpr e2; ")" ]
    | AAnd (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "&&"; pp_anf_immexpr e2; ")" ]
    | AEq (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "="; pp_anf_immexpr e2; ")" ]
    | AGt (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; ">"; pp_anf_immexpr e2; ")" ]
    | ALt (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "<"; pp_anf_immexpr e2; ")" ]
    | AGte (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; ">="; pp_anf_immexpr e2; ")" ]
    | ALte (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "<="; pp_anf_immexpr e2; ")" ]
    | AAdd (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "+"; pp_anf_immexpr e2; ")" ]
    | ASub (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "-"; pp_anf_immexpr e2; ")" ]
    | AMul (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "*"; pp_anf_immexpr e2; ")" ]
    | ADiv (e1, e2) -> concat "" [ "("; pp_anf_immexpr e1; "/"; pp_anf_immexpr e2; ")" ]
    | AIf (e1, e2, e3) ->
      let next_tab = concat "" [ "\t"; tab ] in
      concat
        ""
        [ "if ("
        ; pp_anf_immexpr e1
        ; ")\n"
        ; tab
        ; "then (\n"
        ; next_tab
        ; pp_anf_aexpr next_tab e2
        ; "\n"
        ; tab
        ; ") else (\n"
        ; next_tab
        ; pp_anf_aexpr next_tab e3
        ; ")"
        ]
    | AApp (e, args) ->
      concat
        ""
        [ "("; pp_anf_immexpr e; " "; concat " " (List.map pp_anf_immexpr args); ")" ]
  in
  match ae with
  | ALet (id, e1, e2) ->
    concat
      ""
      [ "(let "
      ; id
      ; "="
      ; pp_anf_cexpr next_tab e1
      ; "\n"
      ; tab
      ; "in\n"
      ; tab
      ; pp_anf_aexpr tab e2
      ; ")"
      ]
  | ACExpr e -> pp_anf_cexpr tab e
;;

let pp_anf_afun = function
  | AFun (id, args, e) ->
    concat "" [ "(fun "; id; "("; concat " " args; ")->\n\t"; pp_anf_aexpr "\t" e; "\n)" ]
;;
