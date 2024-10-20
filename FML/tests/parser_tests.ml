(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Ast

let parse_with_print input =
  match parse input with
  | Ok ast -> Format.printf "%a\n" pp_program ast
  | Error message -> Format.printf "Error: %s\n" message
;;

let%expect_test _ =
  parse_with_print {| let n = 5|};
  [%expect {| [(DDeclaration (NoRec, (PIdentifier "n"), (EConst (CInt 5))))] |}]
;;

let%expect_test _ =
  parse_with_print {| let flag = true|};
  [%expect {| [(DDeclaration (NoRec, (PIdentifier "flag"), (EConst (CBool true))))] |}]
;;

let%expect_test _ =
  parse_with_print {| let flag = false|};
  [%expect {| [(DDeclaration (NoRec, (PIdentifier "flag"), (EConst (CBool false))))] |}]
;;

let%expect_test _ =
  parse_with_print {| let (h::tl) = lst|};
  [%expect
    {|
    [(DDeclaration (NoRec, (PCons ((PIdentifier "h"), (PIdentifier "tl"))),
        (EIdentifier "lst")))
      ] |}]
;;
