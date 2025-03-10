(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Pp

let parse_ty = Ty.parse_ty

let%expect_test "parse_var" =
  pp Ast.pp_ty parse_ty "'some_type_var" ;
  [%expect {| 'some_type_var |}]

let%expect_test "parse_arrow" =
  pp Ast.pp_ty parse_ty "'a -> 'b -> 'c" ;
  [%expect {|
       'a -> 'b -> 'c |}]

let%expect_test "parse_tuple" =
  pp Ast.pp_ty parse_ty "'a * 'b * 'c" ;
  [%expect {|
       'a * 'b * 'c |}]

let%expect_test "parse_constr1" =
  pp Ast.pp_ty parse_ty "int" ;
  [%expect {| int |}]

let%expect_test "parse_constr2" =
  pp Ast.pp_ty parse_ty "int list" ;
  [%expect {| int list |}]

let%expect_test "parse_constr3" =
  pp Ast.pp_ty parse_ty "(int, string) map" ;
  [%expect {|
       (int, string) map |}]

let%expect_test "parse_constr4" =
  pp Ast.pp_ty parse_ty "('a -> int * (string, unit, 'b -> 'c) foo bar) -> e" ;
  [%expect {|
       ('a -> int * (string, unit, ('b -> 'c)) foo bar) -> e |}]
