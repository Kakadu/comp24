(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

(** Some tests for simple cases *)

let infer_expr_and_print_typ str =
  let parsed = Result.get_ok (Parser.run_parser_expr str) in
  match Inferencer.run_infer_expr parsed with
  | Ok ty -> Format.printf "%a" Typedtree.pp_typ ty
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let inc x = x + 1 in inc|} in
  [%expect {| int -> int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let sum a b = a + b in sum|} in
  [%expect {| int -> int -> int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f x g = g x in f|} in
  [%expect {| 'a -> ('a -> 'b) -> 'b |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {| fun f -> fun x -> f x |} in
  [%expect {| ('a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  let _ =
    infer_expr_and_print_typ {|let id x = x in if (id 2 < 3) then id else (fun t -> t)|}
  in
  [%expect {| 'a -> 'a |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|fun _ a -> a|} in
  [%expect {| 'a -> 'b -> 'b |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f ((1,2)::y) = 0 in f |} in
  [%expect {| (int * int) list -> int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let a = [] in a |} in
  [%expect {| 'a list |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {| let x = 1 in match x with a -> 3 |} in
  [%expect {| int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {| let rec fix f x = f (fix f) x in fix |} in
  [%expect {| (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  let _ =
    infer_expr_and_print_typ
      {| let f a b = 
         let inc = (fun a -> a + 1) in 
        (fun b -> b) inc (a b) in f |}
  in
  [%expect {| ('a -> int) -> 'a -> int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f () = 1 in f |} in
  [%expect {| unit -> int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f = () in f |} in
  [%expect {| unit |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let () = print_int 123 in print_bool true |} in
  [%expect {| unit |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let (a :: b) = 1 :: [1] in (a, b)|} in
  [%expect {| int * int list |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let rec f a b c = if a then b else c in f|} in
  [%expect {| bool -> 'a -> 'a -> 'a |}]
;;

let%expect_test "" =
  let _ =
    infer_expr_and_print_typ
      {|let rec fac (n: int): int = if n <= 1 then 1 else n * fac (n - 1) in fac |}
  in
  [%expect {| int -> int |}]
;;

(* Errors *)

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let (3, a) = (true, 1) in 1 |} in
  [%expect {| Unification failed on bool and int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {| fun (a,a) -> a + a |} in
  [%expect {| Variable a is bound several times in matching |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {| let rec f x = f in f |} in
  [%expect {| Occurs check failed |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f (a,b) = a + b in f (1,2,3)|} in
  [%expect {| Unification failed on int * int and int * int * int |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f (a::1) = 0 in f |} in
  [%expect {| Unification failed on int and 'a list |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f x = a in f|} in
  [%expect {| Undefined variable "a" |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let f x = f 1 in f |} in
  [%expect {| Undefined variable "f" |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|fun f x -> x x|} in
  [%expect {| Occurs check failed |}]
;;

let%expect_test "" =
  let _ = infer_expr_and_print_typ {|let 1 = true in 1|} in
  [%expect {| Unification failed on bool and int |}]
;;
