(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_utils
open Middleend

let%expect_test "test int inference" =
  test "4";
  [%expect {| int |}]
;;

let%expect_test "test char inference" =
  test "'c'";
  [%expect {| char |}]
;;

let%expect_test "test string inference" =
  test "\"help me plz\"";
  [%expect {| string |}]
;;

let%expect_test "test if inference" =
  test "if true then 4 else 5";
  [%expect {| int |}]
;;

let%expect_test "test wrong if inference" =
  test "if false then 4 else 'c'";
  [%expect {| ErrorType infering error: failed unification of types int and char |}]
;;

let%expect_test "test if without else inference" =
  test "if true then 4";
  [%expect {| int |}]
;;

let%expect_test "test fun inference" =
  test "fun a -> a";
  [%expect {| 'h -> 'h |}]
;;

let%expect_test "test fun with several arguments inference" =
  test "fun a b c -> b";
  [%expect {| 'h -> 'i -> 'j -> 'i |}]
;;

let%expect_test "test let fun" =
  test "let a b = b in a";
  [%expect {| 'i -> 'i |}]
;;

let%expect_test "test let fun apply" =
  test "let a b = b in a 4";
  [%expect {| int |}]
;;

let%expect_test "test simple let" =
  test "let a = 4 in a";
  [%expect {| int |}]
;;

let%expect_test "test several lets" =
  test "let f a = a and a = true and b = 4 in f f b";
  [%expect {| int |}]
;;

let%expect_test "test tuple" =
  test "(4, 5, true, 'a')";
  [%expect {| (int * int * bool * char) |}]
;;

let%expect_test "test difficult tuple" =
  test "(let b = 4 in b, (3, 5), fun a -> a, 'a')";
  [%expect {| (int * (int * int) * 'h -> ('h * char)) |}]
;;

let%expect_test "test let rec" =
  test "let rec a = 4 in a";
  [%expect {| int |}]
;;

let%expect_test "test let rec fun" =
  test "let rec f a = if a then a else f a in f";
  [%expect {| bool -> bool |}]
;;

let%expect_test "test let rec and" =
  test {|let rec f a b = if b then g a b else m
  and g a b = f a b and m = 4 in f|};
  [%expect {| 'o -> bool -> int |}]
;;

let%expect_test "test invalid let rec with pat binding" =
  test {|let rec a, b = 3, 4 in a|};
  [%expect
    {| ErrorType infering error: only variables are allowed as left-hand side of `let rec' |}]
;;

let%expect_test "test let statement" =
  test {|let a = 4|};
  [%expect {| val a : int |}]
;;

let%expect_test "test let statements" =
  test {|let a = 4
  let b = a|};
  [%expect {|
    val a : int
    val b : int |}]
;;

let%expect_test "test let and statements" =
  test {|let a = 4
  and b = true
  let c = a, b|};
  [%expect {|
    val a : int
    val b : bool
    val c : (int * bool) |}]
;;

let%expect_test "test let rec and statements" =
  test {|let rec f a = a
  and g b = true
  let c = f (g 4)|};
  [%expect {|
    val f : 'k -> 'k
    val g : 'j -> bool
    val c : bool
    |}]
;;

let%expect_test "test wrong let exp and statements" =
  test {|let a = 4 in a
  let b = a|};
  [%expect {|
    ErrorType infering error: variable a is not found |}]
;;

let%expect_test "test constructor" =
  test {|let a = []|};
  [%expect {| val a : 'h list |}]
;;

let%expect_test "test constructor some with" =
  test {|Some true|};
  [%expect {|
      bool optional |}]
;;

let%expect_test "test constructor some" =
  test {|let a b = if b then None else Some b in a|};
  [%expect {|
    bool -> bool optional |}]
;;

let%expect_test "test let with if inside on arg" =
  test {|let f a = if a then a else a in f|};
  [%expect {|
    bool -> bool |}]
;;

let%expect_test "test fun with if inside on arg" =
  test {|fun a -> if a then a else a|};
  [%expect {|
    bool -> bool |}]
;;

let%expect_test "test list" =
  test {|[2]|};
  [%expect {|
    int list |}]
;;

let%expect_test "test list 2" =
  test {|[2; 4]|};
  [%expect {|
    int list |}]
;;

let%expect_test "test list 3" =
  test {|([2; 4]) :: []|};
  [%expect {|
    int list list |}]
;;

let%expect_test "test type" =
  test {|fun a -> (a: int)|};
  [%expect {|
    int -> int |}]
;;

let%expect_test "test fun type" =
  test {|let a b: int -> int = b|};
  [%expect {|
    val a : (int -> int) -> int -> int |}]
;;

let%expect_test "test type bool" =
  test {|let a b: bool = b|};
  [%expect {|
    val a : bool -> bool |}]
;;

let%expect_test "test type string" =
  test {|let a b: string = b|};
  [%expect {|
    val a : string -> string |}]
;;

let%expect_test "test type char" =
  test {|let a b: char = b|};
  [%expect {|
    val a : char -> char |}]
;;

let%expect_test "test type fun hard" =
  test {|let a b: (int -> int) -> int = b|};
  [%expect {|
    val a : ((int -> int) -> int) -> (int -> int) -> int |}]
;;

let%expect_test "test type constructor" =
  test {|let a b: int list = b|};
  [%expect {|
    val a : int list -> int list |}]
;;

let%expect_test "test type tuple" =
  test {|let a b: (int * bool) = b|};
  [%expect {|
    val a : (int * bool) -> (int * bool) |}]
;;

let%expect_test "test predef ops" =
  test {|let a = 1 + 2|};
  [%expect {|
    val a : int |}]
;;

let%expect_test "test predef ops 2" =
  test {|(-) 1|};
  [%expect {|
    int -> int |}]
;;

let%expect_test "test predef ops 3" =
  test {|-1|};
  [%expect {|
    int |}]
;;

let%expect_test "test predef ops 4" =
  test {|(*)|};
  [%expect {|
    int -> int -> int |}]
;;

let%expect_test "test patter matching" =
  test {|match 4 with 4 -> 4| a -> a | _ -> 3|};
  [%expect {|
    int |}]
;;

let%expect_test "test patter matching wrong left type" =
  test {|match 4 with 4 -> 4| a -> a | _ -> 'c'|};
  [%expect {|
    ErrorType infering error: failed unification of types int and char |}]
;;

let%expect_test "test patter matching wrong right type" =
  test {|match 4 with true -> 4| a -> a | _ -> 4|};
  [%expect {|
    ErrorType infering error: failed unification of types int and bool |}]
;;

let%expect_test "test patter matching wrong right type and expr" =
  test {|match 4 with true -> 4 | _ -> 4|};
  [%expect {|
    ErrorType infering error: failed unification of types int and bool |}]
;;

let%expect_test "test patter matching tuple" =
  test {|fun a -> match a with (4, m) -> m | _ -> true|};
  [%expect {|
    (int * bool) -> bool |}]
;;

let%expect_test "test patter matching tuple" =
  test {|fun a -> match a with (m: bool) -> m|};
  [%expect {|
    bool -> bool |}]
;;

let%expect_test "test patter matching tuple" =
  test {|fun a -> match a with | Some n -> n | None -> "Hello, word!"|};
  [%expect {|
    string optional -> string |}]
;;

let%expect_test "test function" =
  test {|function | (a: cool_type) -> 3 | _ -> 4|};
  [%expect {|
    cool_type -> int |}]
;;

let%expect_test "test fun with pattern" =
  test {|fun (a, [b]) -> a + b|};
  [%expect {|
    (int * int list) -> int |}]
;;

let%expect_test "test let with pattern" =
  test {|let (Some m) = Some (3, true)
  let (c, d) = m|};
  [%expect {|
    val m : (int * bool)
    val c : int
    val d : bool |}]
;;

let%expect_test "test let rewrite" =
  test {|let m = 4
  let m = true|};
  [%expect {|
    val m : int
    val m : bool |}]
;;

let%expect_test "test op not" =
  test {|fun a -> not a|};
  [%expect {|
    bool -> bool |}]
;;

let%expect_test "test equal" =
  test {|(=)|};
  [%expect {| 'h -> 'h -> bool |}]
;;

let%expect_test "test less" =
  test {|(>) 4|};
  [%expect {|
    int -> bool |}]
;;

let%expect_test "test not equal" =
  test {|4 <> 5|};
  [%expect {|
    bool |}]
;;

let%expect_test "test match list" =
  test {|let rec a n = match n with 
  | h :: tl -> h + a tl
  | [] -> 0
  |};
  [%expect {|
    val a : int list -> int |}]
;;

let%expect_test "test function with list" =
  test {|let rec a = function
  | h :: tl -> h + a tl
  | [] -> 0
  |};
  [%expect {|
    val a : int list -> int |}]
;;

let%expect_test "test poly inference" =
  test {|
    let f a b = a, b;;
    f 3 4;;
    f 'a' 'b';;
  |};
  [%expect {|
    val f : 'h -> 'i -> ('h * 'i)
    (int * int)
    (char * char)
    |}]
;;

let%expect_test "test poly inference" =
  Typing.Inference.infer_program
    (Middleend.Converter.anf_to_ast
       [ AbsStr_func
           ( "abs_value"
           , [ "n" ]
           , LComplex
               (CExp_apply ("__op_plus", [ AExp_constant (Const_int 52); AExp_ident "n" ]))
           )
       ])
  |> function
  | Result.Ok res ->
    let rec helper acc = function
      | (id, v) :: tl ->
        acc
        ^ (if id = "" then "" else "val " ^ id ^ " : ")
        ^ Typing.Types.show_type_val v
        ^ "\n"
        ^ helper acc tl
      | _ -> acc
    in
    Format.print_string @@ helper "" res
  | Result.Error s ->
    Format.eprintf "Error%s" s;
    [%expect
      {|
    val f : 'h -> 'i -> ('h * 'i)
    (int * int)
    (char * char)
    |}]
;;
