(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

(***************************Expr*Infer*Tests***************************)
let expr_parse_and_infer_result str =
  match Parser.expr_from_string str with
  | Ok parse_result ->
    (match Inferencer.run_expr_infer parse_result with
     | Ok env -> printf "%a" InferencerTypes.pp_ty env
     | Error err -> printf "%a" InferencerTypes.pp_inf_err err)
  | Error _ -> printf "Syntax error"
;;

let%expect_test _ =
  let _ = expr_parse_and_infer_result {| 2 + "a" |} in
  [%expect {| Unification_failed: int # string |}]
;;

let%expect_test _ =
  let _ = expr_parse_and_infer_result {| 2 + 2 |} in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ = expr_parse_and_infer_result {| 5 |} in
  [%expect {| int |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let f () = 1 in f |} in
  [%expect {| unit -> int |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let f = () in f |} in
  [%expect {| unit |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let () = print_int 52 in print_string "52" |} in
  [%expect {| unit |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let (a :: b) = 1 :: 1 :: [] in (a, b)|} in
  [%expect {| int * int list |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let a = 1 :: 1 :: [] in a|} in
  [%expect {| int list |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let rec f a b c = if a then b else c in f|} in
  [%expect {| bool -> '4 -> '4 -> '4 |}]
;;

let%expect_test "" =
  let _ = expr_parse_and_infer_result {|let f ((1,2)::y) = 0 in f |} in
  [%expect {| (int * int) list -> int |}]
;;

let%expect_test "" =
  expr_parse_and_infer_result {|fun p -> let (a,b) = p in a + b |};
  [%expect {|
    int * int -> int |}]
;;

(***************************Structure*Infer*Tests***************************)

let parse_and_infer_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    (match Inferencer.run_stucture_infer parse_result with
     | Ok env -> printf "%a" Inferencer.TypeEnv.pp_env env
     | Error err -> printf "%a" InferencerTypes.pp_inf_err err)
  | Error _ -> printf "Syntax error"
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| 2 + "a" |} in
  [%expect {| Unification_failed: int # string |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| 2 + 2 |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let a = 5 |} in
  [%expect {|
    "a": int|}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let (a, b) = 5, "s" |} in
  [%expect {|
    "a": int
    "b": string|}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = fun (a, b) -> a + 1 |} in
  [%expect {|
      "n": '1 . int * '1 -> int |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n a b = a + 1, b |} in
  [%expect {|
      "n": '1 . int -> '1 -> int * '1 |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let (a, b, _) = 1, 2, 3 and c = "s" |} in
  [%expect {|
      "_": int
      "a": int
      "b": int
      "c": string |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| let n = fun x -> match x with | true -> true | false -> false |}
  in
  [%expect {|
      "n": bool -> bool |}]
;;

let%expect_test "" =
  parse_and_infer_result
    {| let rec is_even n =
    if n = 0 then true
    else is_odd (n - 1)
  
  and is_odd n =
    if n = 0 then false
    else is_even (n - 1);;  |};
  [%expect {|
    "is_even": int -> bool
    "is_odd": int -> bool |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
           let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
           let a = fac 5 |};
  [%expect
    {|
        "a": int
        "fac": int -> int
        "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = 5 |};
  [%expect {|
    "n": int |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = (5, 4, "a") |};
  [%expect {|
    "n": int * int * string |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = 5 :: [] 
  let b = n|};
  [%expect {|
    "b": int list
    "n": int list |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let b = [1; 2] |};
  [%expect {|
      "b": int list |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let b = [1 :: []] |};
  [%expect {|
      "b": (int list) list |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let b = []
         let a = "a"
         let f = 5 
         let id = f  |};
  [%expect {|
    "a": string
    "b": '0 . '0 list
    "f": int
    "id": int |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun x -> x + 1 
  let b = n 5|};
  [%expect {|
    "b": int
    "n": int -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun x -> x
  let b = n 5
  let c = n true |};
  [%expect {|
    "b": int
    "c": bool
    "n": '0 . '0 -> '0 |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun a :: b -> a 
  let b = n (46 :: 52 :: []) |};
  [%expect {|
      "b": int
      "n": '0 . '0 list -> '0 |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun (a, b) -> a + 1 |};
  [%expect {|
      "n": '1 . int * '1 -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let n = fun x -> match x with | true -> true | false -> false |};
  [%expect {|
      "n": bool -> bool |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let x = if true then if true then 1 else 2 else 3 |};
  [%expect {|
      "x": int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
    let a = factorial_recursive 5
    let b = factorial_recursive 6 |};
  [%expect {|
      "a": int
      "b": int
      "factorial_recursive": int -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
           let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
           let a = fac 5 |};
  [%expect
    {|
      "a": int
      "fac": int -> int
      "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let rec fix = fun f -> (fun x -> f (fix f) x) |};
  [%expect {|
      "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {|let rec f = fun (a, b) -> if a + b < 10 then a + b else f (a-1,b-1) |};
  [%expect {|
      "f": int * int -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {|
      let rev = fun lst ->
        (let rec helper = fun acc -> (fun lst ->
        match lst with
          | [] -> acc
          | h :: tl -> helper (h :: acc) tl)
        in
        helper [] lst)
      let reversed1 = rev (1 :: 2 :: 3 :: 4 :: 5 :: [])
      let reversed2 = rev (true :: false :: false :: false :: [])
    |};
  [%expect
    {|
      "rev": '13 . '13 list -> '13 list
      "reversed1": int list
      "reversed2": bool list |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let a = let b x = x + 1 in b;;
    let c = a 4;;|};
  [%expect {|
      "a": int -> int
      "c": int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let x = let f a b = let inc = (fun a -> a + 1) in (fun b -> b) inc (a b) in f |};
  [%expect {|
      "x": '11 . ('11 -> int) -> '11 -> int |}]
;;

let%expect_test "" =
  parse_and_infer_result {|let map f p = let (a,b) = p in (f a, f b) |};
  [%expect {|
    "map": '2 '4 . ('2 -> '4) -> '2 * '2 -> '4 * '4 |}]
;;

let%expect_test "" =
  parse_and_infer_result {|let a b (c, d) = (b c, b d) |};
  [%expect {|
    "a": '1 '3 . ('1 -> '3) -> '1 * '1 -> '3 * '3 |}]
;;

let%expect_test "" =
  parse_and_infer_result {|let map p = let (a,b) = p in a + b |};
  [%expect {|
        "map": int * int -> int |}]
;;

let%expect_test "" =
  parse_and_infer_result {|let map = fun p -> let a = p in a |};
  [%expect {|
        "map": '1 . '1 -> '1|}]
;;

let%expect_test "" =
  parse_and_infer_result {|let rec map = fun p -> let rec a = p in a |};
  [%expect {|
        "map": '2 . '2 -> '2|}]
;;

let%expect_test "" =
  parse_and_infer_result
    {|let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l
  let feven p n =
    let (e, o) = p in
    if n == 0 then 1 else o (n - 1)
  let fodd p n =
    let (e, o) = p in
    if n == 0 then 0 else e (n - 1)
  let tie = fixpoly (feven, fodd)
  
  let rec meven n = if n = 0 then 1 else modd (n - 1)
  and modd n = if n = 0 then 1 else meven (n - 1)
  let main =
    let () = print_int (modd 1) in
    let () = print_int (meven 2) in
    let (even,odd) = tie in
    let () = print_int (odd 3) in
    let () = print_int (even 4) in
    0|};
  [%expect
    {|
        "feven": '32 . '32 * (int -> int) -> int -> int
        "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3
        "fixpoly": '25 '26 . (('25 -> '26) * ('25 -> '26) -> '25 -> '26) * (('25 -> '26) * ('25 -> '26) -> '25 -> '26) -> ('25 -> '26) * ('25 -> '26)
        "fodd": '44 . (int -> int) * '44 -> int -> int
        "main": int
        "map": '8 '10 . ('8 -> '10) -> '8 * '8 -> '10 * '10
        "meven": int -> int
        "modd": int -> int
        "tie": (int -> int) * (int -> int)|}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let _start () () a () b _c () d __ =
      let () = print_int (a+b) in
      let () = print_int __ in
      a*b / _c + d
    
    
    let main =
      print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555)) |};
  [%expect
    {|
      "_start": unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
      "main": unit |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let a (x: int) = x |};
  [%expect {|
      "a": int -> int |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a (b: bool * string) (c: int -> int): int = 5;; |};
  [%expect {|
    "a": bool * string -> (int -> int) -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b c: int = 5;; |};
  [%expect {|
    "a": '3 '4 . '3 -> '4 -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b : int -> int = fun x -> b x;; |};
  [%expect {|
    "a": (int -> int) -> int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b x: int -> int = b x;; |};
  [%expect {|
    "a": '5 . ('5 -> int -> int) -> '5 -> int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b (x: int): int -> int = b x;; |};
  [%expect {|
    "a": (int -> int -> int) -> int -> int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b = b;; |};
  [%expect {|
    "a": '0 . '0 -> '0
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b : int = b;; |};
  [%expect {|
    "a": int -> int
     |}]
;;
