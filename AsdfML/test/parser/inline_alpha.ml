open Base
open Format
open Lib

let test code =
  match Parser.parse_program code with
  | Error e -> print_endline e
  | Ok ast ->
    let alpha_ast = Alpha.alpha_program ast in
    printf "%a" Pp_ast.pp_program alpha_ast
;;

let%expect_test _ =
  test {|
  let a = 1
  let a = 2
  |};
  [%expect {|
    let a = 1
    let a_0 = 2
    |}]
;;

let%expect_test _ =
  test {|
  let fn a b c = a + b + c
  |};
  [%expect {| let fn = (fun a b c -> (( + ) (( + ) a b) c)) |}]
;;

let%expect_test _ =
  test {|
  let fn = (fun a -> fun a -> a + a)
  |};
  [%expect {| let fn = (fun a -> (fun a_0 -> (( + ) a_0 a_0))) |}]
;;

let%expect_test "" =
  test {|
    let f a = let f a s = a + s in f a 5
  |};
  [%expect
    {|
    let f = (fun a -> let f_0 = (fun a_0 s -> (( + ) a_0 s)) in 
      (f_0 a 5))
    |}]
;;

let%expect_test "" =
  test {|
    let m = 5
    let n = 6
    let m = 55
    let f (a, s, p) = (m, n, p) 
  |};
  [%expect
    {|
    let m = 5
    let n = 6
    let m_0 = 55
    let f = (fun (a, s, p) -> (m_0, n, p)) |}]
;;

let%expect_test "" =
  test {|
    let f a = (f 5)
  |};
  [%expect {|
    let f = (fun a -> (f_0 5)) |}]
;;

let%expect_test "" =
  test {|
    let f (a, s, p) = let f (a, s, p) = a * s * p in a + s + p
  |};
  [%expect
    {|
    let f = (fun (a, s, p) ->
      let f_0 = (fun (a_0, s_0, p_0) -> (( * ) (( * ) a_0 s_0) p_0)) in
      (( + ) (( + ) a s) p))
    |}]
;;

let%expect_test "" =
  test {|
    let f (a, a) = 5
  |};
  [%expect {| let f = (fun (a, a) -> 5) |}]
;;

let%expect_test "" =
  test {|
    let rec f a = (f 5)
  |};
  [%expect {|
    let rec f = (fun a -> (f 5)) |}]
;;

let%expect_test "" =
  test {|
    let f a = a + 5
    let g = f + 10
    let f a = a + 6  
  |};
  [%expect
    {|
    let f = (fun a -> (( + ) a 5))
    let g = (( + ) f 10)
    let f_0 = (fun a_0 -> (( + ) a_0 6))
    |}]
;;

let%expect_test "" =
  test {|
    let test = let a = 2 in let c b = ( + ) a b in let a = 3 in c 2
  |};
  [%expect
    {|
    let test = let a = 2 in
      let c = (fun b -> (( + ) a b)) in
      let a_0 = 3 in
      (c 2)
    |}]
;;

let%expect_test "" =
  test {|
    let f = (fun x -> x) ((fun x -> x) 5)
  |};
  [%expect {| let f = ((fun x -> x) ((fun x_0 -> x_0) 5)) |}]
;;

let%expect_test "" =
  test {|
    let a = 1
    let b = 2
    let (a,b) = (a,b)
    let (a,b) = (b,a)
  |};
  [%expect
    {|
    let a = 1
    let b = 2
    let (a_0, b_0) = (a, b)
    let (a_1, b_1) = (b_0, a_0)
    |}]
;;

let%expect_test "" =
  test {|
    let list = [1;2;3]
    let _::list = list
    let _::list = list
  |};
  [%expect
    {|
    let list = [1; 2; 3]
    let _ :: list_0 = list
    let _ :: list_1 = list_0
    |}]
;;

let%expect_test "" =
  test {|
    let list = [[1;2];[3;4]]
    let [_;list] = list
  |};
  [%expect {|
    let list = [[1; 2]; [3; 4]]
    let [_; list_0] = list
    |}]
;;

let%expect_test _ =
  test {|
  let a = 1
  let a_0 = 42
  let a_0 = 42
  let a = 2
  |};
  [%expect {|
    let a = 1
    let a_0 = 42
    let a_0_0 = 42
    let a_1 = 2
    |}]
;;

let%expect_test _ =
  test
    {|
  let a = 42
  let _ = match a with
    | 42 -> 42 + a
    | a -> a + a
    | a_0 -> a_0 + a
  |};
  [%expect
    {|
    let a = 42
    let _ = match a with
      | 42 -> (( + ) 42 a)
      | a_0 -> (( + ) a_0 a_0)
      | a_0 -> (( + ) a_0 a)
    |}]
;;

let%expect_test _ =
  test {|
  let ll_0 = fun x -> x
  let anf = 42
  let x = anf
  let y = ll_0 anf
  |};
  [%expect
    {|
    let __var_ll_0 = (fun x -> x)
    let __var_anf = 42
    let x_0 = __var_anf
    let y = (__var_ll_0 __var_anf)
    |}]
;;
