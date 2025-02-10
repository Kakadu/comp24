open Base
open Lib

let test code =
  let open Format in
  match Parser.parse_program code with
  | Error e -> print_endline e
  | Ok ast ->
    (match Inferencer.inference_program ast with
     | Error e -> printf "%a" Pp_typing.pp_error e
     | Ok ast ->
       let ast = Remove_patterns.remove_patterns ast in
       let ast = Remove_match.remove_match ast in
       printf
         "\n%s"
         (ast |> List.map ~f:(asprintf "%a" Tast.pp_tdefinition) |> String.concat))
;;

let%expect_test _ =
  test {|
    let const _ = 42 
  |};
  [%expect {| let const = (fun _ -> 42) |}]
;;

let%expect_test _ =
  test {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect
    {|
    let tuple_sum = (fun `arg_0 -> let `tuple = `arg_0
     in let b = (`get_tuple_field `tuple 1)
     in let a = (`get_tuple_field `tuple 0)
     in (( + ) a b))
    |}]
;;

let%expect_test _ =
  test
    {|
    let _ = 
      let tuple_sum = fun (a, b) -> a + b in 
      tuple_sum (1, 2)
  |};
  [%expect
    {|
    let _ = let tuple_sum = (fun `arg_0 -> let `tuple = `arg_0
     in let b = (`get_tuple_field `tuple 1)
     in let a = (`get_tuple_field `tuple 0)
     in (( + ) a b))
     in (tuple_sum (1, 2))
    |}]
;;

let%expect_test _ =
  test {|
    let _ = 
      let (x, y) = (1, 2) in 
      x + y
  |};
  [%expect
    {|
    let _ = let `tuple = (1, 2)
     in let y = (`get_tuple_field `tuple 1)
     in let x = (`get_tuple_field `tuple 0)
     in (( + ) x y)
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec map f list = match list with
      | hd::tl -> f hd :: map f tl
      | _ -> []
    |};
  [%expect
    {|
    let rec map = (fun f list -> if (( && ) (not (`list_is_empty list)) (( && ) true true)) then let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (( :: ) (f hd) (map f tl)) else [])
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec map = fun f -> fun (list: int list) -> match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl) 
  |};
  [%expect
    {|
    let rec map = (fun f -> (fun list -> if (`list_is_empty list) then [] else let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (( :: ) (f hd) (map f tl))))
    |}]
;;

let%expect_test _ =
  test
    {|
    let list_mul list = 
      let rec helper acc list = match list with
        | [] -> acc
        | 0 :: _ -> 0
        | hd :: tl -> helper (hd * acc) tl
      in
      helper 1 list
  |};
  [%expect
    {|
    let list_mul = (fun list -> let rec helper = (fun acc list -> if (`list_is_empty list) then acc else if (( && ) (not (`list_is_empty list)) (( && ) (( = ) (`list_hd list) 0) true)) then 0 else let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (helper (( * ) hd acc) tl))
     in (helper 1 list))
    |}]
;;

(* let%expect_test _ =
  test {|
    let (a, b, c) = (1, 2, 3)
  |};
  [%expect {|
    |}]
;; *)

let%expect_test _ =
  test
    {|
    let pow x n =
      let rec helper acc n =
        match n with
        | 0 -> acc
        | n -> helper (acc * x) (n - 1)
      in
      helper 1 n
  |};
  [%expect
    {|
    let pow = (fun x n -> let rec helper = (fun acc n -> if (( = ) n 0) then acc else let n = n
     in (helper (( * ) acc x) (( - ) n 1)))
     in (helper 1 n))
    |}]
;;
