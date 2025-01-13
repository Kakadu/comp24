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
  [%expect {| let const = (fun `arg_0 -> 42) |}]
;;

(* let%expect_test _ =
  test {|
    let (a, b, c) = (1, 2, 3)
  |};
  [%expect {|
    |}]
;; *)

let%expect_test _ =
  test {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect
    {|
    let tuple_sum = (fun `arg_3 -> let `tuple = `arg_3
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
    let _ = let tuple_sum = (fun `arg_3 -> let `tuple = `arg_3
     in let b = (`get_tuple_field `tuple 1)
     in let a = (`get_tuple_field `tuple 0)
     in (( + ) a b))
     in (tuple_sum (1, 2))
    |}]
;;

(* let%expect_test _ =
  test {|
    let list_sum [a;b] = a + b 
  |};
  [%expect
    {|
    |}]
;; *)

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
    let rec map = (fun f list -> if (TODO: check cons pattern) then let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (( :: ) (f hd) (map f tl)) else [])
    |}]
;;

let%expect_test _ =
  test {|
    let rec map = fun f -> fun (list: int list) -> match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl) 
  |};
  [%expect {|
    let rec map = (fun f -> (fun list -> if (`list_is_empty list) then [] else let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (( :: ) (f hd) (map f tl))))
    |}]
;;