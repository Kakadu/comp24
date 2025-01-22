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
       printf
         "\n%s"
         (ast |> List.map ~f:(asprintf "%a" Tast.pp_tdefinition) |> String.concat))
;;

let%expect_test _ =
  test {|
    let const _ = 42 
  |};
  [%expect {|
    let const = (fun `arg_0 -> match `arg_0 with
    | _ -> 42)
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
  test {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect
    {|
    let tuple_sum = (fun `arg_3 -> match `arg_3 with
    | (a, b) -> (( + ) a b))
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
    let _ = let tuple_sum = (fun `arg_3 -> match `arg_3 with
    | (a, b) -> (( + ) a b))
     in (tuple_sum (1, 2))
    |}]
;;

let%expect_test _ =
  test {|
    let list_sum [a;b] = a + b 
  |};
  [%expect
    {|
    let list_sum = (fun `arg_3 -> match `arg_3 with
    | [a; b] -> (( + ) a b))
    |}]
;;

let%expect_test _ =
  test {|
    let _ = 
      let (x, y) = (1, 2) in 
      x + y
  |};
  [%expect {|
    let _ = match (1, 2) with
    | (x, y) -> (( + ) x y)
    |}]
;;
