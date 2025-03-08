open HamsterML.ME
open LambdaLiftingTest

let match_elim_prog (s : string) = s |> lambda_lift_prog |> convert_prog

let pp_match_elim_prog (s : string) =
  let open HamsterML.PrinterME in
  match_elim_prog s |> pretty_print_me_prog |> print_string
;;

let%expect_test _ =
  pp_match_elim_prog {| 
    let main x = match x with 1 -> 10 | _ -> 20 |};
  [%expect {| let ll_var_0 arg_0 = if (1 = arg_0) then 10 else 20 |}]
;;

let%expect_test _ =
  pp_match_elim_prog
    {| 
    let main x = match x with 1 -> 10 | 2 -> 20 | 3 -> 30 | _ -> 0 |};
  [%expect
    {| let ll_var_0 arg_0 = if (1 = arg_0) then 10 else if (2 = arg_0) then 20 else if (3 = arg_0) then 30 else 0 |}]
;;

let%expect_test _ =
  pp_match_elim_prog
    {| 
    let main x = match x with 
    | (1, 2) -> 10 
    | _ -> 20 |};
  [%expect
    {| let ll_var_0 arg_0 = if ((1 = ((tuple_get arg_0) 0)) && (2 = ((tuple_get arg_0) 1))) then 10 else 20 |}]
;;

let%expect_test _ =
  pp_match_elim_prog
    {| 
    let main = match (1, 3) with 
    | (1, 2) -> 10 
    | _ -> 20 |};
  [%expect
    {| let ll_var_0 = if ((1 = ((tuple_get (1, 3)) 0)) && (2 = ((tuple_get (1, 3)) 1))) then 10 else 20 |}]
;;

let%expect_test _ =
  pp_match_elim_prog {| 
  let main x = match x with 
  | [1;2] -> 2 
  | _ -> 3 |};
  [%expect
    {| let ll_var_0 arg_0 = if if (2 = (list_length arg_0)) then ((1 = ((list_get arg_0) 0)) && (2 = ((list_get arg_0) 1))) else false then 2 else 3 |}]
;;

let%expect_test _ =
  pp_match_elim_prog
    {| 
  let main x = match [3; 2; 1] with 
  | [1;2;3] -> 2 
  | _ -> 3 |};
  [%expect
    {| let ll_var_0 arg_0 = if if (3 = (list_length [3; 2; 1])) then (((1 = ((list_get [3; 2; 1]) 0)) && (2 = ((list_get [3; 2; 1]) 1))) && (3 = ((list_get [3; 2; 1]) 2))) else false then 2 else 3 |}]
;;

let%expect_test _ =
  pp_match_elim_prog {| let main x = match x with h :: tl -> h |};
  [%expect
    {| let ll_var_0 arg_0 = let arg_2 = (list_tail arg_0) in let arg_1 = (list_head arg_0) in arg_1 |}]
;;

let%expect_test _ =
  pp_match_elim_prog {| let main x = match x with h :: tl -> h | [] -> 0 |};
  [%expect
    {| let ll_var_0 arg_0 = let arg_2 = (list_tail arg_0) in let arg_1 = (list_head arg_0) in if let arg_1 = (list_head arg_0) in let arg_2 = (list_tail arg_0) in ((((list_length arg_0) >= 2) && (arg_1 = (list_head arg_0))) && (arg_2 = (list_tail arg_0))) then arg_1 else 0 |}]
;;

let%expect_test _ =
  pp_match_elim_prog {| let main x = match x with (a, b, c) -> a + b + c |};
  [%expect
    {| let ll_var_0 arg_0 = let arg_3 = ((tuple_get arg_0) 2) in let arg_2 = ((tuple_get arg_0) 1) in let arg_1 = ((tuple_get arg_0) 0) in ((arg_1 + arg_2) + arg_3) |}]
;;

let%expect_test "016lists" =
  pp_match_elim_prog
    {| 
    let rec map f xs =
    match xs with
    | [] -> []
    | a::[] -> [f a]
    | a::b::[] -> [f a; f b]
    | a::b::c::[] -> [f a; f b; f c]
    | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl |};
  [%expect
    {| let rec ll_var_0 arg_1 arg_2 = if if (0 = (list_length arg_2)) then true else false then [] else let [] = (list_tail arg_2) in let arg_3 = (list_head arg_2) in if let arg_3 = (list_head arg_2) in let [] = (list_tail arg_2) in ((((list_length arg_2) >= 2) && (arg_3 = (list_head arg_2))) && if (0 = (list_length (list_tail arg_2))) then true else false) then [(arg_1 arg_3)] else let arg_5::[] = (list_tail arg_2) in let arg_4 = (list_head arg_2) in if let arg_4 = (list_head arg_2) in let arg_5::[] = (list_tail arg_2) in ((((list_length arg_2) >= 2) && (arg_4 = (list_head arg_2))) && let arg_5 = (list_head (list_tail arg_2)) in let [] = (list_tail (list_tail arg_2)) in ((((list_length (list_tail arg_2)) >= 2) && (arg_5 = (list_head (list_tail arg_2)))) && if (0 = (list_length (list_tail (list_tail arg_2)))) then true else false)) then [(arg_1 arg_4); (arg_1 arg_5)] else let arg_7::arg_8::[] = (list_tail arg_2) in let arg_6 = (list_head arg_2) in if let arg_6 = (list_head arg_2) in let arg_7::arg_8::[] = (list_tail arg_2) in ((((list_length arg_2) >= 2) && (arg_6 = (list_head arg_2))) && let arg_7 = (list_head (list_tail arg_2)) in let arg_8::[] = (list_tail (list_tail arg_2)) in ((((list_length (list_tail arg_2)) >= 2) && (arg_7 = (list_head (list_tail arg_2)))) && let arg_8 = (list_head (list_tail (list_tail arg_2))) in let [] = (list_tail (list_tail (list_tail arg_2))) in ((((list_length (list_tail (list_tail arg_2))) >= 2) && (arg_8 = (list_head (list_tail (list_tail arg_2))))) && if (0 = (list_length (list_tail (list_tail (list_tail arg_2))))) then true else false))) then [(arg_1 arg_6); (arg_1 arg_7); (arg_1 arg_8)] else let arg_10::arg_11::arg_12::arg_13 = (list_tail arg_2) in let arg_9 = (list_head arg_2) in (arg_1 arg_9)::(arg_1 arg_10)::(arg_1 arg_11)::(arg_1 arg_12)::((ll_var_0 arg_1) arg_13) |}]
;;
