open HamsterML.LL
open ClosureConversionTest

let lambda_lift_prog (s : string) =
  let open HamsterML.Utils.R in
  let lambda_lift prog = run @@ ll_prog prog in
  s |> closure_conv_prog |> lambda_lift
;;

let pp_lambda_lift_prog (s : string) =
  let open HamsterML.PrinterLL in
  lambda_lift_prog s |> pretty_print_ll_prog |> print_string
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 1 |};
  [%expect {| let ll_var_0 = 1 |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 1 + 1 |};
  [%expect {| let ll_var_0 = (1 + 1) |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 10 let b = a |};
  [%expect {| 
    let ll_var_0 = 10
    let ll_var_1 = ll_var_0 
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 1 let a = a + 1 let rec a x = a 1 |};
  [%expect
    {| 
    let ll_var_0 = 1
    let ll_var_1 = (ll_var_0 + 1)
    let rec ll_var_2 arg_3 = (ll_var_2 1)
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 10 and b = 20 |};
  [%expect {| 
    let ll_var_0 = 10 and ll_var_1 = 20
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let rec a = 1 + 1 in a + 2 |};
  [%expect {| 
    let rec ll_var_0 = (1 + 1) in (ll_var_0 + 2) 
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let sum x y = let f a b = a + b in f x y|};
  [%expect
    {| 
    let ll_var_1 arg_2 arg_3 = (arg_2 + arg_3)
    let ll_var_0 arg_0 arg_1 = ((ll_var_1 arg_0) arg_1)
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let sum x y = let res = x + y in res |};
  [%expect
    {| 
    let ll_var_0 arg_0 arg_1 = let ll_var_1 = (arg_0 + arg_1) in ll_var_1
    |}]
;;

let%expect_test "fac" =
  pp_lambda_lift_prog {| let rec fac n = if n<=1 then 1 else n * fac (n-1) |};
  [%expect
    {| 
    let rec ll_var_0 arg_1 = if (arg_1 <= 1) then 1 else (arg_1 * (ll_var_0 (arg_1 - 1)))
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let sum x y = x + y let main = sum |};
  [%expect
    {| 
    let ll_var_0 arg_0 arg_1 = (arg_0 + arg_1)
    let ll_var_1 = ll_var_0
    |}]
;;

let%expect_test "mutual rec" =
  pp_lambda_lift_prog {| let rec f x = g x and g x = x + 1 |};
  [%expect
    {| 
    let rec ll_var_0 arg_2 = (ll_var_1 arg_2) and ll_var_1 arg_3 = (arg_3 + 1)
    |}]
;;

let%expect_test "weird arguments elimination" =
  pp_lambda_lift_prog {| let rofl (a,b) _ () = 1 |};
  [%expect
    {| 
    let ll_var_0 ll_arg_1 ll_arg_2 ll_arg_3 = match ll_arg_3 with
    | () -> match ll_arg_2 with
    | _ -> match ll_arg_1 with
    | (arg_0, arg_1) -> 1
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let f x = print_int x |};
  [%expect {| 
    let ll_var_0 arg_0 = (print_int arg_0)
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let main = let () = print_int 10 in 0 |};
  [%expect {| 
    let ll_var_0 = let () = (print_int 10) in 0
    |}]
;;

let%expect_test "fac_cps" =
  pp_lambda_lift_prog
    {| let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n)) |};
  [%expect
    {| 
    let ll_lam_1 arg_1 arg_2 arg_3 = (arg_2 (arg_3 * arg_1))
    let rec ll_var_0 arg_1 arg_2 = if (arg_1 = 1) then (arg_2 1) else ((ll_var_0 (arg_1 - 1)) ((ll_lam_1 arg_1) arg_2))
    |}]
;;

let%expect_test "001fac" =
  pp_lambda_lift_prog
    {| 
    let rec fac n = if n<=1 then 1 else n * fac (n-1)
    let main = let () = print_int (fac 4) in 0 
    |};
  [%expect
    {| 
    let rec ll_var_0 arg_1 = if (arg_1 <= 1) then 1 else (arg_1 * (ll_var_0 (arg_1 - 1)))
    let ll_var_1 = let () = (print_int (ll_var_0 4)) in 0 
    |}]
;;

let%expect_test "002fac" =
  pp_lambda_lift_prog
    {| 
    let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n))

    let main = let () = print_int (fac_cps 4 (fun print_int -> print_int)) in 0
    |};
  [%expect
    {| 
    let ll_lam_1 arg_1 arg_2 arg_3 = (arg_2 (arg_3 * arg_1))
    let rec ll_var_0 arg_1 arg_2 = if (arg_1 = 1) then (arg_2 1) else ((ll_var_0 (arg_1 - 1)) ((ll_lam_1 arg_1) arg_2))
    let ll_lam_3 arg_4 = arg_4
    let ll_var_2 = let () = (print_int ((ll_var_0 4) ll_lam_3)) in 0
    |}]
;;

let%expect_test "003fib" =
  pp_lambda_lift_prog
    {| 
    let rec fib_acc a b n =
      if n=1 then b
      else
        let n1 = n-1 in
        let ab = a+b in
        fib_acc b ab n1

    let rec fib n = if n<2 then n else fib (n - 1) + fib (n - 2) 

    let main =
      let () = print_int (fib_acc 0 1 4) in
      let () = print_int (fib 4) in
      0
      |};
  [%expect
    {| 
    let rec ll_var_0 arg_1 arg_2 arg_3 = if (arg_3 = 1) then arg_2 else let ll_var_1 = (arg_3 - 1) in let ll_var_2 = (arg_1 + arg_2) in (((ll_var_0 arg_2) ll_var_2) ll_var_1)
    let rec ll_var_3 arg_7 = if (arg_7 < 2) then arg_7 else ((ll_var_3 (arg_7 - 1)) + (ll_var_3 (arg_7 - 2)))
    let ll_var_4 = let () = (print_int (((ll_var_0 0) 1) 4)) in let () = (print_int (ll_var_3 4)) in 0      
    |}]
;;

let%expect_test "004manyargs" =
  pp_lambda_lift_prog
    {| 
    let wrap f = if 1 = 1 then f else f

    let test3 a b c =
      let a = print_int a in
      let b = print_int b in
      let c = print_int c in
      0

    let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

    let main =
      let rez =
          (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
            1000000000)
      in
      let () = print_int rez in
      let temp2 = wrap test3 1 10 100 in
      0
      |};
  [%expect
    {| 
      let ll_var_0 arg_0 = if (1 = 1) then arg_0 else arg_0
      let ll_var_1 arg_2 arg_3 arg_4 = let ll_var_2 = (print_int arg_2) in let ll_var_3 = (print_int arg_3) in let ll_var_4 = (print_int arg_4) in 0
      let ll_var_5 arg_9 arg_10 arg_11 arg_12 arg_13 arg_14 arg_15 arg_16 arg_17 arg_18 = (((((((((arg_9 + arg_10) + arg_11) + arg_12) + arg_13) + arg_14) + arg_15) + arg_16) + arg_17) + arg_18)
      let ll_var_6 = let ll_var_7 = (((((((((((ll_var_0 ll_var_5) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in let () = (print_int ll_var_7) in let ll_var_8 = ((((ll_var_0 ll_var_1) 1) 10) 100) in 0     
    |}]
;;

let%expect_test "005fix" =
  pp_lambda_lift_prog
    {| 
    let rec fix f x = f (fix f) x

    let fac self n = if n<=1 then 1 else n * self (n-1)

    let main =
      let () = print_int (fix fac 6) in
      0
    |};
  [%expect
    {| 
    let rec ll_var_0 arg_1 arg_2 = ((arg_1 (ll_var_0 arg_1)) arg_2)
    let ll_var_1 arg_3 arg_4 = if (arg_4 <= 1) then 1 else (arg_4 * (arg_3 (arg_4 - 1)))
    let ll_var_2 = let () = (print_int ((ll_var_0 ll_var_1) 6)) in 0    
    |}]
;;

let%expect_test "006partial" =
  pp_lambda_lift_prog
    {| 
    let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

    let foo x = foo true (foo false (foo true (foo false x)))
    let main = let () = print_int (foo 11) in 0
    |};
  [%expect
    {|
    let ll_lam_1 arg_1 = (arg_1 + 2)
    let ll_lam_2 arg_2 = (arg_2 * 10)
    let ll_var_0 arg_0 = if arg_0 then ll_lam_1 else ll_lam_2
    let ll_var_3 arg_4 = ((ll_var_0 true) ((ll_var_0 false) ((ll_var_0 true) ((ll_var_0 false) arg_4))))
    let ll_var_4 = let () = (print_int (ll_var_3 11)) in 0   
    |}]
;;

let%expect_test "006partial2" =
  pp_lambda_lift_prog
    {|
  let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + b * c

  let main =
    let foo = foo 1 in
    let foo = foo 2 in
    let foo = foo 3 in
    let () = print_int foo in
    0 
  |};
  [%expect
    {|
    let ll_var_0 arg_0 arg_1 arg_2 = let () = (print_int arg_0) in let () = (print_int arg_1) in let () = (print_int arg_2) in (arg_0 + (arg_1 * arg_2))
    let ll_var_1 = let ll_var_2 = (ll_var_0 1) in let ll_var_3 = (ll_var_2 2) in let ll_var_4 = (ll_var_3 3) in let () = (print_int ll_var_4) in 0   
    |}]
;;

let%expect_test "006partial3" =
  pp_lambda_lift_prog
    {|
    let foo a =
    let () = print_int a in fun b ->
    let () = print_int b in fun c ->
    print_int c

    let main =
      let () = foo 4 8 9 in
      0 
  |};
  [%expect
    {|
    let ll_lam_2 arg_2 = (print_int arg_2)
    let ll_lam_1 arg_1 = let () = (print_int arg_1) in ll_lam_2
    let ll_var_0 arg_0 = let () = (print_int arg_0) in ll_lam_1
    let ll_var_3 = let () = (((ll_var_0 4) 8) 9) in 0
    |}]
;;

let%expect_test "007order" =
  pp_lambda_lift_prog
    {|
  let _start () () a () b _c () d __ =
  let () = print_int (a+b) in
  let () = print_int __ in
  a*b / _c + d

  let main =
    print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
  |};
  [%expect
    {|
      let ll_var_0 ll_arg_1 ll_arg_2 arg_0 ll_arg_3 arg_1 arg_2 ll_arg_4 arg_3 arg_4 = match ll_arg_4 with
      | () -> match ll_arg_3 with
      | () -> match ll_arg_2 with
      | () -> match ll_arg_1 with
      | () -> let () = (print_int (arg_0 + arg_1)) in let () = (print_int arg_4) in (((arg_0 * arg_1) / arg_2) + arg_3)
      let ll_var_5 = (print_int (((((((((ll_var_0 (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))
    |}]
;;

let%expect_test "008ascription" =
  pp_lambda_lift_prog
    {|
  let addi = fun f g x -> (f x (g x: bool) : int)

  let main =
    let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
    0
  |};
  [%expect
    {|
      let ll_var_0 arg_0 arg_1 arg_2 = (((arg_0 arg_2) ((arg_1 arg_2) : bool)) : int)
      let ll_lam_2 arg_4 arg_5 = if arg_5 then (arg_4 + 1) else (arg_4 * 2)
      let ll_lam_3 arg_6 = ((arg_6 / 2) = 0)
      let ll_var_1 = let () = (print_int (((ll_var_0 ll_lam_2) ll_lam_3) 4)) in 0
    |}]
;;

let%expect_test "009let_poly" =
  pp_lambda_lift_prog {|
  let temp =
  let f = fun x -> x in
  (f 1, f true)
  |};
  [%expect
    {|
      let ll_var_1 arg_0 = arg_0
      let ll_var_0 = ((ll_var_1 1), (ll_var_1 true))
    |}]
;;

let%expect_test "015tuples" =
  pp_lambda_lift_prog
    {|
  let rec fix f x = f (fix f) x
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
    0
  |};
  [%expect
    {|
      let rec ll_var_0 arg_1 arg_2 = ((arg_1 (ll_var_0 arg_1)) arg_2)
      let ll_var_1 arg_3 arg_4 = let (var_5, var_6) = arg_4 in ((arg_3 var_5), (arg_3 var_6))
      let ll_lam_4 arg_10 arg_9 arg_11 arg_12 = ((arg_11 (arg_9 arg_10)) arg_12)
      let ll_lam_3 arg_9 arg_10 = ((ll_var_1 ((ll_lam_4 arg_10) arg_9)) arg_10)
      let ll_var_2 arg_8 = ((ll_var_0 ll_lam_3) arg_8)
      let ll_var_5 arg_14 arg_15 = let (var_16, var_17) = arg_14 in if (arg_15 == 0) then 1 else (var_17 (arg_15 - 1))
      let ll_var_6 arg_19 arg_20 = let (var_21, var_22) = arg_19 in if (arg_20 == 0) then 0 else (var_21 (arg_20 - 1))
      let ll_var_7 = (ll_var_2 (ll_var_5, ll_var_6))
      let rec ll_var_8 arg_27 = if (arg_27 = 0) then 1 else (ll_var_9 (arg_27 - 1)) and ll_var_9 arg_28 = if (arg_28 = 0) then 1 else (ll_var_8 (arg_28 - 1))
      let ll_var_10 = let () = (print_int (ll_var_9 1)) in let () = (print_int (ll_var_8 2)) in let (var_29, var_30) = ll_var_7 in let () = (print_int (var_30 3)) in let () = (print_int (var_29 4)) in 0
    |}]
;;

let%expect_test "016lists" =
  pp_lambda_lift_prog
    {|
  let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

  let length_tail =
    let rec helper acc xs =
    match xs with
    | [] -> acc
    | h::tl -> helper (acc + 1) tl
    in
    helper 0

  let rec map f xs =
    match xs with
    | [] -> []
    | a::[] -> [f a]
    | a::b::[] -> [f a; f b]
    | a::b::c::[] -> [f a; f b; f c]
    | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

  let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

  let concat =
    let rec helper xs =
      match xs with
      | [] -> []
      | h::tl -> append h (helper tl)
    in helper

  let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

  let rec cartesian xs ys =
    match xs with
    | [] -> []
    | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

  let main =
    let () = iter print_int [1;2;3] in
    let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
    0
  |};
  [%expect
    {|
      let rec ll_var_0 arg_1 = match arg_1 with
      | arg_2::arg_3 -> (1 + (ll_var_0 arg_3))
      | [] -> 0
      let rec ll_var_2 arg_5 arg_6 = match arg_6 with
      | arg_7::arg_8 -> ((ll_var_2 (arg_5 + 1)) arg_8)
      | [] -> arg_5
      let ll_var_1 = (ll_var_2 0)
      let rec ll_var_3 arg_11 arg_12 = match arg_12 with
      | arg_19::arg_20::arg_21::arg_22::arg_23 -> (arg_11 arg_19)::(arg_11 arg_20)::(arg_11 arg_21)::(arg_11 arg_22)::((ll_var_3 arg_11) arg_23)
      | arg_16::arg_17::arg_18::[] -> [(arg_11 arg_16); (arg_11 arg_17); (arg_11 arg_18)]
      | arg_14::arg_15::[] -> [(arg_11 arg_14); (arg_11 arg_15)]
      | arg_13::[] -> [(arg_11 arg_13)]
      | [] -> []
      let rec ll_var_4 arg_25 arg_26 = match arg_25 with
      | arg_27::arg_25 -> arg_27::((ll_var_4 arg_25) arg_26)
      | [] -> arg_26
      let rec ll_var_6 arg_29 = match arg_29 with
      | arg_30::arg_31 -> ((ll_var_4 arg_30) (ll_var_6 arg_31))
      | [] -> []
      let ll_var_5 = ll_var_6
      let rec ll_var_7 arg_34 arg_35 = match arg_35 with
      | arg_36::arg_37 -> let () = (arg_34 arg_36) in ((ll_var_7 arg_34) arg_37)
      | [] -> ()
      let ll_lam_9 arg_41 arg_43 = (arg_41, arg_43)
      let rec ll_var_8 arg_39 arg_40 = match arg_39 with
      | arg_41::arg_42 -> ((ll_var_4 ((ll_var_3 (ll_lam_9 arg_41)) arg_40)) ((ll_var_8 arg_42) arg_40))
      | [] -> []
      let ll_var_10 = let () = ((ll_var_7 print_int) [1; 2; 3]) in let () = (print_int (ll_var_0 ((ll_var_8 [1; 2]) [1; 2; 3; 4]))) in 0 
    |}]
;;
