open HamsterML.CC
open ParserTest

let closure_conv_prog (s : string) =
  let open HamsterML.Utils.R in
  let prog = parse_prog s in
  let alpha_convert prog = run @@ HamsterML.AC.convert_prog prog in
  let closure_convert prog = cc_prog prog in
  prog |> alpha_convert |> closure_convert
;;

let pp_closure_conv_prog (s : string) =
  let open HamsterML.PrinterAst in
  closure_conv_prog s |> pretty_print_prog |> print_string
;;

let%expect_test _ =
  pp_closure_conv_prog {| let a = 1 |};
  [%expect {| let var_0 = 1 |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x y -> x + y |};
  [%expect {| let var_2 arg_0 arg_1 = ((( + ) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x -> fun y -> x + y |};
  [%expect {| let var_2 arg_1 arg_0 = ((( + ) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x y -> fun a b -> a b x y |};
  [%expect {| let var_4 arg_2 arg_3 arg_0 arg_1 = (((arg_2 arg_3) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x -> fun y -> fun a -> fun b -> a b x y |};
  [%expect {| let var_4 arg_3 arg_2 arg_1 arg_0 = (((arg_2 arg_3) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f a b = let g = a in g + b |};
  [%expect
    {| let var_3 arg_0 arg_1 = let var_2 arg_0 = arg_0 in ((( + ) (var_2 arg_0)) arg_1) |}]
;;

(* 001fac.ml *)

let%expect_test _ =
  pp_closure_conv_prog {| let rec fac n = if n<=1 then 1 else n * fac (n-1) |};
  [%expect
    {| let rec var_0 arg_1 = if ((( <= ) arg_1) 1) then 1 else ((( * ) arg_1) (var_0 ((( - ) arg_1) 1))) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let main = let () = print_int (fac 4) in 0 |};
  [%expect {| let var_0 = let () = (print_int (fac 4)) in 0 |}]
;;

(* 002fac.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {| let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n)) |};
  [%expect
    {| let rec var_0 arg_1 arg_2 = if ((( = ) arg_1) 1) then (arg_2 1) else ((var_0 ((( - ) arg_1) 1)) (((fun arg_1 arg_2 arg_3 -> (arg_2 ((( * ) arg_3) arg_1))) arg_1) arg_2)) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog
    {| let main = let () = print_int (fac_cps 4 (fun print_int -> print_int)) in 0 |};
  [%expect {| let var_1 = let () = (print_int ((fac_cps 4) (fun arg_0 -> arg_0))) in 0 |}]
;;

(* 003fib.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {| 
      let rec fib_acc a b n = if n=1 then b else let n1 = n-1 in let ab = a+b in fib_acc b ab n1
      let rec fib n = if n<2 then n else fib (n - 1) + fib (n - 2)
      let main = let () = print_int (fib_acc 0 1 4) in let () = print_int (fib 4) in 0 
    |};
  [%expect
    {| 
      let rec var_0 arg_1 arg_2 arg_3 = if ((( = ) arg_3) 1) then arg_2 else let var_4 arg_3 = ((( - ) arg_3) 1) in let var_5 arg_1 arg_2 = ((( + ) arg_1) arg_2) in (((var_0 arg_2) ((var_5 arg_1) arg_2)) (var_4 arg_3))
      let rec var_6 arg_7 = if ((( < ) arg_7) 2) then arg_7 else ((( + ) (var_6 ((( - ) arg_7) 1))) (var_6 ((( - ) arg_7) 2)))
      let var_8 = let () = (print_int (((var_0 0) 1) 4)) in let () = (print_int (var_6 4)) in 0
    |}]
;;

(* 004manyargs.ml *)

let%expect_test _ =
  pp_closure_conv_prog
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
  [%expect {| defines as functions |}]
;;

(* 005fix.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {|
      let rec fix f x = f (fix f) x

      let fac self n = if n<=1 then 1 else n * self (n-1)

      let main =
        let () = print_int (fix fac 6) in
        0
    |};
  [%expect
    {|
      let rec var_0 arg_1 arg_2 = ((arg_1 (var_0 arg_1)) arg_2)
      let var_5 arg_3 arg_4 = if ((( <= ) arg_4) 1) then 1 else ((( * ) arg_4) (arg_3 ((( - ) arg_4) 1)))
      let var_6 = let () = (print_int ((var_0 var_5) 6)) in 0
    |}]
;;

(* 006partial.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {|   
      let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

      let foo x = foo true (foo false (foo true (foo false x)))
      let main =
        let () = print_int (foo 11) in
        0
    |};
  [%expect {| type error |}]
;;

(* 007order.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {|
      let _start () () a () b _c () d __ =
        let () = print_int (a+b) in
        let () = print_int __ in
        a*b / _c + d


      let main =
        print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
    |};
  [%expect {| defines as functions |}]
;;

(* 008ascription.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {|
      let addi = fun f g x -> (f x (g x: bool) : int)

      let main =
        let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
        0
    |};
  [%expect
    {| 
      let var_3 arg_0 arg_1 arg_2 = (((arg_0 arg_2) ((arg_1 arg_2) : bool)) : int)
      let var_7 = let () = (print_int (((var_3 (fun arg_4 arg_5 -> if arg_5 then ((( + ) arg_4) 1) else ((( * ) arg_4) 2))) (fun arg_6 -> ((( = ) ((( / ) arg_6) 2)) 0))) 4)) in 0       
    |}]
;;

(* 009let_poly.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {|
      let temp =
        let f = fun x -> x in
        (f 1, f true)
    |};
  [%expect {|let var_2 = let var_1 arg_0 = arg_0 in ((var_1 1), (var_1 true)) |}]
;;

(* 015tuples.ml *)

let%expect_test _ =
  pp_closure_conv_prog
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
  [%expect {| syntax error |}]
;;

(* 016lists.ml *)

let%expect_test _ =
  pp_closure_conv_prog
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
  [%expect {| type error |}]
;;
