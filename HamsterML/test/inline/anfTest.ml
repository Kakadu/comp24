open MatchEliminationTest

let anf_prog (s : string) =
  let open HamsterML.Utils.R in
  let convert prog = run @@ HamsterML.Anf.anf_prog prog in
  s |> match_elim_prog |> convert
;;

let pp_anf_prog (s : string) =
  let open HamsterML.PrinterAnf in
  anf_prog s |> pretty_print_anf |> print_string
;;

let%expect_test "001fac" =
  pp_anf_prog
    {| 
    let rec fac n = if n<=1 then 1 else n * fac (n-1)
    let main = let () = print_int (fac 4) in 0 
    |};
  [%expect
    {|   
      let rec ll_var_0 arg_1 = let anf_0 = ( <= ) arg_1 1 in
      let anf_4 = if anf_0 then 1 else let anf_1 = ( - ) arg_1 1 in
      let anf_2 = ll_var_0 anf_1 in
      let anf_3 = ( * ) arg_1 anf_2 in
      anf_3 in
      anf_4

      let ll_var_1  = let anf_5 = ll_var_0 4 in
      let anf_6 = print_int anf_5 in
      let () = anf_6 in
      0
    |}]
;;

let%expect_test "002fac" =
  pp_anf_prog
    {| 
    let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n))

    let main = let () = print_int (fac_cps 4 (fun print_int -> print_int)) in 0
    |};
  [%expect
    {| 
      let ll_lam_1 arg_1 arg_2 arg_3 = let anf_0 = ( * ) arg_3 arg_1 in
      let anf_1 = arg_2 anf_0 in
      anf_1

      let rec ll_var_0 arg_1 arg_2 = let anf_2 = ( = ) arg_1 1 in
      let anf_7 = if anf_2 then let anf_3 = arg_2 1 in
      anf_3 else let anf_4 = ll_lam_1 arg_1 arg_2 in
      let anf_5 = ( - ) arg_1 1 in
      let anf_6 = ll_var_0 anf_5 anf_4 in
      anf_6 in
      anf_7

      let ll_lam_3 arg_4 = arg_4

      let ll_var_2  = let anf_8 = ll_var_0 4 ll_lam_3 in
      let anf_9 = print_int anf_8 in
      let () = anf_9 in
      0
    |}]
;;

let%expect_test "003fib" =
  pp_anf_prog
    {| 
    let rec fib_acc a b n =
    if n=1 then b
    else
      let n1 = n-1 in
      let ab = a+b in
      fib_acc b ab n1

  let rec fib n =
    if n<2
    then n
    else fib (n - 1) + fib (n - 2) 

  let main =
    let () = print_int (fib_acc 0 1 4) in
    let () = print_int (fib 4) in
    0
    |};
  [%expect
    {|
    let rec ll_var_0 arg_1 arg_2 arg_3 = let anf_0 = ( = ) arg_3 1 in
    let anf_4 = if anf_0 then arg_2 else let anf_1 = ( - ) arg_3 1 in
    let ll_var_1 = anf_1 in
    let anf_2 = ( + ) arg_1 arg_2 in
    let ll_var_2 = anf_2 in
    let anf_3 = ll_var_0 arg_2 ll_var_2 ll_var_1 in
    anf_3 in
    anf_4

    let rec ll_var_3 arg_7 = let anf_5 = ( < ) arg_7 2 in
    let anf_11 = if anf_5 then arg_7 else let anf_6 = ( - ) arg_7 2 in
    let anf_7 = ll_var_3 anf_6 in
    let anf_8 = ( - ) arg_7 1 in
    let anf_9 = ll_var_3 anf_8 in
    let anf_10 = ( + ) anf_9 anf_7 in
    anf_10 in
    anf_11

    let ll_var_4  = let anf_12 = ll_var_0 0 1 4 in
    let anf_13 = print_int anf_12 in
    let () = anf_13 in
    let anf_14 = ll_var_3 4 in
    let anf_15 = print_int anf_14 in
    let () = anf_15 in
    0
    |}]
;;

let%expect_test "004manyargs" =
  pp_anf_prog
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
      let ll_var_0 arg_0 = let anf_0 = ( = ) 1 1 in
      let anf_1 = if anf_0 then arg_0 else arg_0 in
      anf_1

      let ll_var_1 arg_2 arg_3 arg_4 = let anf_2 = print_int arg_2 in
      let ll_var_2 = anf_2 in
      let anf_3 = print_int arg_3 in
      let ll_var_3 = anf_3 in
      let anf_4 = print_int arg_4 in
      let ll_var_4 = anf_4 in
      0

      let ll_var_5 arg_9 arg_10 arg_11 arg_12 arg_13 arg_14 arg_15 arg_16 arg_17 arg_18 = let anf_5 = ( + ) arg_9 arg_10 in
      let anf_6 = ( + ) anf_5 arg_11 in
      let anf_7 = ( + ) anf_6 arg_12 in
      let anf_8 = ( + ) anf_7 arg_13 in
      let anf_9 = ( + ) anf_8 arg_14 in
      let anf_10 = ( + ) anf_9 arg_15 in
      let anf_11 = ( + ) anf_10 arg_16 in
      let anf_12 = ( + ) anf_11 arg_17 in
      let anf_13 = ( + ) anf_12 arg_18 in
      anf_13

      let ll_var_6  = let anf_14 = ll_var_0 ll_var_5 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
      let ll_var_7 = anf_14 in
      let anf_15 = print_int ll_var_7 in
      let () = anf_15 in
      let anf_16 = ll_var_0 ll_var_1 1 10 100 in
      let ll_var_8 = anf_16 in
      0  
    |}]
;;

let%expect_test "005fix" =
  pp_anf_prog
    {| 
    let rec fix f x = f (fix f) x

    let fac self n = if n<=1 then 1 else n * self (n-1)

    let main =
      let () = print_int (fix fac 6) in
      0
    |};
  [%expect
    {| 
      let rec ll_var_0 arg_1 arg_2 = let anf_0 = ll_var_0 arg_1 in
      let anf_1 = arg_1 anf_0 arg_2 in
      anf_1

      let ll_var_1 arg_3 arg_4 = let anf_2 = ( <= ) arg_4 1 in
      let anf_6 = if anf_2 then 1 else let anf_3 = ( - ) arg_4 1 in
      let anf_4 = arg_3 anf_3 in
      let anf_5 = ( * ) arg_4 anf_4 in
      anf_5 in
      anf_6

      let ll_var_2  = let anf_7 = ll_var_0 ll_var_1 6 in
      let anf_8 = print_int anf_7 in
      let () = anf_8 in
      0       
    |}]
;;

let%expect_test "006partial" =
  pp_anf_prog
    {| 
    let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

    let foo x = foo true (foo false (foo true (foo false x)))
    let main = let () = print_int (foo 11) in 0
    |};
  [%expect
    {|
      let ll_lam_1 arg_1 = let anf_0 = ( + ) arg_1 2 in
      anf_0

      let ll_lam_2 arg_2 = let anf_1 = ( * ) arg_2 10 in
      anf_1

      let ll_var_0 arg_0 = let anf_2 = if arg_0 then ll_lam_1 else ll_lam_2 in
      anf_2

      let ll_var_3 arg_4 = let anf_3 = ll_var_0 false arg_4 in
      let anf_4 = ll_var_0 true anf_3 in
      let anf_5 = ll_var_0 false anf_4 in
      let anf_6 = ll_var_0 true anf_5 in
      anf_6

      let ll_var_4  = let anf_7 = ll_var_3 11 in
      let anf_8 = print_int anf_7 in
      let () = anf_8 in
      0  
    |}]
;;

let%expect_test "006partial2" =
  pp_anf_prog
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
      let ll_var_0 arg_0 arg_1 arg_2 = let anf_0 = print_int arg_0 in
      let () = anf_0 in
      let anf_1 = print_int arg_1 in
      let () = anf_1 in
      let anf_2 = print_int arg_2 in
      let () = anf_2 in
      let anf_3 = ( * ) arg_1 arg_2 in
      let anf_4 = ( + ) arg_0 anf_3 in
      anf_4

      let ll_var_1  = let anf_5 = ll_var_0 1 in
      let ll_var_2 = anf_5 in
      let anf_6 = ll_var_2 2 in
      let ll_var_3 = anf_6 in
      let anf_7 = ll_var_3 3 in
      let ll_var_4 = anf_7 in
      let anf_8 = print_int ll_var_4 in
      let () = anf_8 in
      0    
    |}]
;;

let%expect_test "006partial3" =
  pp_anf_prog
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
      let ll_lam_2 arg_2 = let anf_0 = print_int arg_2 in
      anf_0

      let ll_lam_1 arg_1 = let anf_1 = print_int arg_1 in
      let () = anf_1 in
      ll_lam_2

      let ll_var_0 arg_0 = let anf_2 = print_int arg_0 in
      let () = anf_2 in
      ll_lam_1

      let ll_var_3  = let anf_3 = ll_var_0 4 8 9 in
      let () = anf_3 in
      0
    |}]
;;

let%expect_test "007order" =
  pp_anf_prog
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
      let ll_var_0 ll_arg_1 ll_arg_2 arg_0 ll_arg_3 arg_1 arg_2 ll_arg_4 arg_3 arg_4 = let anf_0 = ( + ) arg_0 arg_1 in
      let anf_1 = print_int anf_0 in
      let () = anf_1 in
      let anf_2 = print_int arg_4 in
      let () = anf_2 in
      let anf_3 = ( * ) arg_0 arg_1 in
      let anf_4 = ( / ) anf_3 arg_2 in
      let anf_5 = ( + ) anf_4 arg_3 in
      anf_5

      let ll_var_5  = let anf_6 = - 555555 in
      let anf_7 = - 1 in
      let anf_8 = print_int anf_7 in
      let anf_9 = print_int 4 in
      let anf_10 = print_int 2 in
      let anf_11 = print_int 1 in
      let anf_12 = ll_var_0 anf_11 anf_10 3 anf_9 100 1000 anf_8 10000 anf_6 in
      let anf_13 = print_int anf_12 in
      anf_13  
    |}]
;;

let%expect_test "008ascription" =
  pp_anf_prog
    {|
  let addi = fun f g x -> (f x (g x: bool) : int)

  let main =
    let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
    0
  |};
  [%expect
    {|
      let ll_var_0 arg_0 arg_1 arg_2 = let anf_0 = arg_1 arg_2 in
      let anf_1 = arg_0 arg_2 anf_0 in
      anf_1

      let ll_lam_2 arg_4 arg_5 = let anf_4 = if arg_5 then let anf_2 = ( + ) arg_4 1 in
      anf_2 else let anf_3 = ( * ) arg_4 2 in
      anf_3 in
      anf_4

      let ll_lam_3 arg_6 = let anf_5 = ( / ) arg_6 2 in
      let anf_6 = ( = ) anf_5 0 in
      anf_6

      let ll_var_1  = let anf_7 = ll_var_0 ll_lam_2 ll_lam_3 4 in
      let anf_8 = print_int anf_7 in
      let () = anf_8 in
      0     
    |}]
;;

let%expect_test "009let_poly" =
  pp_anf_prog {|
  let temp =
  let f = fun x -> x in
  (f 1, f true)
  |};
  [%expect
    {|
      let ll_var_1 arg_0 = arg_0

      let ll_var_0  = let anf_0 = ll_var_1 1 in
      let anf_1 = ll_var_1 true in
      let anf_2 = (anf_0, anf_1) in
      anf_2        
    |}]
;;
