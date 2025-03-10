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
(* 
let%expect_test _ =
  pp_anf_prog {| let rec fac n = if n<=1 then 1 else n * fac (n-1) |};
  [%expect
    {| 
      let rec anf_var_0 arg_1 = let anf_8 = ( <= ) arg_1 in
      let anf_2 = anf_8 1 in
      let anf_4 = ( * ) arg_1 in
      let anf_7 = ( - ) arg_1 in
      let anf_6 = anf_7 1 in
      let anf_5 = anf_var_0 anf_6 in
      let anf_3 = anf_4 anf_5 in
      let anf_1 = if anf_2 then 1 else anf_3 in
      anf_1 |}]
;; *)


 let%expect_test "001fac" =
  pp_anf_prog
    {| 
    let rec fac n = if n<=1 then 1 else n * fac (n-1)
    let main = let () = print_int (fac 4) in 0 
    |};
  [%expect
    {| 
      let rec ll_var_0 arg_1 = let anf_7 = ( <= ) arg_1 in
      let anf_1 = anf_7 1 in
      let anf_3 = ( * ) arg_1 in
      let anf_6 = ( - ) arg_1 in
      let anf_5 = anf_6 1 in
      let anf_4 = ll_var_0 anf_5 in
      let anf_2 = anf_3 anf_4 in
      let anf_0 = if anf_1 then 1 else anf_2 in
      anf_0

      let ll_var_1  = let anf_9 = ll_var_0 4 in
      let anf_8 = print_int anf_9 in
      let () = anf_8 in
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
      let ll_lam_1 arg_1 arg_2 arg_3 = let anf_2 = ( * ) arg_3 in
      let anf_1 = anf_2 arg_1 in
      let anf_0 = arg_2 anf_1 in
      anf_0

      let rec ll_var_0 arg_1 arg_2 = let anf_12 = ( = ) arg_1 in
      let anf_4 = anf_12 1 in
      let anf_5 = arg_2 1 in
      let anf_11 = ( - ) arg_1 in
      let anf_10 = anf_11 1 in
      let anf_7 = ll_var_0 anf_10 in
      let anf_9 = ll_lam_1 arg_1 in
      let anf_8 = anf_9 arg_2 in
      let anf_6 = anf_7 anf_8 in
      let anf_3 = if anf_4 then anf_5 else anf_6 in
      anf_3

      let ll_lam_3 arg_4 = arg_4

      let ll_var_2  = let anf_15 = ll_var_0 4 in
      let anf_14 = anf_15 ll_lam_3 in
      let anf_13 = print_int anf_14 in
      let () = anf_13 in
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
      let ll_var_0 arg_0 = let anf_2 = ( = ) 1 in
      let anf_1 = anf_2 1 in
      let anf_0 = if anf_1 then arg_0 else arg_0 in
      anf_0

      let ll_var_1 arg_2 arg_3 arg_4 = let anf_3 = print_int arg_2 in
      let anf_4 = print_int arg_3 in
      let anf_5 = print_int arg_4 in
      let ll_var_4 = anf_5 in
      0

      let ll_var_5 arg_9 arg_10 arg_11 arg_12 arg_13 arg_14 arg_15 arg_16 arg_17 arg_18 = let anf_23 = ( + ) arg_9 in
      let anf_22 = anf_23 arg_10 in
      let anf_21 = ( + ) anf_22 in
      let anf_20 = anf_21 arg_11 in
      let anf_19 = ( + ) anf_20 in
      let anf_18 = anf_19 arg_12 in
      let anf_17 = ( + ) anf_18 in
      let anf_16 = anf_17 arg_13 in
      let anf_15 = ( + ) anf_16 in
      let anf_14 = anf_15 arg_14 in
      let anf_13 = ( + ) anf_14 in
      let anf_12 = anf_13 arg_15 in
      let anf_11 = ( + ) anf_12 in
      let anf_10 = anf_11 arg_16 in
      let anf_9 = ( + ) anf_10 in
      let anf_8 = anf_9 arg_17 in
      let anf_7 = ( + ) anf_8 in
      let anf_6 = anf_7 arg_18 in
      anf_6

      let ll_var_6  = let anf_39 = ll_var_0 ll_var_5 in
      let anf_38 = anf_39 1 in
      let anf_37 = anf_38 10 in
      let anf_36 = anf_37 100 in
      let anf_35 = anf_36 1000 in
      let anf_34 = anf_35 10000 in
      let anf_33 = anf_34 100000 in
      let anf_32 = anf_33 1000000 in
      let anf_31 = anf_32 10000000 in
      let anf_30 = anf_31 100000000 in
      let anf_24 = anf_30 1000000000 in
      let anf_25 = print_int ll_var_7 in
      let anf_29 = ll_var_0 ll_var_1 in
      let anf_28 = anf_29 1 in
      let anf_27 = anf_28 10 in
      let anf_26 = anf_27 100 in
      let ll_var_8 = anf_26 in
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
      let rec ll_var_0 arg_1 arg_2 = let anf_2 = ll_var_0 arg_1 in
      let anf_1 = arg_1 anf_2 in
      let anf_0 = anf_1 arg_2 in
      anf_0

      let ll_var_1 arg_3 arg_4 = let anf_10 = ( <= ) arg_4 in
      let anf_4 = anf_10 1 in
      let anf_6 = ( * ) arg_4 in
      let anf_9 = ( - ) arg_4 in
      let anf_8 = anf_9 1 in
      let anf_7 = arg_3 anf_8 in
      let anf_5 = anf_6 anf_7 in
      let anf_3 = if anf_4 then 1 else anf_5 in
      anf_3

      let ll_var_2  = let anf_13 = ll_var_0 ll_var_1 in
      let anf_12 = anf_13 6 in
      let anf_11 = print_int anf_12 in
      let () = anf_11 in
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
      let ll_lam_1 arg_1 = let anf_1 = ( + ) arg_1 in
      let anf_0 = anf_1 2 in
      anf_0

      let ll_lam_2 arg_2 = let anf_3 = ( * ) arg_2 in
      let anf_2 = anf_3 10 in
      anf_2

      let ll_var_0 arg_0 = let anf_4 = if arg_0 then ll_lam_1 else ll_lam_2 in
      anf_4

      let ll_var_3 arg_4 = let anf_6 = ll_var_0 true in
      let anf_8 = ll_var_0 false in
      let anf_10 = ll_var_0 true in
      let anf_12 = ll_var_0 false in
      let anf_11 = anf_12 arg_4 in
      let anf_9 = anf_10 anf_11 in
      let anf_7 = anf_8 anf_9 in
      let anf_5 = anf_6 anf_7 in
      anf_5

      let ll_var_4  = let anf_14 = ll_var_3 11 in
      let anf_13 = print_int anf_14 in
      let () = anf_13 in
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
      let anf_1 = print_int arg_1 in
      let anf_2 = print_int arg_2 in
      let anf_4 = ( + ) arg_0 in
      let anf_6 = ( * ) arg_1 in
      let anf_5 = anf_6 arg_2 in
      let anf_3 = anf_4 anf_5 in
      let () = anf_2 in
      anf_3

      let ll_var_1  = let anf_7 = ll_var_0 1 in
      let anf_8 = ll_var_2 2 in
      let anf_9 = ll_var_3 3 in
      let anf_10 = print_int ll_var_4 in
      let () = anf_10 in
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

      let ll_var_3  = let anf_5 = ll_var_0 4 in
      let anf_4 = anf_5 8 in
      let anf_3 = anf_4 9 in
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
    let ll_var_0 ll_arg_1 ll_arg_2 arg_0 ll_arg_3 arg_1 arg_2 ll_arg_4 arg_3 arg_4 = let anf_9 = ( + ) arg_0 in
    let anf_8 = anf_9 arg_1 in
    let anf_0 = print_int anf_8 in
    let anf_1 = print_int arg_4 in
    let anf_7 = ( * ) arg_0 in
    let anf_6 = anf_7 arg_1 in
    let anf_5 = ( / ) anf_6 in
    let anf_4 = anf_5 arg_2 in
    let anf_3 = ( + ) anf_4 in
    let anf_2 = anf_3 arg_3 in
    let () = anf_1 in
    anf_2

    let ll_var_5  = let anf_25 = print_int 1 in
    let anf_23 = ll_var_0 anf_25 in
    let anf_24 = print_int 2 in
    let anf_22 = anf_23 anf_24 in
    let anf_20 = anf_22 3 in
    let anf_21 = print_int 4 in
    let anf_19 = anf_20 anf_21 in
    let anf_18 = anf_19 100 in
    let anf_15 = anf_18 1000 in
    let anf_17 = - 1 in
    let anf_16 = print_int anf_17 in
    let anf_14 = anf_15 anf_16 in
    let anf_12 = anf_14 10000 in
    let anf_13 = - 555555 in
    let anf_11 = anf_12 anf_13 in
    let anf_10 = print_int anf_11 in
    anf_10
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
      let ll_var_0 arg_0 arg_1 arg_2 = let anf_1 = arg_0 arg_2 in
      let anf_2 = arg_1 arg_2 in
      let anf_0 = anf_1 anf_2 in
      anf_0

      let ll_lam_2 arg_4 arg_5 = let anf_7 = ( + ) arg_4 in
      let anf_4 = anf_7 1 in
      let anf_6 = ( * ) arg_4 in
      let anf_5 = anf_6 2 in
      let anf_3 = if arg_5 then anf_4 else anf_5 in
      anf_3

      let ll_lam_3 arg_6 = let anf_11 = ( / ) arg_6 in
      let anf_10 = anf_11 2 in
      let anf_9 = ( = ) anf_10 in
      let anf_8 = anf_9 0 in
      anf_8

      let ll_var_1  = let anf_15 = ll_var_0 ll_lam_2 in
      let anf_14 = anf_15 ll_lam_3 in
      let anf_13 = anf_14 4 in
      let anf_12 = print_int anf_13 in
      let () = anf_12 in
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
      (anf_0, anf_1)     
    |}]
;;

let%expect_test "016lists" =
  pp_anf_prog
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
      let rec ll_var_0 arg_1 = let anf_8 = ( = ) 0 in
      let anf_9 = list_length arg_1 in
      let anf_7 = anf_8 anf_9 in
      let anf_1 = if anf_7 then true else false in
      let anf_2 = list_tail arg_1 in
      let anf_3 = list_head arg_1 in
      let anf_5 = ( + ) 1 in
      let anf_6 = ll_var_0 arg_3 in
      let anf_4 = anf_5 anf_6 in
      let arg_2 = anf_3 in
      anf_4

      let rec ll_var_2 arg_5 arg_6 = let anf_19 = ( = ) 0 in
      let anf_20 = list_length arg_6 in
      let anf_18 = anf_19 anf_20 in
      let anf_11 = if anf_18 then true else false in
      let anf_12 = list_tail arg_6 in
      let anf_13 = list_head arg_6 in
      let anf_17 = ( + ) arg_5 in
      let anf_16 = anf_17 1 in
      let anf_15 = ll_var_2 anf_16 in
      let anf_14 = anf_15 arg_8 in
      let arg_7 = anf_13 in
      anf_14

      let ll_var_1  = let anf_21 = ll_var_2 0 in
      anf_21

      let rec ll_var_3 arg_11 arg_12 = let anf_45 = ( = ) 0 in
      let anf_46 = list_length arg_12 in
      let anf_44 = anf_45 anf_46 in
      let anf_23 = if anf_44 then true else false in
      let anf_24 = list_tail arg_12 in
      let anf_25 = list_head arg_12 in
      let anf_27 = list_head arg_12 in
      let anf_28 = list_tail arg_12 in
      let anf_43 = list_length arg_12 in
      let anf_42 = ( >= ) anf_43 in
      let anf_41 = anf_42 2 in
      let anf_37 = ( && ) anf_41 in
      let anf_39 = ( = ) arg_13 in
      let anf_40 = list_head arg_12 in
      let anf_38 = anf_39 anf_40 in
      let anf_36 = anf_37 anf_38 in
      let anf_30 = ( && ) anf_36 in
      let anf_33 = ( = ) 0 in
      let anf_35 = list_tail arg_12 in
      let anf_34 = list_length anf_35 in
      let anf_32 = anf_33 anf_34 in
      let anf_31 = if anf_32 then true else false in
      let anf_29 = anf_30 anf_31 in
      let [] = anf_28 in
      anf_29

      let rec ll_var_4 arg_25 arg_26 = let anf_55 = ( = ) 0 in
      let anf_56 = list_length arg_25 in
      let anf_54 = anf_55 anf_56 in
      let anf_48 = if anf_54 then true else false in
      let anf_49 = list_tail arg_25 in
      let anf_50 = list_head arg_25 in
      let anf_53 = ll_var_4 arg_25 in
      let anf_52 = anf_53 arg_26 in
      let anf_51 = arg_27 :: anf_52 in
      let arg_27 = anf_50 in
      anf_51

      let rec ll_var_6 arg_29 = let anf_65 = ( = ) 0 in
      let anf_66 = list_length arg_29 in
      let anf_64 = anf_65 anf_66 in
      let anf_58 = if anf_64 then true else false in
      let anf_59 = list_tail arg_29 in
      let anf_60 = list_head arg_29 in
      let anf_62 = ll_var_4 arg_30 in
      let anf_63 = ll_var_6 arg_31 in
      let anf_61 = anf_62 anf_63 in
      let arg_30 = anf_60 in
      anf_61

      let ll_var_5  = ll_var_6

      let rec ll_var_7 arg_34 arg_35 = let anf_75 = ( = ) 0 in
      let anf_76 = list_length arg_35 in
      let anf_74 = anf_75 anf_76 in
      let anf_68 = if anf_74 then true else false in
      let anf_69 = list_tail arg_35 in
      let anf_70 = list_head arg_35 in
      let anf_71 = arg_34 arg_36 in
      let anf_73 = ll_var_7 arg_34 in
      let anf_72 = anf_73 arg_37 in
      let () = anf_71 in
      anf_72

      let ll_lam_9 arg_41 arg_43 = (arg_41, arg_43)

      let rec ll_var_8 arg_39 arg_40 = let anf_89 = ( = ) 0 in
      let anf_90 = list_length arg_39 in
      let anf_88 = anf_89 anf_90 in
      let anf_78 = if anf_88 then true else false in
      let anf_79 = list_tail arg_39 in
      let anf_80 = list_head arg_39 in
      let anf_87 = ll_lam_9 arg_41 in
      let anf_86 = ll_var_3 anf_87 in
      let anf_85 = anf_86 arg_40 in
      let anf_82 = ll_var_4 anf_85 in
      let anf_84 = ll_var_8 arg_42 in
      let anf_83 = anf_84 arg_40 in
      let anf_81 = anf_82 anf_83 in
      let arg_41 = anf_80 in
      anf_81

      let ll_var_10  = let anf_96 = ll_var_7 print_int in
      let anf_91 = anf_96 [1; 2; 3] in
      let anf_95 = ll_var_8 [1; 2] in
      let anf_94 = anf_95 [1; 2; 3; 4] in
      let anf_93 = ll_var_0 anf_94 in
      let anf_92 = print_int anf_93 in
      let () = anf_92 in
      0    
  |}]
;; 
