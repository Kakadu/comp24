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
;;

let%expect_test "001fac" =
  pp_anf_prog
    {| 
    let rec fac n = if n<=1 then 1 else n * fac (n-1)
    let main = let () = print_int (fac 4) in 0 
    |};
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
      anf_1

      let anf_var_9  = let anf_11 = anf_var_0 4 in
      let anf_10 = print_int anf_11 in
      let () = anf_10 in
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
      let anf_var_0 arg_1 arg_2 arg_3 = let anf_3 = ( * ) arg_3 in
      let anf_2 = anf_3 arg_1 in
      let anf_1 = arg_2 anf_2 in
      anf_1

      let rec anf_var_4 arg_1 arg_2 = let anf_14 = ( = ) arg_1 in
      let anf_6 = anf_14 1 in
      let anf_7 = arg_2 1 in
      let anf_13 = ( - ) arg_1 in
      let anf_12 = anf_13 1 in
      let anf_9 = anf_var_4 anf_12 in
      let anf_11 = anf_var_0 arg_1 in
      let anf_10 = anf_11 arg_2 in
      let anf_8 = anf_9 anf_10 in
      let anf_5 = if anf_6 then anf_7 else anf_8 in
      anf_5

      let anf_var_15 arg_4 = arg_4

      let anf_var_16  = let anf_19 = anf_var_4 4 in
      let anf_18 = anf_19 anf_var_15 in
      let anf_17 = print_int anf_18 in
      let () = anf_17 in
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
    let anf_var_0 arg_0 = let anf_3 = ( = ) 1 in
    let anf_2 = anf_3 1 in
    let anf_1 = if anf_2 then arg_0 else arg_0 in
    anf_1

    let anf_var_4 arg_2 arg_3 arg_4 = let anf_6 = print_int arg_2 in
    let anf_8 = print_int arg_3 in
    let anf_10 = print_int arg_4 in
    let anf_9 = anf_10 in
    0

    let anf_var_11 arg_9 arg_10 arg_11 arg_12 arg_13 arg_14 arg_15 arg_16 arg_17 arg_18 = let anf_29 = ( + ) arg_9 in
    let anf_28 = anf_29 arg_10 in
    let anf_27 = ( + ) anf_28 in
    let anf_26 = anf_27 arg_11 in
    let anf_25 = ( + ) anf_26 in
    let anf_24 = anf_25 arg_12 in
    let anf_23 = ( + ) anf_24 in
    let anf_22 = anf_23 arg_13 in
    let anf_21 = ( + ) anf_22 in
    let anf_20 = anf_21 arg_14 in
    let anf_19 = ( + ) anf_20 in
    let anf_18 = anf_19 arg_15 in
    let anf_17 = ( + ) anf_18 in
    let anf_16 = anf_17 arg_16 in
    let anf_15 = ( + ) anf_16 in
    let anf_14 = anf_15 arg_17 in
    let anf_13 = ( + ) anf_14 in
    let anf_12 = anf_13 arg_18 in
    anf_12

    let anf_var_30  = let anf_48 = anf_var_0 anf_var_11 in
    let anf_47 = anf_48 1 in
    let anf_46 = anf_47 10 in
    let anf_45 = anf_46 100 in
    let anf_44 = anf_45 1000 in
    let anf_43 = anf_44 10000 in
    let anf_42 = anf_43 100000 in
    let anf_41 = anf_42 1000000 in
    let anf_40 = anf_41 10000000 in
    let anf_39 = anf_40 100000000 in
    let anf_32 = anf_39 1000000000 in
    let anf_33 = print_int anf_31 in
    let anf_38 = anf_var_0 anf_var_4 in
    let anf_37 = anf_38 1 in
    let anf_36 = anf_37 10 in
    let anf_35 = anf_36 100 in
    let anf_34 = anf_35 in
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
    let rec anf_var_0 arg_1 arg_2 = let anf_3 = anf_var_0 arg_1 in
    let anf_2 = arg_1 anf_3 in
    let anf_1 = anf_2 arg_2 in
    anf_1

    let anf_var_4 arg_3 arg_4 = let anf_12 = ( <= ) arg_4 in
    let anf_6 = anf_12 1 in
    let anf_8 = ( * ) arg_4 in
    let anf_11 = ( - ) arg_4 in
    let anf_10 = anf_11 1 in
    let anf_9 = arg_3 anf_10 in
    let anf_7 = anf_8 anf_9 in
    let anf_5 = if anf_6 then 1 else anf_7 in
    anf_5

    let anf_var_13  = let anf_16 = anf_var_0 anf_var_4 in
    let anf_15 = anf_16 6 in
    let anf_14 = print_int anf_15 in
    let () = anf_14 in
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
    let anf_var_0 arg_1 = let anf_2 = ( + ) arg_1 in
    let anf_1 = anf_2 2 in
    anf_1

    let anf_var_3 arg_2 = let anf_5 = ( * ) arg_2 in
    let anf_4 = anf_5 10 in
    anf_4

    let anf_var_6 arg_0 = let anf_7 = if arg_0 then anf_var_0 else anf_var_3 in
    anf_7

    let anf_var_8 arg_4 = let anf_10 = anf_var_6 true in
    let anf_12 = anf_var_6 false in
    let anf_14 = anf_var_6 true in
    let anf_16 = anf_var_6 false in
    let anf_15 = anf_16 arg_4 in
    let anf_13 = anf_14 anf_15 in
    let anf_11 = anf_12 anf_13 in
    let anf_9 = anf_10 anf_11 in
    anf_9

    let anf_var_17  = let anf_19 = anf_var_8 11 in
    let anf_18 = print_int anf_19 in
    let () = anf_18 in
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
    let anf_var_0 arg_0 arg_1 arg_2 = let anf_1 = print_int arg_0 in
    let anf_2 = print_int arg_1 in
    let anf_3 = print_int arg_2 in
    let anf_5 = ( + ) arg_0 in
    let anf_7 = ( * ) arg_1 in
    let anf_6 = anf_7 arg_2 in
    let anf_4 = anf_5 anf_6 in
    let () = anf_3 in
    anf_4

    let anf_var_8  = let anf_10 = anf_var_0 1 in
    let anf_12 = anf_9 2 in
    let anf_14 = anf_11 3 in
    let anf_15 = print_int anf_13 in
    let () = anf_15 in
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
    let anf_var_0 arg_2 = let anf_1 = print_int arg_2 in
    anf_1

    let anf_var_2 arg_1 = let anf_3 = print_int arg_1 in
    let () = anf_3 in
    anf_var_0

    let anf_var_4 arg_0 = let anf_5 = print_int arg_0 in
    let () = anf_5 in
    anf_var_2

    let anf_var_6  = let anf_9 = anf_var_4 4 in
    let anf_8 = anf_9 8 in
    let anf_7 = anf_8 9 in
    let () = anf_7 in
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
    let anf_var_0 ll_arg_1 ll_arg_2 arg_0 ll_arg_3 arg_1 arg_2 ll_arg_4 arg_3 arg_4 = let anf_10 = ( + ) arg_0 in
    let anf_9 = anf_10 arg_1 in
    let anf_1 = print_int anf_9 in
    let anf_2 = print_int arg_4 in
    let anf_8 = ( * ) arg_0 in
    let anf_7 = anf_8 arg_1 in
    let anf_6 = ( / ) anf_7 in
    let anf_5 = anf_6 arg_2 in
    let anf_4 = ( + ) anf_5 in
    let anf_3 = anf_4 arg_3 in
    let () = anf_2 in
    anf_3

    let anf_var_11  = let anf_27 = print_int 1 in
    let anf_25 = anf_var_0 anf_27 in
    let anf_26 = print_int 2 in
    let anf_24 = anf_25 anf_26 in
    let anf_22 = anf_24 3 in
    let anf_23 = print_int 4 in
    let anf_21 = anf_22 anf_23 in
    let anf_20 = anf_21 100 in
    let anf_17 = anf_20 1000 in
    let anf_19 = - 1 in
    let anf_18 = print_int anf_19 in
    let anf_16 = anf_17 anf_18 in
    let anf_14 = anf_16 10000 in
    let anf_15 = - 555555 in
    let anf_13 = anf_14 anf_15 in
    let anf_12 = print_int anf_13 in
    anf_12    
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
    let anf_var_0 arg_0 arg_1 arg_2 = let anf_2 = arg_0 arg_2 in
    let anf_3 = arg_1 arg_2 in
    let anf_1 = anf_2 anf_3 in
    anf_1

    let anf_var_4 arg_4 arg_5 = let anf_9 = ( + ) arg_4 in
    let anf_6 = anf_9 1 in
    let anf_8 = ( * ) arg_4 in
    let anf_7 = anf_8 2 in
    let anf_5 = if arg_5 then anf_6 else anf_7 in
    anf_5

    let anf_var_10 arg_6 = let anf_14 = ( / ) arg_6 in
    let anf_13 = anf_14 2 in
    let anf_12 = ( = ) anf_13 in
    let anf_11 = anf_12 0 in
    anf_11

    let anf_var_15  = let anf_19 = anf_var_0 anf_var_4 in
    let anf_18 = anf_19 anf_var_10 in
    let anf_17 = anf_18 4 in
    let anf_16 = print_int anf_17 in
    let () = anf_16 in
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
    let anf_var_0 arg_0 = arg_0

    let anf_var_1  = let anf_2 = anf_var_0 1 in
    let anf_3 = anf_var_0 true in
    (anf_2, anf_3)     
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
    let rec anf_var_0 arg_1 = let anf_11 = ( = ) 0 in
    let anf_12 = list_length arg_1 in
    let anf_10 = anf_11 anf_12 in
    let anf_2 = if anf_10 then true else false in
    let anf_4 = list_tail arg_1 in
    let anf_6 = list_head arg_1 in
    let anf_8 = ( + ) 1 in
    let anf_9 = anf_var_0 anf_3 in
    let anf_7 = anf_8 anf_9 in
    let anf_5 = anf_6 in
    anf_7

    let rec anf_var_13 arg_5 arg_6 = let anf_25 = ( = ) 0 in
    let anf_26 = list_length arg_6 in
    let anf_24 = anf_25 anf_26 in
    let anf_15 = if anf_24 then true else false in
    let anf_17 = list_tail arg_6 in
    let anf_19 = list_head arg_6 in
    let anf_23 = ( + ) arg_5 in
    let anf_22 = anf_23 1 in
    let anf_21 = anf_var_13 anf_22 in
    let anf_20 = anf_21 anf_16 in
    let anf_18 = anf_19 in
    anf_20

    let anf_var_27  = let anf_28 = anf_var_13 0 in
    anf_28

    let rec anf_var_29 arg_11 arg_12 = let anf_55 = ( = ) 0 in
    let anf_56 = list_length arg_12 in
    let anf_54 = anf_55 anf_56 in
    let anf_31 = if anf_54 then true else false in
    let anf_32 = list_tail arg_12 in
    let anf_34 = list_head arg_12 in
    let anf_37 = list_head arg_12 in
    let anf_38 = list_tail arg_12 in
    let anf_53 = list_length arg_12 in
    let anf_52 = ( >= ) anf_53 in
    let anf_51 = anf_52 2 in
    let anf_47 = ( && ) anf_51 in
    let anf_49 = ( = ) anf_36 in
    let anf_50 = list_head arg_12 in
    let anf_48 = anf_49 anf_50 in
    let anf_46 = anf_47 anf_48 in
    let anf_40 = ( && ) anf_46 in
    let anf_43 = ( = ) 0 in
    let anf_45 = list_tail arg_12 in
    let anf_44 = list_length anf_45 in
    let anf_42 = anf_43 anf_44 in
    let anf_41 = if anf_42 then true else false in
    let anf_39 = anf_40 anf_41 in
    let [] = anf_38 in
    anf_39

    let rec anf_var_57 arg_25 arg_26 = let anf_68 = ( = ) 0 in
    let anf_69 = list_length arg_25 in
    let anf_67 = anf_68 anf_69 in
    let anf_59 = if anf_67 then true else false in
    let anf_61 = list_tail arg_25 in
    let anf_63 = list_head anf_60 in
    let anf_66 = anf_var_57 anf_60 in
    let anf_65 = anf_66 arg_26 in
    let anf_64 = anf_62 :: anf_65 in
    let anf_62 = anf_63 in
    anf_64

    let rec anf_var_70 arg_29 = let anf_81 = ( = ) 0 in
    let anf_82 = list_length arg_29 in
    let anf_80 = anf_81 anf_82 in
    let anf_72 = if anf_80 then true else false in
    let anf_74 = list_tail arg_29 in
    let anf_76 = list_head arg_29 in
    let anf_78 = anf_var_57 anf_75 in
    let anf_79 = anf_var_70 anf_73 in
    let anf_77 = anf_78 anf_79 in
    let anf_75 = anf_76 in
    anf_77

    let anf_var_83  = anf_var_70

    let rec anf_var_84 arg_34 arg_35 = let anf_95 = ( = ) 0 in
    let anf_96 = list_length arg_35 in
    let anf_94 = anf_95 anf_96 in
    let anf_86 = if anf_94 then true else false in
    let anf_88 = list_tail arg_35 in
    let anf_90 = list_head arg_35 in
    let anf_91 = arg_34 anf_89 in
    let anf_93 = anf_var_84 arg_34 in
    let anf_92 = anf_93 anf_87 in
    let () = anf_91 in
    anf_92

    let anf_var_97 arg_41 arg_43 = (arg_41, arg_43)

    let rec anf_var_98 arg_39 arg_40 = let anf_113 = ( = ) 0 in
    let anf_114 = list_length arg_39 in
    let anf_112 = anf_113 anf_114 in
    let anf_100 = if anf_112 then true else false in
    let anf_102 = list_tail arg_39 in
    let anf_104 = list_head arg_39 in
    let anf_111 = anf_var_97 anf_103 in
    let anf_110 = anf_var_29 anf_111 in
    let anf_109 = anf_110 arg_40 in
    let anf_106 = anf_var_57 anf_109 in
    let anf_108 = anf_var_98 anf_101 in
    let anf_107 = anf_108 arg_40 in
    let anf_105 = anf_106 anf_107 in
    let anf_103 = anf_104 in
    anf_105

    let anf_var_115  = let anf_121 = anf_var_84 print_int in
    let anf_116 = anf_121 [1; 2; 3] in
    let anf_120 = anf_var_98 [1; 2] in
    let anf_119 = anf_120 [1; 2; 3; 4] in
    let anf_118 = anf_var_0 anf_119 in
    let anf_117 = print_int anf_118 in
    let () = anf_117 in
    0    
  |}]
;;
