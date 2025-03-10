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
      let anf_3 = print_int arg_3 in
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
      let anf_15 = print_int ll_var_7 in
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
      let anf_1 = print_int arg_1 in
      let anf_2 = print_int arg_2 in
      let anf_3 = ( * ) arg_1 arg_2 in
      let anf_4 = ( + ) arg_0 anf_3 in
      let () = anf_2 in
      anf_4

      let ll_var_1  = let anf_5 = ll_var_0 1 in
      let anf_6 = ll_var_2 2 in
      let anf_7 = ll_var_3 3 in
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
      let anf_2 = print_int arg_4 in
      let anf_3 = ( * ) arg_0 arg_1 in
      let anf_4 = ( / ) anf_3 arg_2 in
      let anf_5 = ( + ) anf_4 arg_3 in
      let () = anf_2 in
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
      let rec ll_var_0 arg_1 = let anf_0 = list_length arg_1 in
      let anf_1 = ( = ) 0 anf_0 in
      let anf_12 = if anf_1 then let anf_6 = if true then 0 else let anf_2 = list_tail arg_1 in
      let anf_3 = list_head arg_1 in
      let anf_4 = ll_var_0 arg_3 in
      let anf_5 = ( + ) 1 anf_4 in
      let arg_2 = anf_3 in
      anf_5 in
      anf_6 else let anf_11 = if false then 0 else let anf_7 = list_tail arg_1 in
      let anf_8 = list_head arg_1 in
      let anf_9 = ll_var_0 arg_3 in
      let anf_10 = ( + ) 1 anf_9 in
      let arg_2 = anf_8 in
      anf_10 in
      anf_11 in
      let anf_17 = if anf_12 then 0 else let anf_13 = list_tail arg_1 in
      let anf_14 = list_head arg_1 in
      let anf_15 = ll_var_0 arg_3 in
      let anf_16 = ( + ) 1 anf_15 in
      let arg_2 = anf_14 in
      anf_16 in
      anf_17

      let rec ll_var_2 arg_5 arg_6 = let anf_18 = list_length arg_6 in
      let anf_19 = ( = ) 0 anf_18 in
      let anf_30 = if anf_19 then let anf_24 = if true then arg_5 else let anf_20 = list_tail arg_6 in
      let anf_21 = list_head arg_6 in
      let anf_22 = ( + ) arg_5 1 in
      let anf_23 = ll_var_2 anf_22 arg_8 in
      let arg_7 = anf_21 in
      anf_23 in
      anf_24 else let anf_29 = if false then arg_5 else let anf_25 = list_tail arg_6 in
      let anf_26 = list_head arg_6 in
      let anf_27 = ( + ) arg_5 1 in
      let anf_28 = ll_var_2 anf_27 arg_8 in
      let arg_7 = anf_26 in
      anf_28 in
      anf_29 in
      let anf_35 = if anf_30 then arg_5 else let anf_31 = list_tail arg_6 in
      let anf_32 = list_head arg_6 in
      let anf_33 = ( + ) arg_5 1 in
      let anf_34 = ll_var_2 anf_33 arg_8 in
      let arg_7 = anf_32 in
      anf_34 in
      anf_35

      let ll_var_1  = let anf_36 = ll_var_2 0 in
      anf_36

      let rec ll_var_3 arg_11 arg_12 = let anf_37 = list_length arg_12 in
      let anf_38 = ( = ) 0 anf_37 in
      let anf_95 = if anf_38 then let anf_66 = if true then let anf_39 = () in
      anf_39 else let anf_40 = list_tail arg_12 in
      let anf_41 = list_head arg_12 in
      let anf_42 = list_head arg_12 in
      let anf_43 = list_tail arg_12 in
      let anf_44 = list_tail arg_12 in
      let anf_45 = list_length anf_44 in
      let anf_46 = ( = ) 0 anf_45 in
      let anf_59 = if anf_46 then let anf_47 = list_head arg_12 in
      let anf_48 = ( = ) arg_13 anf_47 in
      let anf_49 = list_length arg_12 in
      let anf_50 = ( >= ) anf_49 2 in
      let anf_51 = ( && ) anf_50 anf_48 in
      let anf_52 = ( && ) anf_51 true in
      let [] = anf_43 in
      anf_52 else let anf_53 = list_head arg_12 in
      let anf_54 = ( = ) arg_13 anf_53 in
      let anf_55 = list_length arg_12 in
      let anf_56 = ( >= ) anf_55 2 in
      let anf_57 = ( && ) anf_56 anf_54 in
      let anf_58 = ( && ) anf_57 false in
      let [] = anf_43 in
      anf_58 in
      let anf_60 = list_head arg_12 in
      let anf_61 = ( = ) arg_13 anf_60 in
      let anf_62 = list_length arg_12 in
      let anf_63 = ( >= ) anf_62 2 in
      let anf_64 = ( && ) anf_63 anf_61 in
      let anf_65 = ( && ) anf_64 anf_59 in
      let [] = anf_43 in
      anf_65 in
      anf_66 else let anf_94 = if false then let anf_67 = () in
      anf_67 else let anf_68 = list_tail arg_12 in
      let anf_69 = list_head arg_12 in
      let anf_70 = list_head arg_12 in
      let anf_71 = list_tail arg_12 in
      let anf_72 = list_tail arg_12 in
      let anf_73 = list_length anf_72 in
      let anf_74 = ( = ) 0 anf_73 in
      let anf_87 = if anf_74 then let anf_75 = list_head arg_12 in
      let anf_76 = ( = ) arg_13 anf_75 in
      let anf_77 = list_length arg_12 in
      let anf_78 = ( >= ) anf_77 2 in
      let anf_79 = ( && ) anf_78 anf_76 in
      let anf_80 = ( && ) anf_79 true in
      let [] = anf_71 in
      anf_80 else let anf_81 = list_head arg_12 in
      let anf_82 = ( = ) arg_13 anf_81 in
      let anf_83 = list_length arg_12 in
      let anf_84 = ( >= ) anf_83 2 in
      let anf_85 = ( && ) anf_84 anf_82 in
      let anf_86 = ( && ) anf_85 false in
      let [] = anf_71 in
      anf_86 in
      let anf_88 = list_head arg_12 in
      let anf_89 = ( = ) arg_13 anf_88 in
      let anf_90 = list_length arg_12 in
      let anf_91 = ( >= ) anf_90 2 in
      let anf_92 = ( && ) anf_91 anf_89 in
      let anf_93 = ( && ) anf_92 anf_87 in
      let [] = anf_71 in
      anf_93 in
      anf_94 in
      let anf_123 = if anf_95 then let anf_96 = () in
      anf_96 else let anf_97 = list_tail arg_12 in
      let anf_98 = list_head arg_12 in
      let anf_99 = list_head arg_12 in
      let anf_100 = list_tail arg_12 in
      let anf_101 = list_tail arg_12 in
      let anf_102 = list_length anf_101 in
      let anf_103 = ( = ) 0 anf_102 in
      let anf_116 = if anf_103 then let anf_104 = list_head arg_12 in
      let anf_105 = ( = ) arg_13 anf_104 in
      let anf_106 = list_length arg_12 in
      let anf_107 = ( >= ) anf_106 2 in
      let anf_108 = ( && ) anf_107 anf_105 in
      let anf_109 = ( && ) anf_108 true in
      let [] = anf_100 in
      anf_109 else let anf_110 = list_head arg_12 in
      let anf_111 = ( = ) arg_13 anf_110 in
      let anf_112 = list_length arg_12 in
      let anf_113 = ( >= ) anf_112 2 in
      let anf_114 = ( && ) anf_113 anf_111 in
      let anf_115 = ( && ) anf_114 false in
      let [] = anf_100 in
      anf_115 in
      let anf_117 = list_head arg_12 in
      let anf_118 = ( = ) arg_13 anf_117 in
      let anf_119 = list_length arg_12 in
      let anf_120 = ( >= ) anf_119 2 in
      let anf_121 = ( && ) anf_120 anf_118 in
      let anf_122 = ( && ) anf_121 anf_116 in
      let [] = anf_100 in
      anf_122 in
      anf_123

      let rec ll_var_4 arg_25 arg_26 = let anf_124 = list_length arg_25 in
      let anf_125 = ( = ) 0 anf_124 in
      let anf_136 = if anf_125 then let anf_130 = if true then arg_26 else let anf_126 = list_tail arg_25 in
      let anf_127 = list_head arg_25 in
      let anf_129 = ll_var_4 arg_25 arg_26 in
      let anf_128 = arg_27 :: anf_129 in
      let arg_27 = anf_127 in
      anf_128 in
      anf_130 else let anf_135 = if false then arg_26 else let anf_131 = list_tail arg_25 in
      let anf_132 = list_head arg_25 in
      let anf_134 = ll_var_4 arg_25 arg_26 in
      let anf_133 = arg_27 :: anf_134 in
      let arg_27 = anf_132 in
      anf_133 in
      anf_135 in
      let anf_141 = if anf_136 then arg_26 else let anf_137 = list_tail arg_25 in
      let anf_138 = list_head arg_25 in
      let anf_140 = ll_var_4 arg_25 arg_26 in
      let anf_139 = arg_27 :: anf_140 in
      let arg_27 = anf_138 in
      anf_139 in
      anf_141

      let rec ll_var_6 arg_29 = let anf_142 = list_length arg_29 in
      let anf_143 = ( = ) 0 anf_142 in
      let anf_156 = if anf_143 then let anf_149 = if true then let anf_144 = () in
      anf_144 else let anf_145 = list_tail arg_29 in
      let anf_146 = list_head arg_29 in
      let anf_147 = ll_var_6 arg_31 in
      let anf_148 = ll_var_4 arg_30 anf_147 in
      let arg_30 = anf_146 in
      anf_148 in
      anf_149 else let anf_155 = if false then let anf_150 = () in
      anf_150 else let anf_151 = list_tail arg_29 in
      let anf_152 = list_head arg_29 in
      let anf_153 = ll_var_6 arg_31 in
      let anf_154 = ll_var_4 arg_30 anf_153 in
      let arg_30 = anf_152 in
      anf_154 in
      anf_155 in
      let anf_162 = if anf_156 then let anf_157 = () in
      anf_157 else let anf_158 = list_tail arg_29 in
      let anf_159 = list_head arg_29 in
      let anf_160 = ll_var_6 arg_31 in
      let anf_161 = ll_var_4 arg_30 anf_160 in
      let arg_30 = anf_159 in
      anf_161 in
      anf_162

      let ll_var_5  = ll_var_6

      let rec ll_var_7 arg_34 arg_35 = let anf_163 = list_length arg_35 in
      let anf_164 = ( = ) 0 anf_163 in
      let anf_175 = if anf_164 then let anf_169 = if true then () else let anf_165 = list_tail arg_35 in
      let anf_166 = list_head arg_35 in
      let anf_167 = arg_34 arg_36 in
      let anf_168 = ll_var_7 arg_34 arg_37 in
      let () = anf_167 in
      anf_168 in
      anf_169 else let anf_174 = if false then () else let anf_170 = list_tail arg_35 in
      let anf_171 = list_head arg_35 in
      let anf_172 = arg_34 arg_36 in
      let anf_173 = ll_var_7 arg_34 arg_37 in
      let () = anf_172 in
      anf_173 in
      anf_174 in
      let anf_180 = if anf_175 then () else let anf_176 = list_tail arg_35 in
      let anf_177 = list_head arg_35 in
      let anf_178 = arg_34 arg_36 in
      let anf_179 = ll_var_7 arg_34 arg_37 in
      let () = anf_178 in
      anf_179 in
      anf_180

      let ll_lam_9 arg_41 arg_43 = let anf_181 = (arg_41, arg_43) in
      anf_181

      let rec ll_var_8 arg_39 arg_40 = let anf_182 = list_length arg_39 in
      let anf_183 = ( = ) 0 anf_182 in
      let anf_200 = if anf_183 then let anf_191 = if true then let anf_184 = () in
      anf_184 else let anf_185 = list_tail arg_39 in
      let anf_186 = list_head arg_39 in
      let anf_187 = ll_var_8 arg_42 arg_40 in
      let anf_188 = ll_lam_9 arg_41 in
      let anf_189 = ll_var_3 anf_188 arg_40 in
      let anf_190 = ll_var_4 anf_189 anf_187 in
      let arg_41 = anf_186 in
      anf_190 in
      anf_191 else let anf_199 = if false then let anf_192 = () in
      anf_192 else let anf_193 = list_tail arg_39 in
      let anf_194 = list_head arg_39 in
      let anf_195 = ll_var_8 arg_42 arg_40 in
      let anf_196 = ll_lam_9 arg_41 in
      let anf_197 = ll_var_3 anf_196 arg_40 in
      let anf_198 = ll_var_4 anf_197 anf_195 in
      let arg_41 = anf_194 in
      anf_198 in
      anf_199 in
      let anf_208 = if anf_200 then let anf_201 = () in
      anf_201 else let anf_202 = list_tail arg_39 in
      let anf_203 = list_head arg_39 in
      let anf_204 = ll_var_8 arg_42 arg_40 in
      let anf_205 = ll_lam_9 arg_41 in
      let anf_206 = ll_var_3 anf_205 arg_40 in
      let anf_207 = ll_var_4 anf_206 anf_204 in
      let arg_41 = anf_203 in
      anf_207 in
      anf_208

      let ll_var_10  = let anf_209 = (1, 2, 3) in
      let anf_210 = ll_var_7 print_int anf_209 in
      let anf_211 = (1, 2, 3, 4) in
      let anf_212 = (1, 2) in
      let anf_213 = ll_var_8 anf_212 anf_211 in
      let anf_214 = ll_var_0 anf_213 in
      let anf_215 = print_int anf_214 in
      let () = anf_215 in
      0      
  |}]
;;
