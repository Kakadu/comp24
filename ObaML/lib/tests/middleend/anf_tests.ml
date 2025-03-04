(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let parse_and_get_anf_result str =
  match Parser.structure_from_string str with
  | Ok structure ->
    (match Inferencer.run_structure_infer structure with
     | Ok structure_env ->
       Format.printf
         "Types:\n%a\n"
         Inferencer.TypeEnv.pretty_pp_env
         (Std.std_lst, structure_env);
       let simple_structure = To_simple_ast.convert structure in
       let simple_structure, varSet =
         Alpha_conversion.run_alpha_conversion simple_structure Inner
       in
       let simple_structure =
         Closure_conversion.run_closure_conversion simple_structure
       in
       let simple_structure, varSet =
         Lambda_lifting.run_lambda_lifting simple_structure varSet
       in
       let anf_res = To_anf.convert simple_structure varSet in
       (match anf_res with
        | Ok anf ->
          let new_structure = Anf_to_simple_ast.convert anf in
          let new_structure = To_ast.convert new_structure in
          Format.printf "Converted structure:\n%a\n" Anf_pretty_printer.print_program anf;
          (match
             Inferencer.run_structure_infer_with_custom_std
               new_structure
               Std.extended_std_lst
           with
           | Ok new_structure_env ->
             Format.printf
               "Types after conversions:\n%a"
               Inferencer.TypeEnv.pretty_pp_env
               (Std.extended_std_lst, new_structure_env)
           | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
        | Error e -> Format.printf "Anf conversion error: %s" e)
     | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
  | Error err -> Format.printf "Parser: %s\n" err
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a = fun x -> fun y -> y + x |};
  [%expect
    {|
    Types:
    val a : int -> int -> int

    Converted structure:
    let oba0 x y = (( + ) y x);;

    let a x = (oba0 x);;


    Types after conversions:
    val a : int -> int -> int
    val oba0 : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a = fun x -> fun y -> fun z -> z y x |};
  [%expect
    {|
    Types:
    val a : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c

    Converted structure:
    let oba1 x y z = (z y x);;

    let oba0 x y = (oba1 x y);;

    let a x = (oba0 x);;


    Types after conversions:
    val a : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
    val oba0 : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
    val oba1 : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a x = fun y -> x + y|};
  [%expect
    {|
    Types:
    val a : int -> int -> int

    Converted structure:
    let oba0 x y = (( + ) x y);;

    let a x = (oba0 x);;


    Types after conversions:
    val a : int -> int -> int
    val oba0 : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a x y z = x + y + z|};
  [%expect
    {|
    Types:
    val a : int -> int -> int -> int

    Converted structure:
    let a x y z =
    	let oba0 = (( + ) x y) in (( + ) oba0 z);;


    Types after conversions:
    val a : int -> int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a x y z = x y z|};
  [%expect
    {|
    Types:
    val a : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

    Converted structure:
    let a x y z = (x y z);;


    Types after conversions:
    val a : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a = fun x -> let b = x in b;;|};
  [%expect
    {|
    Types:
    val a : 'a -> 'a

    Converted structure:
    let a x =
    	let b = x in b;;


    Types after conversions:
    val a : 'a -> 'a |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let a x = let b y = (x * y * 52) :: 52 :: [] in match b 1 with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      Types:
      val a : int -> string

      Converted structure:
      let b x y =
      	let oba1 = (( * ) x y) in
      	let oba2 = (( * ) oba1 52) in
      	let oba3 = 52 :: [] in oba2 :: oba3;;

      let a x =
      	let oba0 = (b x 1) in
      	let oba4 = (#list_length_getter# oba0) in
      	let oba5 = (( = ) oba4 2) in
      	let oba6 = (#list_head_getter# oba0) in
      	let oba7 = (( = ) oba6 52) in
      	let oba8 = (( && ) oba5 oba7) in
      	let oba9 = (#list_tail_getter# oba0) in
      	let oba10 = (#list_head_getter# oba9) in
      	let oba11 = (( = ) oba10 52) in
      	let oba12 = (( && ) oba8 oba11) in
      	if oba12
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (#matching_failed# ());;


      Types after conversions:
      val a : int -> string
      val b : int -> int -> int list |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let a x = let b = (x * 52) :: 52 :: [] in match b with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      Types:
      val a : int -> string

      Converted structure:
      let a x =
      	let oba1 = (( * ) x 52) in
      	let oba2 = 52 :: [] in
      	let b = oba1 :: oba2 in
      	let oba0 = b in
      	let oba3 = (#list_length_getter# oba0) in
      	let oba4 = (( = ) oba3 2) in
      	let oba5 = (#list_head_getter# oba0) in
      	let oba6 = (( = ) oba5 52) in
      	let oba7 = (( && ) oba4 oba6) in
      	let oba8 = (#list_tail_getter# oba0) in
      	let oba9 = (#list_head_getter# oba8) in
      	let oba10 = (( = ) oba9 52) in
      	let oba11 = (( && ) oba7 oba10) in
      	if oba11
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (#matching_failed# ());;


      Types after conversions:
      val a : int -> string |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
    let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
    let a = fac 5 |};
  [%expect
    {|
      Types:
      val a : int
      val fac : int -> int
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

      Converted structure:
      let oba0 f fix x =
      	let oba3 = (fix f) in (f oba3 x);;

      let rec fix f = (oba0 f fix);;

      let oba2 self n =
      	let oba4 = (( <= ) n 1) in
      	if oba4
      	then 1
      	else
      	let oba5 = (( - ) n 1) in
      	let oba6 = (self oba5) in (( * ) n oba6);;

      let oba1 self = (oba2 self);;

      let fac = (fix oba1);;

      let a = (fac 5);;


      Types after conversions:
      val a : int
      val fac : int -> int
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val oba0 : ('b -> 'a -> 'c) -> (('b -> 'a -> 'c) -> 'b) -> 'a -> 'c
      val oba1 : (int -> int) -> int -> int
      val oba2 : (int -> int) -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let rev = fun lst ->
      let rec helper = fun acc -> (fun lst ->
      match lst with
        | [] -> acc
        | h :: tl -> helper (h :: acc) tl)
      in
      helper [] lst
    let reversed1 = rev (1 :: 2 :: 3 :: 4 :: 5 :: [])
    let reversed2 = rev (true :: false :: false :: false :: []) |};
  [%expect
    {|
    Types:
    val rev : 'a list -> 'a list
    val reversed1 : int list
    val reversed2 : bool list

    Converted structure:
    let oba2 acc helper oba0 =
    	let oba1 = oba0 in
    	let oba3 = (( = ) oba1 []) in
    	if oba3
    	then acc
    	else
    	let oba4 = (#list_length_getter# oba1) in
    	let oba5 = (( >= ) oba4 1) in
    	if oba5
    	then
    	let h = (#list_head_getter# oba1) in
    	let tl = (#list_tail_getter# oba1) in
    	let oba6 = h :: acc in (helper oba6 tl)
    	else (#matching_failed# ());;

    let rec helper acc = (oba2 acc helper);;

    let rev lst = (helper [] lst);;

    let reversed1 =
    	let oba7 = 5 :: [] in
    	let oba8 = 4 :: oba7 in
    	let oba9 = 3 :: oba8 in
    	let oba10 = 2 :: oba9 in
    	let oba11 = 1 :: oba10 in (rev oba11);;

    let reversed2 =
    	let oba12 = false :: [] in
    	let oba13 = false :: oba12 in
    	let oba14 = false :: oba13 in
    	let oba15 = true :: oba14 in (rev oba15);;


    Types after conversions:
    val helper : 'a list -> 'a list -> 'a list
    val oba2 : 'a list -> ('a list -> 'a list -> 'a list) -> 'a list -> 'a list
    val rev : 'a list -> 'a list
    val reversed1 : int list
    val reversed2 : bool list |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let rec y = fun x -> let a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      Types:
      val y : 'a -> int

      Converted structure:
      let b a oba0 = (( + ) a oba0);;

      let rec y x =
      	let a = (y x) in (b a 5);;


      Types after conversions:
      val b : int -> int -> int
      val y : 'a -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;  |};
  [%expect
    {|
      Types:
      val y : 'a -> int

      Converted structure:
      let a x y z =
      	let oba1 = (y x) in (( + ) oba1 z);;

      let b a oba0 =
      	let oba2 = (a 5) in (( + ) oba2 oba0);;

      let rec y x =
      	let oba3 = (a x y) in (b oba3 5);;


      Types after conversions:
      val a : 'a -> ('a -> int) -> int -> int
      val b : (int -> int) -> int -> int
      val y : 'a -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let f z y = let z x = y + x in z 5 + y|};
  [%expect
    {|
      Types:
      val f : 'a -> int -> int

      Converted structure:
      let oba0 y x = (( + ) y x);;

      let f z y =
      	let oba1 = (oba0 y 5) in (( + ) oba1 y);;


      Types after conversions:
      val f : 'a -> int -> int
      val oba0 : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let rec fix f x = f (fix f) x
  let helper f p = f p
  let zu l =
    fix (fun self l -> helper (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      Types:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val helper : ('a -> 'b) -> 'a -> 'b
      val zu : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

      Converted structure:
      let rec fix f x =
      	let oba5 = (fix f) in (f oba5 x);;

      let helper oba0 p = (oba0 p);;

      let oba4 oba1 self li oba2 =
      	let oba6 = (self oba1) in (li oba6 oba2);;

      let oba3 self oba1 =
      	let oba7 = (oba4 oba1 self) in (helper oba7 oba1);;

      let zu l = (fix oba3 l);;


      Types after conversions:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val helper : ('a -> 'b) -> 'a -> 'b
      val oba3 : (('a -> 'b -> 'c) -> 'a) -> ('a -> 'b -> 'c) -> 'b -> 'c
      val oba4 : 'a -> ('a -> 'c) -> ('c -> 'b -> 'd) -> 'b -> 'd
      val zu : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a x = let z = (fun zu -> zu + x) in z;; |};
  [%expect
    {|
      Types:
      val a : int -> int -> int

      Converted structure:
      let z x zu = (( + ) zu x);;

      let a x = (z x);;


      Types after conversions:
      val a : int -> int -> int
      val z : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| 
  let rec a x = let z = (fun zu -> zu + x) in
      let rec a x y = a (x + z 1) y in 
         a;;
;; |};
  [%expect
    {|
    Types:
    val a : int -> int -> 'a -> 'b

    Converted structure:
    let z x zu = (( + ) zu x);;

    let rec oba0 z oba1 y =
    	let oba2 = (z 1) in
    	let oba3 = (( + ) oba1 oba2) in (oba0 z oba3 y);;

    let rec a x =
    	let oba4 = (z x) in (oba0 oba4);;


    Types after conversions:
    val a : int -> int -> 'a -> 'b
    val oba0 : (int -> int) -> int -> 'a -> 'b
    val z : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      Types:
      val f : 'a -> int -> int

      Converted structure:
      let oba0 y z = (( + ) y z);;

      let oba1 oba0 oba2 =
      	let oba3 = (oba0 1) in (( + ) oba3 oba2);;

      let f x y =
      	let oba4 = (oba0 y 1) in
      	let oba5 = (oba0 y) in
      	let oba6 = (oba1 oba5 2) in (( + ) oba4 oba6);;


      Types after conversions:
      val f : 'a -> int -> int
      val oba0 : int -> int -> int
      val oba1 : (int -> int) -> int -> int |}]
;;

let%expect_test "tuple types cannot be fully infered because the exact type of \
                 `tuple_getter` is not clear"
  =
  parse_and_get_anf_result
    {| let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      Types:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
      val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b

      Converted structure:
      let rec fix f x =
      	let oba6 = (fix f) in (f oba6 x);;

      let map oba0 p =
      	let oba1 = p in
      	let a = (#tuple_getter# 0 oba1) in
      	let b = (#tuple_getter# 1 oba1) in
      	let oba7 = (oba0 a) in
      	let oba8 = (oba0 b) in (oba7, oba8);;

      let oba5 oba2 self li oba3 =
      	let oba9 = (self oba2) in (li oba9 oba3);;

      let oba4 self oba2 =
      	let oba10 = (oba5 oba2 self) in (map oba10 oba2);;

      let fixpoly l = (fix oba4 l);;


      Types after conversions:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
      val map : ('b -> 'c) -> 'a -> 'c * 'c
      val oba4 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
      val oba5 : 'a -> ('a -> 'c) -> ('c -> 'b -> 'd) -> 'b -> 'd |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a b c = if (b, c) = (5, 4) then 1 else 2;; |};
  [%expect
    {|
      Types:
      val a : int -> int -> int

      Converted structure:
      let a b c =
      	let oba0 = (( = ) (b, c) (5, 4)) in
      	if oba0
      	then 1
      	else 2;;


      Types after conversions:
      val a : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result {| let a b c d e = (b c, d e) = (8, 9);; |};
  [%expect
    {|
      Types:
      val a : ('a -> int) -> 'a -> ('b -> int) -> 'b -> bool

      Converted structure:
      let a b c d e =
      	let oba0 = (b c) in
      	let oba1 = (d e) in (( = ) (oba0, oba1) (8, 9));;


      Types after conversions:
      val a : ('a -> int) -> 'a -> ('b -> int) -> 'b -> bool |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let a = let x = 1 in
  let f x = 
    let y = x + 1 in
    let g y =
      let x = y * 2 in
      let h x =
        let y = x - 3 in
        y + x
      in
      h (x + y)
    in
    g (y + x)
  in
  let x = f 5 in
  let f x = x * 2 in
  f x;; |};
  [%expect
    {|
      Types:
      val a : int

      Converted structure:
      let h oba3 =
      	let oba4 = (( - ) oba3 3) in (( + ) oba4 oba3);;

      let g oba1 =
      	let oba2 = (( * ) oba1 2) in
      	let oba8 = (( + ) oba2 oba1) in (h oba8);;

      let f oba0 =
      	let y = (( + ) oba0 1) in
      	let oba9 = (( + ) y oba0) in (g oba9);;

      let oba6 oba7 = (( * ) oba7 2);;

      let a =
      	let x = 1 in
      	let oba5 = (f 5) in (oba6 oba5);;


      Types after conversions:
      val a : int
      val f : int -> int
      val g : int -> int
      val h : int -> int
      val oba6 : int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {| let rec outer n =
    let rec middle x =
      let rec inner y =
        if y = 0 then x
        else
          let x = x + y in
          inner (y - 1)
      in
      inner x
    in
    if n = 0 then 0
    else
      let result = middle n in
      let outer = outer (n - 1) in 
      result + outer
  
  let final_result = outer 5  
  
  let outer = fun x -> x * x  
  
  let final_transformed = outer final_result  
  
  let () = print_int final_transformed |};
  [%expect
    {|
      Types:
      val final_result : int
      val final_transformed : int
      val outer : int -> int

      Converted structure:
      let rec inner x y =
      	let oba3 = (( = ) y 0) in
      	if oba3
      	then x
      	else
      	let oba0 = (( + ) x y) in
      	let oba4 = (( - ) y 1) in (inner x oba4);;

      let rec middle x = (inner x x);;

      let rec outer n =
      	let oba5 = (( = ) n 0) in
      	if oba5
      	then 0
      	else
      	let result = (middle n) in
      	let oba6 = (( - ) n 1) in
      	let oba1 = (outer oba6) in (( + ) result oba1);;

      let final_result = (outer 5);;

      let outer oba2 = (( * ) oba2 oba2);;

      let final_transformed = (outer final_result);;

      let () = (print_int final_transformed);;


      Types after conversions:
      val final_result : int
      val final_transformed : int
      val inner : int -> int -> int
      val middle : int -> int
      val outer : int -> int |}]
;;

let%expect_test "" =
  parse_and_get_anf_result
    {|let rec oba0 n =
    let oba1 = n * 2 in
    let rec oba2 x =
      let oba3 = x + oba1 in
      let rec oba4 y =
        if y = 0 then oba3
        else
          let oba5 = oba3 + y in
          let rec oba6 z =
            let oba7 = z + oba5 in
            if z = 1 then oba7
            else
              let oba8 = oba7 * 2 in
              oba6 (z - 1)
          in
          oba6 y
      in
      oba4 x
    in
    if n = 0 then 0
    else
      let oba9 = oba2 n in
      let oba0 = oba0 (n - 1) in  
      oba9 + oba0
  
  let oba10 = oba0 5 
  
  let oba0 = fun x -> x - 7  
  
  let oba11 = oba0 oba10  
  
  let () = print_int oba11 |};
  [%expect
    {|
      Types:
      val oba0 : int -> int
      val oba10 : int
      val oba11 : int

      Converted structure:
      let rec oba6 oba5 z =
      	let oba7 = (( + ) z oba5) in
      	let oba12 = (( = ) z 1) in
      	if oba12
      	then oba7
      	else
      	let oba8 = (( * ) oba7 2) in
      	let oba13 = (( - ) z 1) in (oba6 oba5 oba13);;

      let rec oba4 oba3 y =
      	let oba14 = (( = ) y 0) in
      	if oba14
      	then oba3
      	else
      	let oba5 = (( + ) oba3 y) in (oba6 oba5 y);;

      let rec oba2 oba1 x =
      	let oba3 = (( + ) x oba1) in (oba4 oba3 x);;

      let rec oba0 n =
      	let oba1 = (( * ) n 2) in
      	let oba15 = (( = ) n 0) in
      	if oba15
      	then 0
      	else
      	let oba9 = (oba2 oba1 n) in
      	let oba16 = (( - ) n 1) in
      	let oba10 = (oba0 oba16) in (( + ) oba9 oba10);;

      let oba10 = (oba0 5);;

      let oba0 oba11 = (( - ) oba11 7);;

      let oba11 = (oba0 oba10);;

      let () = (print_int oba11);;


      Types after conversions:
      val oba0 : int -> int
      val oba10 : int
      val oba11 : int
      val oba2 : int -> int -> int
      val oba4 : int -> int -> int
      val oba6 : int -> int -> int |}]
;;
