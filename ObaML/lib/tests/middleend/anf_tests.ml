(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let infer_and_result fmt structure =
  match Inferencer.run_structure_infer_with_custom_std structure Std.extended_std_lst with
  | Ok env ->
    Format.fprintf fmt "%a" Inferencer.TypeEnv.pretty_pp_env (Std.extended_std_lst, env)
  | Error err -> Format.fprintf fmt "Infer: %a" Typedtree.pp_error err
;;

let parse_and_middleend_result str =
  match Parser.structure_from_string str with
  | Ok structure ->
    let simple_structure = To_simple_ast.convert structure in
    let simple_structure = Alpha_conversion.run_alpha_conversion simple_structure Inner in
    let simple_structure = Closure_conversion.run_closure_conversion simple_structure in
    let simple_structure = Lambda_lifting.run_lambda_lifting simple_structure in
    let anf_res = To_anf.convert simple_structure in
    (match anf_res with
     | Ok anf ->
       let new_structure = Anf_to_simple_ast.convert anf in
       let new_structure = To_ast.convert new_structure in
       Format.printf
         "Types:\n%a\nConverted structure:\n%a\nTypes after conversions:\n%a"
         infer_and_result
         structure
         Anf_pretty_printer.print_program
         anf
         infer_and_result
         new_structure
     | Error e -> printf "Anf conversion error: %s" e)
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_middleend_result {| let a = fun x -> fun y -> y + x |};
  [%expect
    {|
    Types:
    val a : int -> int -> int

    Converted structure:
    let foba0 x y = (( + ) y x);;

    let a x = (foba0 x);;


    Types after conversions:
    val a : int -> int -> int
    val foba0 : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a = fun x -> fun y -> fun z -> z y x |};
  [%expect
    {|
    Types:
    val a : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c

    Converted structure:
    let foba1 x y z = (z y x);;

    let foba0 x y = (foba1 x y);;

    let a x = (foba0 x);;


    Types after conversions:
    val a : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
    val foba0 : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
    val foba1 : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a x = fun y -> x + y|};
  [%expect
    {|
    Types:
    val a : int -> int -> int

    Converted structure:
    let foba0 x y = (( + ) x y);;

    let a x = (foba0 x);;


    Types after conversions:
    val a : int -> int -> int
    val foba0 : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a x y z = x + y + z|};
  [%expect
    {|
    Types:
    val a : int -> int -> int -> int

    Converted structure:
    let a x y z =
    	let aoba0 = (( + ) x y) in (( + ) aoba0 z);;


    Types after conversions:
    val a : int -> int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a x y z = x y z|};
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
  parse_and_middleend_result {| let a = fun x -> let b = x in b;;|};
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
  parse_and_middleend_result
    {| let a x = let b y = (x * y * 52) :: 52 :: [] in match b 1 with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      Types:
      val a : int -> string

      Converted structure:
      let b x y =
      	let aoba0 = (( * ) x y) in
      	let aoba1 = (( * ) aoba0 52) in
      	let aoba2 = 52 :: [] in aoba1 :: aoba2;;

      let a x =
      	let #pat#0 = (b x 1) in
      	let aoba3 = (#list_length_getter# #pat#0) in
      	let aoba4 = (( = ) aoba3 2) in
      	let aoba5 = (#list_head_getter# #pat#0) in
      	let aoba6 = (( = ) aoba5 52) in
      	let aoba7 = (( && ) aoba4 aoba6) in
      	let aoba8 = (#list_tail_getter# #pat#0) in
      	let aoba9 = (#list_head_getter# aoba8) in
      	let aoba10 = (( = ) aoba9 52) in
      	let aoba11 = (( && ) aoba7 aoba10) in
      	if aoba11
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
  parse_and_middleend_result
    {| let a x = let b = (x * 52) :: 52 :: [] in match b with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      Types:
      val a : int -> string

      Converted structure:
      let a x =
      	let aoba0 = (( * ) x 52) in
      	let aoba1 = 52 :: [] in
      	let b = aoba0 :: aoba1 in
      	let #pat#0 = b in
      	let aoba2 = (#list_length_getter# #pat#0) in
      	let aoba3 = (( = ) aoba2 2) in
      	let aoba4 = (#list_head_getter# #pat#0) in
      	let aoba5 = (( = ) aoba4 52) in
      	let aoba6 = (( && ) aoba3 aoba5) in
      	let aoba7 = (#list_tail_getter# #pat#0) in
      	let aoba8 = (#list_head_getter# aoba7) in
      	let aoba9 = (( = ) aoba8 52) in
      	let aoba10 = (( && ) aoba6 aoba9) in
      	if aoba10
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (#matching_failed# ());;


      Types after conversions:
      val a : int -> string |}]
;;

let%expect_test "" =
  parse_and_middleend_result
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
      let foba0 f fix x =
      	let aoba0 = (fix f) in (f aoba0 x);;

      let rec fix f = (foba0 f fix);;

      let foba2 self n =
      	let aoba1 = (( <= ) n 1) in
      	if aoba1
      	then 1
      	else
      	let aoba2 = (( - ) n 1) in
      	let aoba3 = (self aoba2) in (( * ) n aoba3);;

      let foba1 self = (foba2 self);;

      let fac = (fix foba1);;

      let a = (fac 5);;


      Types after conversions:
      val a : int
      val fac : int -> int
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val foba0 : ('b -> 'a -> 'c) -> (('b -> 'a -> 'c) -> 'b) -> 'a -> 'c
      val foba1 : (int -> int) -> int -> int
      val foba2 : (int -> int) -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result
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
    let foba0 acc helper oba0 =
    	let #pat#0 = oba0 in
    	let aoba0 = (( = ) #pat#0 []) in
    	if aoba0
    	then acc
    	else
    	let aoba1 = (#list_length_getter# #pat#0) in
    	let aoba2 = (( >= ) aoba1 1) in
    	if aoba2
    	then
    	let h = (#list_head_getter# #pat#0) in
    	let tl = (#list_tail_getter# #pat#0) in
    	let aoba3 = h :: acc in (helper aoba3 tl)
    	else (#matching_failed# ());;

    let rec helper acc = (foba0 acc helper);;

    let rev lst = (helper [] lst);;

    let reversed1 =
    	let aoba4 = 5 :: [] in
    	let aoba5 = 4 :: aoba4 in
    	let aoba6 = 3 :: aoba5 in
    	let aoba7 = 2 :: aoba6 in
    	let aoba8 = 1 :: aoba7 in (rev aoba8);;

    let reversed2 =
    	let aoba9 = false :: [] in
    	let aoba10 = false :: aoba9 in
    	let aoba11 = false :: aoba10 in
    	let aoba12 = true :: aoba11 in (rev aoba12);;


    Types after conversions:
    val foba0 : 'a list -> ('a list -> 'a list -> 'a list) -> 'a list -> 'a list
    val helper : 'a list -> 'a list -> 'a list
    val rev : 'a list -> 'a list
    val reversed1 : int list
    val reversed2 : bool list |}]
;;

let%expect_test "" =
  parse_and_middleend_result
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
  parse_and_middleend_result
    {| let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;  |};
  [%expect
    {|
      Types:
      val y : 'a -> int

      Converted structure:
      let a x y z =
      	let aoba0 = (y x) in (( + ) aoba0 z);;

      let b a oba0 =
      	let aoba1 = (a 5) in (( + ) aoba1 oba0);;

      let rec y x =
      	let aoba2 = (a x y) in (b aoba2 5);;


      Types after conversions:
      val a : 'a -> ('a -> int) -> int -> int
      val b : (int -> int) -> int -> int
      val y : 'a -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let f z y = let z x = y + x in z 5 + y|};
  [%expect
    {|
      Types:
      val f : 'a -> int -> int

      Converted structure:
      let oba0 y x = (( + ) y x);;

      let f z y =
      	let aoba0 = (oba0 y 5) in (( + ) aoba0 y);;


      Types after conversions:
      val f : 'a -> int -> int
      val oba0 : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result
    {| let rec fix f x = f (fix f) x
  let helper f p = f p
  let zet l =
    fix (fun self l -> helper (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      Types:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val helper : ('a -> 'b) -> 'a -> 'b
      val zet : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

      Converted structure:
      let rec fix f x =
      	let aoba0 = (fix f) in (f aoba0 x);;

      let helper f p = (f p);;

      let foba1 oba0 self li x =
      	let aoba1 = (self oba0) in (li aoba1 x);;

      let foba0 self oba0 =
      	let aoba2 = (foba1 oba0 self) in (helper aoba2 oba0);;

      let zet l = (fix foba0 l);;


      Types after conversions:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val foba0 : (('a -> 'b -> 'c) -> 'a) -> ('a -> 'b -> 'c) -> 'b -> 'c
      val foba1 : 'a -> ('a -> 'c) -> ('c -> 'b -> 'd) -> 'b -> 'd
      val helper : ('a -> 'b) -> 'a -> 'b
      val zet : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a x = let z = (fun zu -> zu + x) in z;; |};
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
  parse_and_middleend_result
    {| 
  let rec a x = let z = (fun zet -> zet + x) in
      let rec a x y = a (x + z 1) y in 
         a;;
;; |};
  [%expect
    {|
    Types:
    val a : int -> int -> 'a -> 'b

    Converted structure:
    let z x zet = (( + ) zet x);;

    let rec oba0 z oba1 y =
    	let aoba0 = (z 1) in
    	let aoba1 = (( + ) oba1 aoba0) in (oba0 z aoba1 y);;

    let rec a x =
    	let aoba2 = (z x) in (oba0 aoba2);;


    Types after conversions:
    val a : int -> int -> 'a -> 'b
    val oba0 : (int -> int) -> int -> 'a -> 'b
    val z : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result
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

      let oba1 oba0 z =
      	let aoba0 = (oba0 1) in (( + ) aoba0 z);;

      let f x y =
      	let aoba1 = (oba0 y 1) in
      	let aoba2 = (oba0 y) in
      	let aoba3 = (oba1 aoba2 2) in (( + ) aoba1 aoba3);;


      Types after conversions:
      val f : 'a -> int -> int
      val oba0 : int -> int -> int
      val oba1 : (int -> int) -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result
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
      	let aoba0 = (fix f) in (f aoba0 x);;

      let map f p =
      	let #pat#0 = p in
      	let a = (#tuple_getter# 0 #pat#0) in
      	let b = (#tuple_getter# 1 #pat#0) in
      	let aoba1 = (f a) in
      	let aoba2 = (f b) in (aoba1, aoba2);;

      let foba1 oba0 self li x =
      	let aoba3 = (self oba0) in (li aoba3 x);;

      let foba0 self oba0 =
      	let aoba4 = (foba1 oba0 self) in (map aoba4 oba0);;

      let fixpoly l = (fix foba0 l);;


      Types after conversions:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
      val foba0 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
      val foba1 : 'a -> ('a -> 'c) -> ('c -> 'b -> 'd) -> 'b -> 'd
      val map : ('b -> 'c) -> 'a -> 'c * 'c |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a b c = if (b, c) = (5, 4) then 1 else 2;; |};
  [%expect
    {|
      Types:
      val a : int -> int -> int

      Converted structure:
      let a b c =
      	let aoba4 = (( = ) (b, c) (5, 4)) in
      	if aoba4
      	then 1
      	else 2;;


      Types after conversions:
      val a : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let x = let a = let b = 5 in b in a;; |};
  [%expect
    {|
      Types:
      val x : int

      Converted structure:
      let x =
      	let b = 5 in
      	let a = b in a;;


      Types after conversions:
      val x : int |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a b c = (b c, b c) = (9, 9);; |};
  [%expect
    {|
      Types:
      val a : ('a -> int) -> 'a -> bool

      Converted structure:
      let a b c =
      	let aoba0 = (b c) in
      	let aoba1 = (b c) in (( = ) (aoba0, aoba1) (9, 9));;


      Types after conversions:
      val a : ('a -> int) -> 'a -> bool |}]
;;

let%expect_test "" =
  parse_and_middleend_result {| let a = let x = 1 in
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
      	let aoba0 = (( + ) oba2 oba1) in (h aoba0);;

      let f oba0 =
      	let y = (( + ) oba0 1) in
      	let aoba1 = (( + ) y oba0) in (g aoba1);;

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
  parse_and_middleend_result {| let rec outer n =
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
      	let aoba0 = (( = ) y 0) in
      	if aoba0
      	then x
      	else
      	let oba0 = (( + ) x y) in
      	let aoba1 = (( - ) y 1) in (inner x aoba1);;

      let rec middle x = (inner x x);;

      let rec outer n =
      	let aoba2 = (( = ) n 0) in
      	if aoba2
      	then 0
      	else
      	let result = (middle n) in
      	let aoba3 = (( - ) n 1) in
      	let oba1 = (outer aoba3) in (( + ) result oba1);;

      let final_result = (outer 5);;

      let outer x = (( * ) x x);;

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
  parse_and_middleend_result {| let rec aoba0 n =
    let aoba1 = n * 2 in
    let rec aoba2 x =
      let aoba3 = x + aoba1 in
      let rec aoba4 y =
        if y = 0 then aoba3
        else
          let aoba5 = aoba3 + y in
          let rec aoba6 z =
            let aoba7 = z + aoba5 in
            if z = 1 then aoba7
            else
              let aoba8 = aoba7 * 2 in
              aoba6 (z - 1)
          in
          aoba6 y
      in
      aoba4 x
    in
    if n = 0 then 0
    else
      let aoba9 = aoba2 n in
      let aoba0 = aoba0 (n - 1) in  
      aoba9 + aoba0
  
  let aoba10 = aoba0 5 
  
  let aoba0 = fun x -> x - 7  
  
  let aoba11 = aoba0 aoba10  
  
  let () = print_int aoba11 |};
  [%expect
    {|
      Types:
      val aoba0 : int -> int
      val aoba10 : int
      val aoba11 : int

      Converted structure:
      let rec aoba6 aoba5 z =
      	let aoba7 = (( + ) z aoba5) in
      	let aoba0 = (( = ) z 1) in
      	if aoba0
      	then aoba7
      	else
      	let aoba8 = (( * ) aoba7 2) in
      	let aoba1 = (( - ) z 1) in (aoba6 aoba5 aoba1);;

      let rec aoba4 aoba3 y =
      	let aoba2 = (( = ) y 0) in
      	if aoba2
      	then aoba3
      	else
      	let aoba5 = (( + ) aoba3 y) in (aoba6 aoba5 y);;

      let rec aoba2 aoba1 x =
      	let aoba3 = (( + ) x aoba1) in (aoba4 aoba3 x);;

      let rec aoba0 n =
      	let aoba1 = (( * ) n 2) in
      	let aoba3 = (( = ) n 0) in
      	if aoba3
      	then 0
      	else
      	let aoba9 = (aoba2 aoba1 n) in
      	let aoba4 = (( - ) n 1) in
      	let oba0 = (aoba0 aoba4) in (( + ) aoba9 oba0);;

      let aoba10 = (aoba0 5);;

      let aoba0 x = (( - ) x 7);;

      let aoba11 = (aoba0 aoba10);;

      let () = (print_int aoba11);;


      Types after conversions:
      val aoba0 : int -> int
      val aoba10 : int
      val aoba11 : int
      val aoba2 : int -> int -> int
      val aoba4 : int -> int -> int
      val aoba6 : int -> int -> int |}]
;;