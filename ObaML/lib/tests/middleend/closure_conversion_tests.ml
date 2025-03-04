(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let parse_and_closure_result str =
  match Parser.structure_from_string str with
  | Ok structure ->
    (match Inferencer.run_structure_infer structure with
     | Ok structure_env ->
       Format.printf
         "Types:\n%a\n"
         Inferencer.TypeEnv.pretty_pp_env
         (Std.std_lst, structure_env);
      let structure, _ =
         Alpha_conversion.run_alpha_conversion structure
       in
       let simple_structure = To_simple_ast.convert structure in
       let simple_structure =
         Closure_conversion.run_closure_conversion simple_structure
       in
       let new_structure = To_ast.convert simple_structure in
       Format.printf
         "Converted structure:\n%a\n"
         Simple_ast_pretty_printer.print_structure
         simple_structure;
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
     | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
  | Error err -> Format.printf "Parser: %s\n" err
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> fun y -> y + x |};
  [%expect
    {|
    Types:
    val a : int -> int -> int

    Converted structure:
    let a x = ((fun x y -> (y  +  x)) x);;


    Types after conversions:
    val a : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> fun y -> fun z -> z y x |};
  [%expect
    {|
    Types:
    val a : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c

    Converted structure:
    let a x = ((fun x y -> (((fun x y z -> ((z y) x)) x) y)) x);;


    Types after conversions:
    val a : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a x = fun y -> x + y|};
  [%expect
    {|
    Types:
    val a : int -> int -> int

    Converted structure:
    let a x = ((fun x y -> (x  +  y)) x);;


    Types after conversions:
    val a : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> let b = x in b;;|};
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
  parse_and_closure_result
    {| let a x = let b y = (x * y * 52) :: 52 :: [] in match b 1 with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      Types:
      val a : int -> string

      Converted structure:
      let a x =
      	let b x y = (((x  *  y)  *  52) :: (52 :: [])) in
      	let pat0 = ((b x) 1) in
      	if ((((list_length_getter pat0)  =  2)  &&  ((list_head_getter pat0)  =  52))  &&  ((list_head_getter (list_tail_getter pat0))  =  52))
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (matching_failed ());;


      Types after conversions:
      val a : int -> string |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let a x = let b = (x * 52) :: 52 :: [] in match b with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      Types:
      val a : int -> string

      Converted structure:
      let a x =
      	let b = ((x  *  52) :: (52 :: [])) in
      	let pat0 = b in
      	if ((((list_length_getter pat0)  =  2)  &&  ((list_head_getter pat0)  =  52))  &&  ((list_head_getter (list_tail_getter pat0))  =  52))
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (matching_failed ());;


      Types after conversions:
      val a : int -> string |}]
;;

let%expect_test "" =
  parse_and_closure_result
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
      let rec fix f = (((fun f fix x -> ((f (fix f)) x)) f) fix);;

      let fac = (fix (fun self -> ((fun self n ->
      	if (n  <=  1)
      	then 1
      	else (n  *  (self (n  -  1)))) self)));;

      let a = (fac 5);;


      Types after conversions:
      val a : int
      val fac : int -> int
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  parse_and_closure_result
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
    let rev lst =
    	let rec helper acc = (((fun acc helper oba0 ->
    	let pat0 = oba0 in
    	if (pat0  =  [])
    	then acc
    	else
    	if ((list_length_getter pat0)  >=  1)
    	then
    	let h = (list_head_getter pat0) in
    	let tl = (list_tail_getter pat0) in ((helper (h :: acc)) tl)
    	else (matching_failed ())) acc) helper) in ((helper []) lst);;

    let reversed1 = (rev (1 :: (2 :: (3 :: (4 :: (5 :: []))))));;

    let reversed2 = (rev (true :: (false :: (false :: (false :: [])))));;


    Types after conversions:
    val rev : 'a list -> 'a list
    val reversed1 : int list
    val reversed2 : bool list |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec y = fun x -> let a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      Types:
      val y : 'a -> int

      Converted structure:
      let rec y x =
      	let a = (y x) in
      	let b a oba0 = (a  +  oba0) in ((b a) 5);;


      Types after conversions:
      val y : 'a -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec y = fun x -> let rec a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      Types:
      val y : 'a -> int

      Converted structure:
      let rec y x =
      	let a = (y x) in
      	let b a oba0 = (a  +  oba0) in ((b a) 5);;


      Types after conversions:
      val y : 'a -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;  |};
  [%expect
    {|
      Types:
      val y : 'a -> int

      Converted structure:
      let rec y x =
      	let a x y z = ((y x)  +  z) in
      	let b a oba0 = ((a 5)  +  oba0) in ((b ((a x) y)) 5);;


      Types after conversions:
      val y : 'a -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let f z y = let z x = y + x in z 5 + y|};
  [%expect
    {|
      Types:
      val f : 'a -> int -> int

      Converted structure:
      let f z y =
      	let oba0 y x = (y  +  x) in (((oba0 y) 5)  +  y);;


      Types after conversions:
      val f : 'a -> int -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec fix f x = f (fix f) x
  let helper f p = f p
  let u l =
    fix (fun self l -> helper (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      Types:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val helper : ('a -> 'b) -> 'a -> 'b
      val u : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

      Converted structure:
      let rec fix f x = ((f (fix f)) x);;

      let helper oba0 p = (oba0 p);;

      let u l = ((fix (fun self oba1 -> ((helper (((fun oba1 self li oba2 -> ((li (self oba1)) oba2)) oba1) self)) oba1))) l);;


      Types after conversions:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val helper : ('a -> 'b) -> 'a -> 'b
      val u : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a x = let z = (fun zu -> zu + x) in z;; |};
  [%expect
    {|
      Types:
      val a : int -> int -> int

      Converted structure:
      let a x =
      	let z x zu = (zu  +  x) in (z x);;


      Types after conversions:
      val a : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result
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
    let rec a x =
    	let z x zu = (zu  +  x) in
    	let rec oba0 z oba1 y = (((oba0 z) (oba1  +  (z 1))) y) in (oba0 (z x));;


    Types after conversions:
    val a : int -> int -> 'a -> 'b |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      Types:
      val f : 'a -> int -> int

      Converted structure:
      let f x y =
      	let oba0 y z = (y  +  z) in
      	let oba1 oba0 oba2 = ((oba0 1)  +  oba2) in (((oba0 y) 1)  +  ((oba1 (oba0 y)) 2));;


      Types after conversions:
      val f : 'a -> int -> int |}]
;;
