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

let parse_and_closure_result str =
  match Parser.structure_from_string str with
  | Ok structure ->
    let simple_structure = To_simple_ast.convert structure in
    let closure_structure = Closure_conversion.run_closure_conversion simple_structure in
    let new_structure = To_ast.convert closure_structure in
    Format.printf
      "Types:\n%a\nConverted structure:\n%a\nTypes after conversions:\n%a"
      infer_and_result
      structure
      Simple_ast_pretty_printer.print_structure
      closure_structure
      infer_and_result
      new_structure
  | Error _ -> printf "Syntax error"
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
      let a = (((((fun #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# x ->
      	let b x y = ((x  *  y)  *  52) :: 52 :: [] in
      	let #pat#0 = ((b x) 1) in
      	if ((((#list_length_getter# #pat#0)  =  2)  &&  ((#list_head_getter# #pat#0)  =  52))  &&  ((#list_head_getter# (#list_tail_getter# #pat#0))  =  52))
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (#matching_failed# ())) #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#);;


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
      let a = (((((fun #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# x ->
      	let b = (x  *  52) :: 52 :: [] in
      	let #pat#0 = b in
      	if ((((#list_length_getter# #pat#0)  =  2)  &&  ((#list_head_getter# #pat#0)  =  52))  &&  ((#list_head_getter# (#list_tail_getter# #pat#0))  =  52))
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (#matching_failed# ())) #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#);;


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
      let rev = (((((fun #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# lst ->
      	let rec helper #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# acc = (((((((fun #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# acc helper lst ->
      	let #pat#0 = lst in
      	if (#pat#0  =  [])
      	then acc
      	else
      	if ((#list_length_getter# #pat#0)  >=  1)
      	then
      	let h = (#list_head_getter# #pat#0) in
      	let tl = (#list_tail_getter# #pat#0) in ((helper h :: acc) tl)
      	else (#matching_failed# ())) #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#) acc) ((((helper #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#)) in ((((((helper #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#) []) lst)) #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#);;

      let reversed1 = (rev 1 :: 2 :: 3 :: 4 :: 5 :: []);;

      let reversed2 = (rev true :: false :: false :: false :: []);;


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
      	let b a x = (a  +  x) in ((b a) 5);;


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
      	let rec a = (y x) in
      	let b a x = (a  +  x) in ((b a) 5);;


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
      	let b a x = ((a 5)  +  x) in ((b ((a x) y)) 5);;


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
      	let z y x = (y  +  x) in (((z y) 5)  +  y);;


      Types after conversions:
      val f : 'a -> int -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result
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
      let rec fix f x = ((f (fix f)) x);;

      let helper f p = (f p);;

      let zet l = ((fix (fun self l -> ((helper (((fun l self li x -> ((li (self l)) x)) l) self)) l))) l);;


      Types after conversions:
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
      val helper : ('a -> 'b) -> 'a -> 'b
      val zet : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a x = let z = (fun zet -> zet + x) in z;; |};
  [%expect
    {|
      Types:
      val a : int -> int -> int

      Converted structure:
      let a x =
      	let z x zet = (zet  +  x) in (z x);;


      Types after conversions:
      val a : int -> int -> int |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| 
  let a x = let z = (fun zet -> zet + x) in
      let rec a x y = a (x + z 1) y in 
         a;;
;; |};
  [%expect
    {|
      Types:
      val a : int -> int -> 'a -> 'b

      Converted structure:
      let a x =
      	let z x zet = (zet  +  x) in
      	let rec a z x y = (((a z) (x  +  (z 1))) y) in (a (z x));;


      Types after conversions:
      val a : int -> int -> 'a -> 'b |}]
;;

let parse_and_closure_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
    let structure = Alpha_conversion.run_alpha_conversion structure Inner in
    let structure = Closure_conversion.run_closure_conversion structure in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_closure_result
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      let f x y =
      	let oba0 y z = (y  +  z) in
      	let oba1 oba0 z = ((oba0 1)  +  z) in (((oba0 y) 1)  +  ((oba1 (oba0 y)) 2));; |}]
;;
