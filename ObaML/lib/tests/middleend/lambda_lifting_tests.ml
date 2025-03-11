(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_and_lift_lambdas_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure, varSet = Alpha_conversion.run_alpha_conversion parse_result in
    let structure = To_simple_ast.convert structure in
    let structure = Closure_conversion.run_closure_conversion structure in
    let structure, _ = Lambda_lifting.run_lambda_lifting structure varSet in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a = fun x -> fun y -> y + x |};
  [%expect {|
    let oba0 x y = (y  +  x);;

    let a x = (oba0 x);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a = fun x -> fun y -> fun z -> z y x |};
  [%expect
    {|
    let oba1 x y z = ((z y) x);;

    let oba0 x y = ((oba1 x) y);;

    let a x = (oba0 x);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a x = fun y -> x + y|};
  [%expect {|
    let oba0 x y = (x  +  y);;

    let a x = (oba0 x);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a = fun x -> let b = x in b;;|};
  [%expect {|
    let a x =
    	let b = x in b;; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let a x = let b y = (x * y * 52) :: 52 :: [] in match b 1 with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      let b x y = (((x  *  y)  *  52) :: (52 :: []));;

      let a x =
      	let pat0 = ((b x) 1) in
      	if ((((list_length_getter pat0)  =  2)  &&  ((list_head_getter pat0)  =  52))  &&  ((list_head_getter (list_tail_getter pat0))  =  52))
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (matching_failed ());; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let a x = let b = (x * 52) :: 52 :: [] in match b with 52 :: 52 :: [] -> "Nice" | _ -> "Bad" |};
  [%expect
    {|
      let a x =
      	let b = ((x  *  52) :: (52 :: [])) in
      	let pat0 = b in
      	if ((((list_length_getter pat0)  =  2)  &&  ((list_head_getter pat0)  =  52))  &&  ((list_head_getter (list_tail_getter pat0))  =  52))
      	then "Nice"
      	else
      	if true
      	then "Bad"
      	else (matching_failed ());; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
    let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
    let a = fac 5 |};
  [%expect
    {|
      let oba0 f fix x = ((f (fix f)) x);;

      let rec fix f = ((oba0 f) fix);;

      let oba2 self n =
      	if (n  <=  1)
      	then 1
      	else (n  *  (self (n  -  1)));;

      let oba1 self = (oba2 self);;

      let fac = (fix oba1);;

      let a = (fac 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
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
    let oba1 acc helper oba0 =
    	let pat0 = oba0 in
    	if (pat0  =  [])
    	then acc
    	else
    	if ((list_length_getter pat0)  >=  1)
    	then
    	let h = (list_head_getter pat0) in
    	let tl = (list_tail_getter pat0) in ((helper (h :: acc)) tl)
    	else (matching_failed ());;

    let rec helper acc = ((oba1 acc) helper);;

    let rev lst = ((helper []) lst);;

    let reversed1 = (rev (1 :: (2 :: (3 :: (4 :: (5 :: []))))));;

    let reversed2 = (rev (true :: (false :: (false :: (false :: [])))));; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec y = fun x -> let a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      let b a oba0 = (a  +  oba0);;

      let rec y x =
      	let a = (y x) in ((b a) 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec y = fun x -> let rec a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      let b a oba0 = (a  +  oba0);;

      let rec y x =
      	let a = (y x) in ((b a) 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;  |};
  [%expect
    {|
      let a x y z = ((y x)  +  z);;

      let b a oba0 = ((a 5)  +  oba0);;

      let rec y x = ((b ((a x) y)) 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let f z y = let z x = y + x in z 5 + y|};
  [%expect
    {|
      let oba0 y x = (y  +  x);;

      let f z y = (((oba0 y) 5)  +  y);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec fix f x = f (fix f) x
  let helper f p = f p
  let zu l =
    fix (fun self l -> helper (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      let rec fix f x = ((f (fix f)) x);;

      let helper oba0 p = (oba0 p);;

      let oba4 oba1 self li oba2 = ((li (self oba1)) oba2);;

      let oba3 self oba1 = ((helper ((oba4 oba1) self)) oba1);;

      let zu l = ((fix oba3) l);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a x = let z = (fun zu -> zu + x) in z;; |};
  [%expect {|
      let z x zu = (zu  +  x);;

      let a x = (z x);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| 
  let rec a x = let z = (fun zu -> zu + x) in
      let rec a x y = a (x + z 1) y in 
         a;;
;; |};
  [%expect
    {|
    let z x zu = (zu  +  x);;

    let rec oba0 z oba1 y = (((oba0 z) (oba1  +  (z 1))) y);;

    let rec a x = (oba0 (z x));; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      let oba0 y z = (y  +  z);;

      let oba1 oba0 oba2 = ((oba0 1)  +  oba2);;

      let f x y = (((oba0 y) 1)  +  ((oba1 (oba0 y)) 2));; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      let rec fix f x = ((f (fix f)) x);;

      let map oba0 p =
      	let pat0 = p in
      	let a = ((tuple_getter 0) pat0) in
      	let b = ((tuple_getter 1) pat0) in ((oba0 a), (oba0 b));;

      let oba4 oba1 self li oba2 = ((li (self oba1)) oba2);;

      let oba3 self oba1 = ((map ((oba4 oba1) self)) oba1);;

      let fixpoly l = ((fix oba3) l);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a b c = if (b, c) = (5, 4) then 1 else 2;; |};
  [%expect
    {|
      let a b c =
      	if ((b, c)  =  (5, 4))
      	then 1
      	else 2;; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let x = let a = let b = 5 in b in a;; |};
  [%expect {|
      let x =
      	let a =
      	let b = 5 in b in a;; |}]
;;
