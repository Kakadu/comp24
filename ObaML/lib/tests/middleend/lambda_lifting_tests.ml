(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_and_lift_lambdas_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
    (* let structure = Alpha_conversion.run_alpha_conversion structure in *)
    let structure = Closure_conversion.run_closure_conversion structure in
    let structure = Lambda_lifting.run_lambda_lifting structure in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| fun x -> fun y -> y + x |};
  [%expect
    {|
    let fobaml1 x y = (y  +  x);;

    let fobaml0 x = (fobaml1 x);;

    fobaml0 |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| fun x -> fun y -> fun z -> z y x |};
  [%expect
    {|
    let fobaml2 x y z = ((z y) x);;

    let fobaml1 x y = ((fobaml2 x) y);;

    let fobaml0 x = (fobaml1 x);;

    fobaml0 |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a = fun x -> x|};
  [%expect {|
    let a x = x;; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a x = fun y -> x + y|};
  [%expect {|
    let fobaml0 x y = (x  +  y);;

    let a x = (fobaml0 x);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a = fun x -> let b = x in b;;|};
  [%expect {|
    let a x =
    	let b = x in b;; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let gen seed1 seed2 = 
  let gen n = n * seed2 + seed1 * 42 in
  gen 0 :: [gen 1; gen 2; gen 3]|};
  [%expect
    {|
      let gen seed1 seed2 n = ((n  *  seed2)  +  (seed1  *  42));;

      let gen seed1 seed2 = (((gen seed1) seed2) 0) :: (((gen seed1) seed2) 1) :: (((gen seed1) seed2) 2) :: (((gen seed1) seed2) 3) :: [];; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let fac n =
  let rec fack n k =
  if n <= 1 then k 1
  else fack (n - 1) (fun m -> k (m * n))
  in
  fack n (fun x -> x) |};
  [%expect
    {|
      let fobaml0 k n m = (k (m  *  n));;

      let rec fack n k =
      	if (n  <=  1)
      	then (k 1)
      	else ((fack (n  -  1)) ((fobaml0 k) n));;

      let fobaml1 x = x;;

      let fac n = ((fack n) fobaml1);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let main x = 
  let const f = fun s -> f in
   let rev_const f s = const s in
  rev_const (fun _ -> x)|};
  [%expect
    {|
      let fobaml0 f s = f;;

      let const f = (fobaml0 f);;

      let rev_const const f s = (const s);;

      let fobaml1 x #pat#0 = x;;

      let main x = ((rev_const const) (fobaml1 x));; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| 
 let add_cps x y = fun k -> k (x + y)
   let square_cps x = fun k -> k (x * x)
   let pythagoras_cps x y = fun k ->
      square_cps x (fun x_squared ->
        square_cps y (fun y_squared ->
          add_cps x_squared y_squared k)) |};
  [%expect
    {|
      let fobaml0 x y k = (k (x  +  y));;

      let add_cps x y = ((fobaml0 x) y);;

      let fobaml1 x k = (k (x  *  x));;

      let square_cps x = (fobaml1 x);;

      let fobaml4 k x_squared y_squared = (((add_cps x_squared) y_squared) k);;

      let fobaml3 k y x_squared = ((square_cps y) ((fobaml4 k) x_squared));;

      let fobaml2 x y k = ((square_cps x) ((fobaml3 k) y));;

      let pythagoras_cps x y = ((fobaml2 x) y);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec y = fun x -> let a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      let b a x = (a  +  x);;

      let rec y x =
      	let a = (y x) in ((b a) 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec y = fun x -> let rec a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      let b a x = (a  +  x);;

      let rec y x =
      	let rec a = (y x) in ((b a) 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;  |};
  [%expect
    {|
      let a x y z = ((y x)  +  z);;

      let b a x = ((a 5)  +  x);;

      let rec y x = ((b ((a x) y)) 5);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let f a b = 
          let a x = b + x in a + b|};
  [%expect {|
      let a b x = (b  +  x);;

      let f a b = ((a b)  +  b);; |}]
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

      let fobaml0 #tuple_getter# f p =
      	let #pat#0 = p in
      	let a = ((#tuple_getter# 0) #pat#0) in
      	let b = ((#tuple_getter# 1) #pat#0) in ((f b), (f a));;

      let map = (fobaml0 #tuple_getter#);;

      let fobaml2 l self li x = ((li (self l)) x);;

      let fobaml1 self l = ((map ((fobaml2 l) self)) l);;

      let fixpoly l = ((fix fobaml1) l);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result {| let a x = let z = (fun zet -> zet + x) in z;; |};
  [%expect {|
      let z x zet = (zet  +  x);;

      let a x = (z x);; |}]
;;

let%expect_test "" =
  parse_and_lift_lambdas_result
    {| 
  let a x = let z = (fun zet -> zet + x) in
      let rec a x y = a (x + z 1) y in 
         a;;
;; |};
  [%expect
    {|
      let z x zet = (zet  +  x);;

      let rec a z x y = (((a z) (x  +  (z 1))) y);;

      let a x = (a (z x));; |}]
;;
