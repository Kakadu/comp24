(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let print_result val_pp = function
  | Ok v -> Stdlib.Format.printf "%a" val_pp v
  | Error _ -> Stdlib.Printf.printf "Syntax error"
;;

let parse_and_closure_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
    let structure = Closure_conversion.run_closure_conversion structure in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_closure_result {| fun x -> fun y -> y + x |};
  [%expect {| (fun x -> ((fun x y -> (y  +  x)) x)) |}]
;;

let%expect_test "" =
  parse_and_closure_result {| fun x -> fun y -> fun z -> z y x |};
  [%expect {| (fun x -> ((fun x y -> (((fun x y z -> ((z y) x)) x) y)) x)) |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> x|};
  [%expect {| let a x = x;; |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a x = fun y -> x + y|};
  [%expect {| let a x = ((fun x y -> (x  +  y)) x);; |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> let b = x in b;;|};
  [%expect {| let a x = let b = x in b;; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let gen seed1 seed2 = 
  let gen n = n * seed2 + seed1 * 42 in
  gen 0 :: [gen 1; gen 2; gen 3]|};
  [%expect
    {| let gen seed1 seed2 = let gen seed1 seed2 n = ((n  *  seed2)  +  (seed1  *  42)) in (((gen seed1) seed2) 0) :: (((gen seed1) seed2) 1) :: (((gen seed1) seed2) 2) :: (((gen seed1) seed2) 3) :: [];; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let fac n =
  let rec fack n k =
  if n <= 1 then k 1
  else fack (n - 1) (fun m -> k (m * n))
  in
  fack n (fun x -> x) |};
  [%expect
    {| let fac n = let rec fack n k = if (n  <=  1) then (k 1) else ((fack (n  -  1)) (((fun k n m -> (k (m  *  n))) k) n)) in ((fack n) (fun x -> x));; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let main x = 
  let const f = fun s -> f in
   let rev_const f s = const s in
  rev_const (fun _ -> x)|};
  [%expect
    {| let main x = let const f = ((fun f s -> f) f) in let rev_const const f s = (const s) in ((rev_const const) ((fun x #gen_pat_expr#0 -> x) x));; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| 
 let add_cps x y = fun k -> k (x + y)
   let square_cps x = fun k -> k (x * x)
   let pythagoras_cps x y = fun k ->
      square_cps x (fun x_squared ->
        square_cps y (fun y_squared ->
          add_cps x_squared y_squared k)) |};
  [%expect
    {|
      let add_cps x y = (((fun x y k -> (k (x  +  y))) x) y);;
      let square_cps x = ((fun x k -> (k (x  *  x))) x);;
      let pythagoras_cps x y = (((fun x y k -> ((square_cps x) (((fun k y x_squared -> ((square_cps y) (((fun k x_squared y_squared -> (((add_cps x_squared) y_squared) k)) k) x_squared))) k) y))) x) y);; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec y = fun x -> let a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      let rec y x = let a = (y x) in let b a x = (a  +  x) in ((b a) 5);; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec y = fun x -> let rec a = y x in let b = fun x -> a + x in b 5;;|};
  [%expect
    {|
      let rec y x = let rec a = (y x) in let b a x = (a  +  x) in ((b a) 5);; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;  |};
  [%expect
    {|
      let rec y x = let a x y z = ((y x)  +  z) in let b a x = ((a 5)  +  x) in ((b ((a x) y)) 5);; |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let f a b = 
          let a x = b + x in a + b|};
  [%expect {|
      let f a b = let a b x = (b  +  x) in ((a b)  +  b);; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l |};
  [%expect
    {|
      let rec fix f x = ((f (fix f)) x);;
      let map f p = let #gen_pat_expr#0 = p in let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let b = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in ((f a), (f b));;
      let fixpoly l = ((fix (fun self l -> ((map (((fun l self li x -> ((li (self l)) x)) l) self)) l))) l);; |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a x = let z = (fun zet -> zet + x) in z;; |};
  [%expect {|
      let a x = let z x zet = (zet  +  x) in (z x);; |}]
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
      let a x = let z x zet = (zet  +  x) in let rec a z x y = (((a z) (x  +  (z 1))) y) in (a (z x));; |}]
;;

let parse_and_closure_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
    let structure = Alpha_conversion.run_alpha_conversion structure in
    let structure = Closure_conversion.run_closure_conversion structure in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_closure_result
    {| let f a b = 
      let a x = b + x in 
      let b x = a 1 + x in
      a 1 + b 2;; |};
  [%expect
    {|
      let obaml0 obaml1 obaml2 = let obaml3 obaml2 obaml4 = (obaml2  +  obaml4) in let obaml5 obaml3 obaml6 = ((obaml3 1)  +  obaml6) in (((obaml3 obaml2) 1)  +  ((obaml5 (obaml3 obaml2)) 2));; |}]
;;
