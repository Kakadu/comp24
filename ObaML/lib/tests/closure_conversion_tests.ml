(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* open ObaML
open Format

let print_result val_pp = function
  | Ok v -> Stdlib.Format.printf "%a" val_pp v
  | Error _ -> Stdlib.Printf.printf "Syntax error"
;;

let parse_and_print s =
  Parser.structure_from_string s |> print_result Ast_pretty_printer.print_structure
;;

let parse_and_closure_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = Closure_conversion.run_closure_conversion parse_result in
    printf "%a" Ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_and_closure_result {| fun x -> fun y -> y + x |};
  [%expect {| (fun x -> ((fun x y -> ((( + ) y) x)) x)) |}]
;;

let%expect_test "" =
  parse_and_closure_result {| fun x -> fun y -> fun z -> z y x |};
  [%expect {| (fun x -> ((fun x y -> (((fun y x z -> ((z y) x)) y) x)) x)) |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> x|};
  [%expect {| let a x = x;; |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a x = fun y -> x + y|};
  [%expect {| let a x = ((fun x y -> ((( + ) x) y)) x);; |}]
;;

let%expect_test "" =
  parse_and_closure_result {| let a = fun x -> let b = x in b;;|};
  [%expect {| let a x = let b x = x in (b x);; |}]
;;

let%expect_test "" =
  parse_and_print
    {| let gen seed1 seed2 = 
  let gen n = n * seed2 + seed1 * 42 in
  gen 0 :: [gen 1; gen 2; gen 3]|};
  [%expect
    {| let gen seed1 seed2 = let gen n = ((( + ) ((( * ) n) seed2)) ((( * ) seed1) 42)) in (gen 0) :: (gen 1) :: (gen 2) :: (gen 3) :: [];; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let gen seed1 seed2 = 
  let gen n = n * seed2 + seed1 * 42 in
  gen 0 :: [gen 1; gen 2; gen 3]|};
  [%expect
    {| let gen seed1 seed2 = let gen seed1 seed2 n = ((( + ) ((( * ) n) seed2)) ((( * ) seed1) 42)) in (((gen seed1) seed2) 0) :: (((gen seed1) seed2) 1) :: (((gen seed1) seed2) 2) :: (((gen seed1) seed2) 3) :: [];; |}]
;;

let%expect_test "" =
  parse_and_print
    {| let fac n =
  let rec fack n k =
  if n <= 1 then k 1
  else fack (n - 1) (fun m -> k (m * n))
  in
  fack n (fun x -> x) |};
  [%expect
    {| let fac n = let rec fack n k = if ((( <= ) n) 1) then (k 1) else ((fack ((( - ) n) 1)) (fun m -> (k ((( * ) m) n)))) in ((fack n) (fun x -> x));; |}]
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
    {| let fac n = let rec fack n k = if ((( <= ) n) 1) then (k 1) else ((fack ((( - ) n) 1)) (((fun n k m -> (k ((( * ) m) n))) n) k)) in ((fack n) (fun x -> x));; |}]
;;

let%expect_test "" =
  parse_and_closure_result
    {| let main x = 
  let const f = fun s -> f in
   let rev_const f s = const s in
  rev_const (fun _ -> x)|};
  [%expect
    {| let main x = let const f = ((fun f s -> f) f) in let rev_const f s = (const s) in (rev_const ((fun x _ -> x) x));; |}]
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
      let add_cps x y = (((fun y x k -> (k ((( + ) x) y))) y) x);;
      let square_cps x = ((fun x k -> (k ((( * ) x) x))) x);;
      let pythagoras_cps x y = (((fun y x k -> ((square_cps x) (((fun y k x_squared -> ((square_cps y) (((fun x_squared k y_squared -> (((add_cps x_squared) y_squared) k)) x_squared) k))) y) k))) y) x);; |}]
;; *)
