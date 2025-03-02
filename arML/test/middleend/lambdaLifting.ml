(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Basic *)

let%expect_test _ =
  lambda_lifting_program {| let _ = (fun x -> x) 0 |};
  [%expect {|
    let ll_0 x = x
    let _ = (ll_0 0) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f x = (fun y -> x + y) 0 |};
  [%expect {|
    let ll_0 cc_0 y = (( + ) cc_0 y)
    let f x = ((ll_0 x) 0) |}]
;;

(* ---------------- *)

(* Let in *)

let%expect_test _ =
  lambda_lifting_program {| let _ = let f x = x in f 0 |};
  [%expect {|
    let ll_0 x = x
    let _ = (ll_0 0) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let _ = let x = 3 in x + 1 |};
  [%expect {|
    let _ = (let x = 3 in (( + ) x 1)) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let _ = let (x, y) = (1, 2) in x + y |};
  [%expect {|
    let _ = (let (x, y) = (1, 2) in (( + ) x y)) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let _ = let f x = x in let g y = y + y in f (g 1) |};
  [%expect
    {|
    let ll_0 x = x
    let ll_1 y = (( + ) y y)
    let _ = (ll_0 (ll_1 1)) |}]
;;

(* ---------------- *)

(* Let in with and *)

let%expect_test _ =
  lambda_lifting_program {| let _ = let f x = x and g x = x in f g |};
  [%expect {|
    let ll_0 x = x
    let ll_1 x = x
    let _ = (ll_0 ll_1) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let _ = let f x = x and g = 5 in f g |};
  [%expect {|
    let ll_0 x = x
    let _ = (let g = 5 in (ll_0 g)) |}]
;;

let%expect_test _ =
  lambda_lifting_program
    {| let _ = let (x, y) = (1, 2) and (z :: _) = (1 :: [2]) in x + y + z |};
  [%expect
    {|
    let _ = (let (z :: _) = (1 :: (2 :: [])) and (x, y) = (1, 2) in (( + ) (( + ) x y) z)) |}]
;;

let%expect_test _ =
  lambda_lifting_program
    {| let _ = let (x, y) = (1, 2) and (z :: _) = (1 :: [2]) and f x = x*x*x in f (x + y + z) |};
  [%expect
    {|
    let ll_0 x = (( * ) (( * ) x x) x)
    let _ = (let (z :: _) = (1 :: (2 :: [])) and (x, y) = (1, 2) in (ll_0 (( + ) (( + ) x y) z))) |}]
;;

(* ---------------- *)

(* Recursive let in *)

let%expect_test _ =
  lambda_lifting_program {| let _ = let rec f x = f x in f |};
  [%expect {|
    let rec ll_0 x = (ll_0 x)
    let _ = ll_0 |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let _ = let rec f x = f x in let rec g x = g x in (f, g) |};
  [%expect
    {|
    let rec ll_0 x = (ll_0 x)
    let rec ll_1 x = (ll_1 x)
    let _ = (ll_0, ll_1) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let _ = let rec f x = (fun x -> f x) x in f |};
  [%expect {|
    let rec ll_1 x = (ll_0 x) and ll_0 x = (ll_1 x)
    let _ = ll_0 |}]
;;

let%expect_test _ =
  lambda_lifting_program
    {| let _ = let rec fac x = if x = 1 then x else x * fac (x-1) in fac 1 |};
  [%expect
    {|
    let rec ll_0 x = (if (( = ) x 1) then x else (( * ) x (ll_0 (( - ) x 1))))
    let _ = (ll_0 1) |}]
;;

(* ---------------- *)

(* Let in with mutual recursion *)

let%expect_test _ =
  lambda_lifting_program {| let _ = let rec f x = g x and g x = f x in (f, g) |};
  [%expect
    {|
    let rec ll_0 x = (ll_1 x) and ll_1 x = (ll_0 x)
    let _ = (ll_0, ll_1) |}]
;;

let%expect_test _ =
  lambda_lifting_program
    {| let _ = let rec f x = g x and g x = (fun _ -> f x) () in (f, g) |};
  [%expect
    {|
    let rec ll_0 x = (ll_1 x) and ll_2 cc_0 _ = (ll_0 cc_0) and ll_1 x = ((ll_2 x) ())
    let _ = (ll_0, ll_1) |}]
;;

let%expect_test _ =
  lambda_lifting_program
    {| let _ = let rec f x = g x and g x = f x and x = 5 in (f, g, x) |};
  [%expect
    {|
    let rec ll_0 x = (ll_1 ll_2) and ll_1 x = (ll_0 ll_2) and ll_2 = 5
    let _ = (ll_0, ll_1, ll_2) |}]
;;

(* ---------------- *)

(* Ordinary declarations *)

let%expect_test _ =
  lambda_lifting_program {| let f = (fun x -> x) 0 |};
  [%expect {|
    let ll_0 x = x
    let f = (ll_0 0) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f = (fun x -> x) 0 and g x = (fun _ -> x) () |};
  [%expect
    {|
    let ll_1 cc_0 _ = cc_0
    let ll_0 x = x
    let f = (ll_0 0) and g x = ((ll_1 x) ()) |}]
;;

let%expect_test _ =
  lambda_lifting_program
    {| let f = (fun x -> x) 0 and g x = (fun _ -> x) ();; let h x y = x + ((fun x -> x * x) y) |};
  [%expect
    {|
    let ll_2 cc_0 _ = cc_0
    let ll_1 x = x
    let ll_0 x = (( * ) x x)
    let f = (ll_1 0) and g x = ((ll_2 x) ())
    let h x y = (( + ) x (ll_0 y)) |}]
;;

(* ---------------- *)

(* Recursive declarations *)

let%expect_test _ =
  lambda_lifting_program {| let rec f x = g x and g x = f x |};
  [%expect {|
    let rec f x = (g x) and g x = (f x) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let rec f x = g x and g x = (fun () -> f x) () |};
  [%expect
    {|
    let rec f x = (g x) and g x = ((ll_0 x) ()) and ll_0 cc_0 () = (f cc_0) |}]
;;

(* ---------------- *)

(* Other expressions *)

let%expect_test _ =
  lambda_lifting_program
    {| let f = if (fun x -> x) true then (fun y -> y) 0 else (fun () -> 0) () |};
  [%expect
    {|
    let ll_0 x = x
    let ll_1 y = y
    let ll_2 () = 0
    let f = (if (ll_0 true) then (ll_1 0) else (ll_2 ())) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f = (function | hd :: tl -> hd | _ -> 0) [1; 2] |};
  [%expect
    {|
    let ll_0 cc_0 = (match cc_0 with (hd :: tl) -> hd | _ -> 0)
    let f = (ll_0 (1 :: (2 :: []))) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f x = match (fun _ -> x) with | _ -> (fun z -> z) 0 |};
  [%expect
    {|
    let ll_0 cc_0 _ = cc_0
    let ll_1 z = z
    let f x = (match (ll_0 x) with _ -> (ll_1 0)) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f = (fun (x : int) -> (x : int)) 0 |};
  [%expect {|
    let ll_0 (x : int) = (x : int)
    let f = (ll_0 0) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f = (fun x -> fun y -> y, fun z -> z) |};
  [%expect {|
    let ll_0 x y = y
    let ll_1 z = z
    let f = (ll_0, ll_1) |}]
;;

let%expect_test _ =
  lambda_lifting_program {| let f = (let f x = x in f 1) :: [(let g y = y + y in g 1)] |};
  [%expect
    {|
    let ll_0 x = x
    let ll_1 y = (( + ) y y)
    let f = ((ll_0 1) :: ((ll_1 1) :: [])) |}]
;;

(* ---------------- *)
