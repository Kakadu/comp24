(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Fun *)

let%expect_test _ =
  closure_conversion_expression {| fun x -> x |};
  [%expect {| (fun x -> x) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x -> y |};
  [%expect {| ((fun cc_0 x -> cc_0) y) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x -> x + y |};
  [%expect {| ((fun cc_0 x -> (( + ) x cc_0)) y) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x -> x + y + z |};
  [%expect {| ((fun cc_1 cc_0 x -> (( + ) (( + ) x cc_0) cc_1)) z y) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x -> fun arg_0 -> y |};
  [%expect {| ((fun cc_0 x arg_0 -> cc_0) y) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x -> ((fun x -> x + y) x z) |};
  [%expect
    {| ((fun cc_1 cc_0 x -> (((fun cc_2 x -> (( + ) x cc_2)) cc_0) x cc_1)) z y) |}]
;;

(* ---------------- *)

(* Application *)

let%expect_test _ =
  closure_conversion_expression {| (fun x -> x) y |};
  [%expect {| ((fun x -> x) y) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| (fun x -> x + y) y |};
  [%expect {| (((fun cc_0 x -> (( + ) x cc_0)) y) y) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| (fun x -> x) (fun x -> y) (fun _ -> z) |};
  [%expect {| ((fun x -> x) ((fun cc_0 x -> cc_0) y) ((fun cc_1 _ -> cc_1) z)) |}]
;;

(* ---------------- *)

(* Let in *)

let%expect_test _ =
  closure_conversion_expression {| let f x = x in f |};
  [%expect {| (let f x = x in f) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f = x + y in f |};
  [%expect {| (let f = (( + ) x y) in f) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = y in f |};
  [%expect {| (let f cc_0 x = cc_0 in (f y)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = x + y in f 0 |};
  [%expect {| (let f cc_0 x = (( + ) x cc_0) in ((f y) 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = let g y = x + y in g 0 in f 0 |};
  [%expect {| (let f x = (let g cc_0 y = (( + ) cc_0 y) in ((g x) 0)) in (f 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = let g y = x + y + z in g 0 in f 0 |};
  [%expect
    {| (let f cc_0 x = (let g cc_2 cc_1 y = (( + ) (( + ) cc_1 y) cc_2) in ((g cc_0 x) 0)) in ((f z) 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| let f arg_0 = let g y = x + y + arg_0 + arg_1 in g 0 in f 0 |};
  [%expect
    {| (let f cc_1 cc_0 arg_0 = (let g cc_4 cc_3 cc_2 y = (( + ) (( + ) (( + ) cc_4 y) cc_2) cc_3) in ((g cc_1 cc_0 arg_0) 0)) in ((f x arg_1) 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let (x, y) = (1, z) in x + y |};
  [%expect {| (let (x, y) = (1, z) in (( + ) x y)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let (x :: y) = (1 :: z) in x |};
  [%expect {| (let (x :: y) = (1 :: z) in x) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = f x in f |};
  [%expect {| (let cc_0 cc_1 x = (cc_1 x) in (cc_0 f)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun f -> let f x = f x in f |};
  [%expect {| (fun f -> (let cc_0 cc_1 x = (cc_1 x) in (cc_0 f))) |}]
;;

(* ---------------- *)

(* Let in with and *)

let%expect_test _ =
  closure_conversion_expression {| let f x = x and g x = x in (f 0, g 0) |};
  [%expect {| (let f x = x and g x = x in ((f 0), (g 0))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = x and g x = x + y in (f 0, g 0) |};
  [%expect {| (let f x = x and g cc_0 x = (( + ) x cc_0) in ((f 0), ((g y) 0))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = x + z and g x = x + y in (f 0, g 0) |};
  [%expect
    {| (let f cc_0 x = (( + ) x cc_0) and g cc_1 x = (( + ) x cc_1) in (((f z) 0), ((g y) 0))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = x and g x = f x in (f 0, g 0) |};
  [%expect {| (let cc_0 x = x and g cc_1 x = (cc_1 x) in ((cc_0 0), ((g f) 0))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let f x = g x and g x = f x in (f 0, g 0) |};
  [%expect
    {| (let cc_0 cc_2 x = (cc_2 x) and cc_1 cc_3 x = (cc_3 x) in (((cc_0 g) 0), ((cc_1 f) 0))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let (x, y) = (1, 2) and g k = k in (x, y, g 1) |};
  [%expect {| (let (x, y) = (1, 2) and g k = k in (x, y, (g 1))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let (x, y) = (1, 2) and g k = k + x in (x, y, g 1) |};
  [%expect
    {| (let (cc_0, y) = (1, 2) and g cc_1 k = (( + ) k cc_1) in (cc_0, y, ((g x) 1))) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| let (x, y) = (1, 2) and g k = k + x + y in (x, y, g 1) |};
  [%expect
    {| (let (cc_0, cc_1) = (1, 2) and g cc_3 cc_2 k = (( + ) (( + ) k cc_2) cc_3) in (cc_0, cc_1, ((g y x) 1))) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| let (x :: y) = (1 :: 2) and g k = k + x + y in (x, y, g 1) |};
  [%expect
    {| (let (cc_0 :: cc_1) = (1 :: 2) and g cc_3 cc_2 k = (( + ) (( + ) k cc_2) cc_3) in (cc_0, cc_1, ((g y x) 1))) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| let (x, arg_0) = (1, 2) and g k = k + x + y in (x, arg_0, g 1) |};
  [%expect
    {| (let (cc_0, arg_0) = (1, 2) and g cc_2 cc_1 k = (( + ) (( + ) k cc_1) cc_2) in (cc_0, arg_0, ((g y x) 1))) |}]
;;

(* ---------------- *)

(* Recursion let in *)

let%expect_test _ =
  closure_conversion_expression {| let rec f x = f x in f |};
  [%expect {| (let rec f x = (f x) in f) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = f y in f |};
  [%expect {| (let rec f cc_0 x = ((f cc_0) cc_0) in (f y)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| let rec f x = if x = 1 then x else x * f (x - 1) in f 5 |};
  [%expect
    {| (let rec f x = (if (( = ) x 1) then x else (( * ) x (f (( - ) x 1)))) in (f 5)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| let rec f _ = if x = 1 then x else x * f (x - 1) in f 5 |};
  [%expect
    {| (let rec f cc_0 _ = (if (( = ) cc_0 1) then cc_0 else (( * ) cc_0 ((f cc_0) (( - ) cc_0 1)))) in ((f x) 5)) |}]
;;

(* ---------------- *)

(* Mutual recursion let in *)

let%expect_test _ =
  closure_conversion_expression {| let rec f x = f x and g x = g x in (f, g) |};
  [%expect {| (let rec f x = (f x) and g x = (g x) in (f, g)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = f x and g x = x + g y in (f, g) |};
  [%expect
    {| (let rec f x = (f x) and g cc_0 x = (( + ) x ((g cc_0) cc_0)) in (f, (g y))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = x + f y  and g x = g x in (f, g) |};
  [%expect
    {| (let rec f cc_0 x = (( + ) x ((f cc_0) cc_0)) and g x = (g x) in ((f y), g)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = g x and g x = f x in (f, g) |};
  [%expect {| (let rec f x = (g x) and g x = (f x) in (f, g)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = g x and g x = f y in (f, g) |};
  [%expect
    {| (let rec f cc_1 x = ((g cc_1) x) and g cc_0 x = ((f cc_0) cc_0) in ((f y), (g y))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = g y and g x = f x in (f, g) |};
  [%expect
    {| (let rec f cc_0 x = ((g cc_0) cc_0) and g cc_1 x = ((f cc_1) x) in ((f y), (g y))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = g y and g x = f y in (f, g) |};
  [%expect
    {| (let rec f cc_0 x = ((g cc_0) cc_0) and g cc_1 x = ((f cc_1) cc_1) in ((f y), (g y))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| let rec f x = g y and g x = f z in (f, g) |};
  [%expect
    {| (let rec f cc_2 cc_0 x = (((g cc_0) cc_2) cc_0) and g cc_3 cc_1 x = (((f cc_1) cc_3) cc_1) in (((f z) y), ((g y) z))) |}]
;;

(* ---------------- *)

(* If then else *)

let%expect_test _ =
  closure_conversion_expression
    {| if x > 0 then (fun x -> x + y) x else (fun x y -> x + y) x x |};
  [%expect
    {| (if (( > ) x 0) then (((fun cc_0 x -> (( + ) x cc_0)) y) x) else ((fun x y -> (( + ) x y)) x x)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| if ((fun x -> x + k) x) > 0 then (fun x -> x + y) x else (fun x y -> x + y) x x |};
  [%expect
    {| (if (( > ) (((fun cc_0 x -> (( + ) x cc_0)) k) x) 0) then (((fun cc_1 x -> (( + ) x cc_1)) y) x) else ((fun x y -> (( + ) x y)) x x)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x -> if x > y then x else y |};
  [%expect {| ((fun cc_0 x -> (if (( > ) x cc_0) then x else cc_0)) y) |}]
;;

(* ---------------- *)

(* Tuples *)

let%expect_test _ =
  closure_conversion_expression {| ((fun x -> x), (fun x -> y), (fun _ -> z)) |};
  [%expect {| ((fun x -> x), ((fun cc_0 x -> cc_0) y), ((fun cc_1 _ -> cc_1) z)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x y -> (x, y, z) |};
  [%expect {| ((fun cc_0 x y -> (x, y, cc_0)) z) |}]
;;

(* ---------------- *)

(* Lists *)

let%expect_test _ =
  closure_conversion_expression {| (fun x -> x) :: (fun x -> y) :: (fun _ -> z) |};
  [%expect {| ((fun x -> x) :: (((fun cc_0 x -> cc_0) y) :: ((fun cc_1 _ -> cc_1) z))) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun x y z -> (x, y, z) :: [(k, m, n)] |};
  [%expect
    {| ((fun cc_2 cc_1 cc_0 x y z -> ((x, y, z) :: ((cc_0, cc_1, cc_2) :: []))) n m k) |}]
;;

(* ---------------- *)

(* Match with *)

let%expect_test _ =
  closure_conversion_expression {| match x with | (m, n) -> m + n | _ -> 0 |};
  [%expect {| (match x with (m, n) -> (( + ) m n) | _ -> 0) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| match x with | (m, n) -> (fun _ -> m + n) () | _ -> 0 |};
  [%expect
    {| (match x with (m, n) -> (((fun cc_1 cc_0 _ -> (( + ) cc_0 cc_1)) n m) ()) | _ -> 0) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> match x with | (m, n) -> (fun _ -> m + n) () | _ -> 0 |};
  [%expect
    {| (fun x -> (match x with (m, n) -> (((fun cc_1 cc_0 _ -> (( + ) cc_0 cc_1)) n m) ()) | _ -> 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> match x with | (m, n) -> (fun _ -> m + n + k) () | _ -> 0 |};
  [%expect
    {| ((fun cc_0 x -> (match x with (m, n) -> (((fun cc_3 cc_2 cc_1 _ -> (( + ) (( + ) cc_2 cc_3) cc_1)) n m cc_0) ()) | _ -> 0)) k) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> (match ((fun x -> x + y) 1) with 5 -> 0 | _ -> (fun x -> x + y + 1) 1) |};
  [%expect
    {| ((fun cc_0 x -> (match (((fun cc_1 x -> (( + ) x cc_1)) cc_0) 1) with 5 -> 0 | _ -> (((fun cc_2 x -> (( + ) (( + ) x cc_2) 1)) cc_0) 1))) y) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> (match ((fun x -> x + y) 1) with 5 -> 0 | _ -> (fun x -> x + z + 1) 1) |};
  [%expect
    {| ((fun cc_1 cc_0 x -> (match (((fun cc_2 x -> (( + ) x cc_2)) cc_0) 1) with 5 -> 0 | _ -> (((fun cc_3 x -> (( + ) (( + ) x cc_3) 1)) cc_1) 1))) z y) |}]
;;

(* ---------------- *)

(* Function *)

let%expect_test _ =
  closure_conversion_expression {| function | (m, n) -> m + n | _ -> 0 |};
  [%expect {| (fun cc_0 -> (match cc_0 with (m, n) -> (( + ) m n) | _ -> 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| function | (m, n) -> (fun _ -> m + n) () | _ -> 0 |};
  [%expect
    {| (fun cc_0 -> (match cc_0 with (m, n) -> (((fun cc_2 cc_1 _ -> (( + ) cc_1 cc_2)) n m) ()) | _ -> 0)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> function | (m, n) -> (fun _ -> m + n) () | _ -> x |};
  [%expect
    {| (fun x -> ((fun cc_3 cc_0 -> (match cc_0 with (m, n) -> (((fun cc_2 cc_1 _ -> (( + ) cc_1 cc_2)) n m) ()) | _ -> cc_3)) x)) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> function | (m, n) -> (fun _ -> m + n + k) () | _ -> x + k |};
  [%expect
    {| ((fun cc_0 x -> ((fun cc_6 cc_5 cc_1 -> (match cc_1 with (m, n) -> (((fun cc_4 cc_3 cc_2 _ -> (( + ) (( + ) cc_3 cc_4) cc_2)) n m cc_5) ()) | _ -> (( + ) cc_6 cc_5))) x cc_0)) k) |}]
;;

let%expect_test _ =
  closure_conversion_expression
    {| fun x -> function | (m, n) -> (fun _ -> m + n + k) () | _ -> x + z |};
  [%expect
    {| ((fun cc_1 cc_0 x -> ((fun cc_8 cc_7 cc_6 cc_2 -> (match cc_2 with (m, n) -> (((fun cc_5 cc_4 cc_3 _ -> (( + ) (( + ) cc_4 cc_5) cc_3)) n m cc_6) ()) | _ -> (( + ) cc_7 cc_8))) cc_1 x cc_0)) z k) |}]
;;

(* ---------------- *)

(* Typed expressions *)

let%expect_test _ =
  closure_conversion_expression {| ((fun (x : int) -> (x : int)) : int -> int) |};
  [%expect {| ((fun (x : int) -> (x : int)) : (int -> int)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| ((fun (x : int) -> (y : int)) : int -> int) |};
  [%expect {| (((fun cc_0 (x : int) -> (cc_0 : int)) y) : (int -> int)) |}]
;;

let%expect_test _ =
  closure_conversion_expression {| fun (x : int) -> if ((x > y) : bool) then x else y |};
  [%expect
    {| ((fun cc_0 (x : int) -> (if ((( > ) x cc_0) : bool) then x else cc_0)) y) |}]
;;

(* ---------------- *)

(* Ordinary declarations *)

let%expect_test _ =
  closure_conversion_program {| let f x = x |};
  [%expect {| let f x = x |}]
;;

let%expect_test _ =
  closure_conversion_program {| 
  let f x = x 
  let g x = x
  |};
  [%expect {|
    let f x = x
    let g x = x 
    |}]
;;

let%expect_test _ =
  closure_conversion_program {| 
  let f x = fun y -> x
  let g x = f x
  |};
  [%expect {|
    let f x y = x
    let g x = (f x)
    |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
  let f x = (fun y -> x + y) 1
  let g x = f ((fun y -> x) ())
  |};
  [%expect
    {|
    let f x = (((fun cc_0 y -> (( + ) cc_0 y)) x) 1)
    let g x = (f (((fun cc_1 y -> cc_1) x) ()))
    |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
  let (x, y) = (0, 0)
  let g z = (fun _ -> x + y + z) ()
  |};
  [%expect
    {|
    let (x, y) = (0, 0)
    let g z = (((fun cc_0 _ -> (( + ) (( + ) x y) cc_0)) z) ())
    |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
  let (x, y) = (0, 0)
  and g x y z = (fun _ -> x + y + z) ()
  |};
  [%expect
    {|
    let (x, y) = (0, 0) and g x y z = (((fun cc_2 cc_1 cc_0 _ -> (( + ) (( + ) cc_0 cc_1) cc_2)) z y x) ())
    |}]
;;

(* ---------------- *)

(* Recursive declarations *)

let%expect_test _ =
  closure_conversion_program {| let rec f x = f x |};
  [%expect {| let rec f x = (f x) |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
    let rec fac x = if x = 1 then x else x * fac (x - 1)
  |};
  [%expect
    {|
    let rec fac x = (if (( = ) x 1) then x else (( * ) x (fac (( - ) x 1))))
    |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
    let rec fac x = (fun _ -> if x = 1 then x else x * fac (x - 1)) ()
  |};
  [%expect
    {|
      let rec fac x = (((fun cc_0 _ -> (if (( = ) cc_0 1) then cc_0 else (( * ) cc_0 (fac (( - ) cc_0 1))))) x) ())
    |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
    let rec f x = g x
    and g y = (fun _ -> f y) ()
  |};
  [%expect
    {|
      let rec f x = (g x) and g y = (((fun cc_0 _ -> (f cc_0)) y) ())
    |}]
;;

let%expect_test _ =
  closure_conversion_program
    {| 
    let rec f x = (fun _ -> g x) ()
    and g y = (fun _ -> f y) ()
  |};
  [%expect
    {|
      let rec f x = (((fun cc_0 _ -> (g cc_0)) x) ()) and g y = (((fun cc_1 _ -> (f cc_1)) y) ())
    |}]
;;

(* ---------------- *)
