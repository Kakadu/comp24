(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Typeinference
open Typeinference__StartState

let%expect_test "" =
  test_infer_exp "fun ((x, y): (int*bool)) -> y";
  [%expect
    {|
    res: (TFunction ((TTuple [TInt; TBool]), TBool))
     substs: [("p3", bool); ("p2", int); ("p1", (int * bool))] |}]
;;

let%expect_test "Test list type" =
  test_infer_exp "fun ((x::y): (int list)) -> y";
  [%expect
    {|
    res: (TFunction ((TList TInt), (TList TInt)))
     substs: [("p2", int); ("p3", (int list)); ("p1", (int list))] |}]
;;

let%expect_test "Test if then else" =
  test_infer_exp "fun (x, y) -> if x then x else y";
  [%expect
    {|
    res: (TFunction ((TTuple [TBool; TBool]), TBool))
     substs: [("p2", bool); ("p3", bool); ("p1", bool); ("p0", (bool * bool))] |}]
;;

let%expect_test "Test match (with error)" =
  test_infer_exp
    {|fun (tuper_var: int) -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect {|
    Infer error: Can not unify `TInt` and `(TList (TPoly "a"))` |}]
;;

let%expect_test "Test occurs check" =
  test_infer_exp
    {|fun tuper_var -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect {|
    Infer error: The type variable p5 occurs inside (TList (TPoly "p5")) |}]
;;

let%expect_test "Test unbound val" =
  test_infer_exp {|fun f list -> match nolist with
  | [] -> list
  | h :: tl -> h|};
  [%expect {| Infer error: Unbound value nolist |}]
;;

let%expect_test "Test some combinator" =
  test_infer_exp {|(fun f x -> f)(fun f x -> f)|};
  [%expect
    {|
    res: (TFunction ((TPoly "p2"),
       (TFunction ((TPoly "p3"), (TFunction ((TPoly "p4"), (TPoly "p3")))))))
     substs: [("p0", ('p2 -> ('p3 -> ('p4 -> 'p3)))); ("p1", ('p3 -> ('p4 -> 'p3)))] |}]
;;

let%expect_test "Test let in" =
  test_infer_exp {|let x = 1 in x|};
  [%expect {|
    res: TInt
     substs: [("p0", int)] |}]
;;

let%expect_test "Test id fun" =
  test_infer_exp {|let id = fun x -> x in id|};
  [%expect
    {|
    res: (TFunction ((TPoly "p2"), (TPoly "p2")))
     substs: [("p1", ('p0 -> 'p0))] |}]
;;

let%expect_test "Test pseudo fiboCPS" =
  test_infer_exp
    {|let rec fiboCPS = fun n acc -> match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS n (fun x -> fiboCPS n (fun y -> acc x))
      in fiboCPS 2 (fun x -> x)|};
  [%expect
    {|
    res: TInt
     substs: [("p11", int); ("p13", int); ("p14", int); ("p12", ((int -> int) -> int));
      ("p9", int); ("pd", 'pa); ("p10", 'pa); ("pf", int);
      ("pe", ((int -> 'pa) -> 'pa)); ("p8", 'pa); ("pc", int);
      ("pb", ((int -> 'pa) -> 'pa)); ("p0", (int -> ((int -> 'pa) -> 'pa)));
      ("p6", 'pa); ("p7", int); ("p3", 'pa); ("p4", int); ("p2", (int -> 'pa));
      ("p5", int); ("p1", int)] |}]
;;

let%expect_test "Test simplest generalise" =
  test_infer_exp {|let id = fun x -> x in ((id 1), (id true))|};
  [%expect
    {|
    res: (TTuple [TInt; TBool])
     substs: [("p4", bool); ("p5", bool); ("p2", int); ("p3", int); ("p1", ('p0 -> 'p0))] |}]
;;

(* Declarations *)

let%expect_test "Test simple declarations" =
  test_infer_prog empty_state {|let x = 1
    let y = 2|};
  [%expect {|
    [""x"": int,
     ""y"": int,
     ] |}]
;;

let%expect_test "Test function decl" =
  test_infer_prog empty_state {|let a = fun s -> ()|};
  [%expect {|
    [""a"": ('p2 -> unit),
     ] |}]
;;

let%expect_test "Test declaration with constraint" =
  test_infer_prog empty_state {|let (a: ('a -> unit)) = fun s -> s|};
  [%expect {|
    [""a"": (unit -> unit),
     ]
    |}]
;;

let%expect_test "Test id declaration" =
  test_infer_prog empty_state {|let id = fun x-> x|};
  [%expect {|
    [""id"": ('p2 -> 'p2),
     ] |}]
;;

let%expect_test "Test declaration with generalise" =
  test_infer_prog empty_state {|let id = fun x-> x
    let (x, y) = (id true, id 2)|};
  [%expect {|
    [""id"": ('p9 -> 'p9),
     ""x"": bool,
     ""y"": int,
     ] |}]
;;

let%expect_test "Test occurs check declaration" =
  test_infer_prog empty_state {|let rec f = fun x -> f|};
  [%expect
    {|
    Infer error: The type variable p0 occurs inside (TFunction ((TPoly "p1"), (TPoly "p0"))) |}]
;;

let%expect_test "Test generalise in one scope" =
  test_infer_prog
    empty_state
    {|let rec id = fun x -> x and dup = fun x y -> (id x, id y)|};
  [%expect
    {|
    [""dup"": ('p7 -> ('p7 -> ('p7 * 'p7))),
     ""id"": ('p8 -> 'p8),
     ] |}]
;;

let%expect_test "Test generalise scope 1" =
  test_infer_prog
    empty_state
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)))
  let (a, b) = ((x (1, 2)), (x (true, false)))|};
  [%expect
    {|
    [""a"": (int * int),
     ""b"": (bool * bool),
     ""x"": (('pf * 'pf) -> ('pf * 'pf)),
     ""y"": (('p10 * 'p10) -> ('p10 * 'p10)),
     ] |}]
;;

let%expect_test "Test generalise scope 2" =
  test_infer_prog
    empty_state
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)))
  let (a, b) = ((x 1), (y (true, false)))|};
  [%expect
    {|
    Infer error: Can not unify `TInt` and `(TTuple [(TPoly "p9"); (TPoly "p9")])` |}]
;;

let%expect_test "Test pseudo EvenOrOdd" =
  test_infer_prog
    empty_state
    {|
let rec even = fun n -> match n with
    | 0 -> true
    | x -> odd (x)
and odd = fun n -> match n with
    | 0 -> false
    | x -> even (x)
|};
  [%expect {|
    [""even"": (int -> bool),
     ""odd"": (int -> bool),
     ] |}]
;;

let%expect_test "Test pseudo EvenOrOdd (with minus decl)" =
  test_infer_prog
    empty_state
    {|
let (-) = fun (a:int) (b:int)->  a

let rec even = fun n -> match n with
    | 0 -> true
    | x -> odd (x - 1)
and odd = fun n -> match n with
    | 0 -> false
    | x -> even (x - 1)
|};
  [%expect
    {|
    [""( - )"": (int -> (int -> int)),
     ""even"": (int -> bool),
     ""odd"": (int -> bool),
     ] |}]
;;

let%expect_test "Test pseudo Fibo (with `+` and `-` decl)" =
  test_infer_prog
    empty_state
    {|
    let (-) = fun (a:int) (b:int)->  a
    let (+) = fun (a:int) (b:int)->  a


  let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
      in fiboCPS n (fun x -> x)
  |};
  [%expect
    {|
    [""( + )"": (int -> (int -> int)),
     ""( - )"": (int -> (int -> int)),
     ""fibo"": (int -> int),
     ] |}]
;;

let%expect_test "Test pseudo Fibo" =
  test_infer_prog
    start_state
    {|
  let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
      in fiboCPS n (fun x -> x)
  |};
  [%expect
    {|
    [""( * )"": (int -> (int -> int)),
     ""( + )"": (int -> (int -> int)),
     ""( - )"": (int -> (int -> int)),
     ""( / )"": (int -> (int -> int)),
     ""( :: )"": ('p1d -> (('p1d list) -> ('p1d list))),
     ""( < )"": ('p1e -> ('p1e -> bool)),
     ""( <= )"": ('p1f -> ('p1f -> bool)),
     ""( <> )"": ('p20 -> ('p20 -> bool)),
     ""( = )"": ('p21 -> ('p21 -> bool)),
     ""( > )"": ('p22 -> ('p22 -> bool)),
     ""( >= )"": ('p23 -> ('p23 -> bool)),
     ""fibo"": (int -> int),
     ] |}]
;;

let%expect_test "Test partial application" =
  test_infer_prog
    start_state
    {|
  let rec map = fun f lst-> match lst with
  | [] -> []
  | (x :: xs) -> (f x) :: (map f xs)
  
  let mulTwo = (fun i -> (i * 2))
  let doubleList = fun lst -> map mulTwo lst|};
  [%expect
    {|
     [""( * )"": (int -> (int -> int)),
      ""( + )"": (int -> (int -> int)),
      ""( - )"": (int -> (int -> int)),
      ""( / )"": (int -> (int -> int)),
      ""( :: )"": ('p1b -> (('p1b list) -> ('p1b list))),
      ""( < )"": ('p1c -> ('p1c -> bool)),
      ""( <= )"": ('p1d -> ('p1d -> bool)),
      ""( <> )"": ('p1e -> ('p1e -> bool)),
      ""( = )"": ('p1f -> ('p1f -> bool)),
      ""( > )"": ('p20 -> ('p20 -> bool)),
      ""( >= )"": ('p21 -> ('p21 -> bool)),
      ""doubleList"": ((int list) -> (int list)),
      ""map"": (('p22 -> 'p23) -> (('p22 list) -> ('p23 list))),
      ""mulTwo"": (int -> int),
      ] |}]
;;

let%expect_test "Test default binops" =
  test_infer_prog start_state {|
  let (a, b) = ((true < false), (3 < 4))|};
  [%expect
    {|
     [""( * )"": (int -> (int -> int)),
      ""( + )"": (int -> (int -> int)),
      ""( - )"": (int -> (int -> int)),
      ""( / )"": (int -> (int -> int)),
      ""( :: )"": ('p9 -> (('p9 list) -> ('p9 list))),
      ""( < )"": ('pa -> ('pa -> bool)),
      ""( <= )"": ('pb -> ('pb -> bool)),
      ""( <> )"": ('pc -> ('pc -> bool)),
      ""( = )"": ('pd -> ('pd -> bool)),
      ""( > )"": ('pe -> ('pe -> bool)),
      ""( >= )"": ('pf -> ('pf -> bool)),
      ""a"": bool,
      ""b"": bool,
      ] |}]
;;

let%expect_test "Test binops overriding" =
  test_infer_prog
    start_state
    {|
  let ( < ) = fun a b ->  (b = 2)
  let (a, b) = ((true < false), (3 < 4))|};
  [%expect {|
     Infer error: Can not unify `TInt` and `TBool` |}]
;;
