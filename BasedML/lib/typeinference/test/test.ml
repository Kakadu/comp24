(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Typeinference
open Typeinference__StartState

let%expect_test "" =
  test_infer_exp "fun ((x, y): (int*bool)) -> y";
  [%expect
    {|
    res: (TFunction ((TTuple [TInt; TBool]), TBool))
     substs: [(_p3, bool); (_p2, int); (_p1, (int * bool))] |}]
;;

let%expect_test "Test list type" =
  test_infer_exp "fun ((x::y): (int list)) -> y";
  [%expect
    {|
    res: (TFunction ((TList TInt), (TList TInt)))
     substs: [(_p2, int); (_p3, (int list)); (_p1, (int list))] |}]
;;

let%expect_test "Test if then else" =
  test_infer_exp "fun (x, y) -> if x then x else y";
  [%expect
    {|
    res: (TFunction ((TTuple [TBool; TBool]), TBool))
     substs: [(_p2, bool); (_p3, bool); (_p1, bool); (_p0, (bool * bool))] |}]
;;

let%expect_test "Test match (with error)" =
  test_infer_exp
    {|fun (tuper_var: int) -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect {|
    Infer error: Can not unify `TInt` and `(TList (TPoly a))` |}]
;;

let%expect_test "Test occurs check" =
  test_infer_exp
    {|fun tuper_var -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect {|
    Infer error: The type variable a occurs inside ('a list) |}]
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
    res: (TFunction ((TPoly _p2),
       (TFunction ((TPoly _p3), (TFunction ((TPoly _p4), (TPoly _p3)))))))
     substs: [(_p0, ('_p2 -> ('_p3 -> ('_p4 -> '_p3)))); (_p1, ('_p3 -> ('_p4 -> '_p3)))] |}]
;;

let%expect_test "Test let in" =
  test_infer_exp {|let x = 1 in x|};
  [%expect {|
    res: TInt
     substs: [(_p0, int)] |}]
;;

let%expect_test "Test id fun" =
  test_infer_exp {|let id = fun x -> x in id|};
  [%expect
    {|
    res: (TFunction ((TPoly _p2), (TPoly _p2)))
     substs: [(_p1, ('_p0 -> '_p0))] |}]
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
     substs: [(_p11, int); (_p13, int); (_p14, int); (_p12, ((int -> int) -> int));
      (_p9, int); (_pd, '_pa); (_p10, '_pa); (_pf, int);
      (_pe, ((int -> '_pa) -> '_pa)); (_p8, '_pa); (_pc, int);
      (_pb, ((int -> '_pa) -> '_pa)); (_p0, (int -> ((int -> '_pa) -> '_pa)));
      (_p6, '_pa); (_p7, int); (_p3, '_pa); (_p4, int); (_p2, (int -> '_pa));
      (_p5, int); (_p1, int)] |}]
;;

let%expect_test "Test simplest generalise" =
  test_infer_exp {|let id = fun x -> x in ((id 1), (id true))|};
  [%expect
    {|
    res: (TTuple [TInt; TBool])
     substs: [(_p4, bool); (_p5, bool); (_p2, int); (_p3, int); (_p1, ('_p0 -> '_p0))] |}]
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
    [""a"": ('_p2 -> unit),
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
    [""id"": ('_p2 -> '_p2),
     ] |}]
;;

let%expect_test "Test declaration with generalise" =
  test_infer_prog empty_state {|let id = fun x-> x
    let (x, y) = (id true, id 2)|};
  [%expect {|
    [""id"": ('_p9 -> '_p9),
     ""x"": bool,
     ""y"": int,
     ] |}]
;;

let%expect_test "Test occurs check declaration" =
  test_infer_prog empty_state {|let rec f = fun x -> f|};
  [%expect
    {|
    Infer error: The type variable _p0 occurs inside ('_p1 -> '_p0) |}]
;;

let%expect_test "Test generalise in one scope" =
  test_infer_prog
    empty_state
    {|let rec id = fun x -> x and dup = fun x y -> (id x, id y)|};
  [%expect
    {|
    [""dup"": ('_p7 -> ('_p7 -> ('_p7 * '_p7))),
     ""id"": ('_p8 -> '_p8),
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
     ""x"": (('_pf * '_pf) -> ('_pf * '_pf)),
     ""y"": (('_p10 * '_p10) -> ('_p10 * '_p10)),
     ] |}]
;;

let%expect_test "Test generalise scope 2" =
  test_infer_prog
    empty_state
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)))
  let (a, b) = ((x 1), (y (true, false)))|};
  [%expect
    {|
    Infer error: Can not unify `TInt` and `(TTuple [(TPoly _p9); (TPoly _p9)])` |}]
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
     ""( :: )"": ('_p1d -> (('_p1d list) -> ('_p1d list))),
     ""( < )"": ('_p1e -> ('_p1e -> bool)),
     ""( <= )"": ('_p1f -> ('_p1f -> bool)),
     ""( <> )"": ('_p20 -> ('_p20 -> bool)),
     ""( = )"": ('_p21 -> ('_p21 -> bool)),
     ""( > )"": ('_p22 -> ('_p22 -> bool)),
     ""( >= )"": ('_p23 -> ('_p23 -> bool)),
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
      ""( :: )"": ('_p1b -> (('_p1b list) -> ('_p1b list))),
      ""( < )"": ('_p1c -> ('_p1c -> bool)),
      ""( <= )"": ('_p1d -> ('_p1d -> bool)),
      ""( <> )"": ('_p1e -> ('_p1e -> bool)),
      ""( = )"": ('_p1f -> ('_p1f -> bool)),
      ""( > )"": ('_p20 -> ('_p20 -> bool)),
      ""( >= )"": ('_p21 -> ('_p21 -> bool)),
      ""doubleList"": ((int list) -> (int list)),
      ""map"": (('_p22 -> '_p23) -> (('_p22 list) -> ('_p23 list))),
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
      ""( :: )"": ('_p9 -> (('_p9 list) -> ('_p9 list))),
      ""( < )"": ('_pa -> ('_pa -> bool)),
      ""( <= )"": ('_pb -> ('_pb -> bool)),
      ""( <> )"": ('_pc -> ('_pc -> bool)),
      ""( = )"": ('_pd -> ('_pd -> bool)),
      ""( > )"": ('_pe -> ('_pe -> bool)),
      ""( >= )"": ('_pf -> ('_pf -> bool)),
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
