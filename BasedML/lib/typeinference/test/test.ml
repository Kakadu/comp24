open Typeinference
open Typeinference__StartState

let%expect_test _ =
  test_infer_exp "fun ((x, y): (int*bool)) -> y";
  [%expect
    {|
    res: (TFunction ((TTuple [TInt; TBool]), TBool))
     substs: [("_p1", TBool); ("_p0", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp "fun ((x::y): (int list)) -> y";
  [%expect
    {|
    res: (TFunction ((TList TInt), (TList TInt)))
     substs: [("_p0", TInt); ("_p1", (TList TInt))] |}]
;;

let%expect_test _ =
  test_infer_exp
    {|fun (tuper_var: int) -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect {|
    Infer error: Can not unify `TInt` and `(TList (TPoly "_p2"))` |}]
;;

let%expect_test _ =
  test_infer_exp
    {|fun tuper_var -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect
    {|
    Infer error: The type variable _p4 occurs inside (TList (TPoly "_p4")) |}]
;;

let%expect_test _ =
  test_infer_exp {|fun f list -> match nolist with
  | [] -> list
  | h :: tl -> h|};
  [%expect {| Infer error: Unbound value nolist |}]
;;

let%expect_test _ =
  test_infer_exp {|(fun f x -> f)(fun f x -> f)|};
  [%expect
    {|
    res: (TFunction ((TPoly "_p2"),
       (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3")))))))
     substs: [("_p0",
      (TFunction ((TPoly "_p2"),
         (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3")))))
         )));
      ("_p1",
       (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3"))))))
      ] |}]
;;

let%expect_test _ =
  test_infer_exp {|let x = 1 in x|};
  [%expect {|
    res: TInt
     substs: [("_p0", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp {|let id = fun x -> x in id|};
  [%expect
    {|
    res: (TFunction ((TPoly "_p2"), (TPoly "_p2")))
     substs: [("_p1", (TFunction ((TPoly "_p0"), (TPoly "_p0"))))] |}]
;;

let%expect_test _ =
  test_infer_exp
    {|let rec fiboCPS = fun n acc -> match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS n (fun x -> fiboCPS n (fun y -> acc x))
      in fiboCPS 2 (fun x -> x)|};
  [%expect
    {|
    res: TInt
     substs: [("_p11", TInt); ("_p13", TInt); ("_p14", TInt);
      ("_p12", (TFunction ((TFunction (TInt, TInt)), TInt))); ("_p9", TInt);
      ("_pd", (TPoly "_pa")); ("_p10", (TPoly "_pa")); ("_pf", TInt);
      ("_pe", (TFunction ((TFunction (TInt, (TPoly "_pa"))), (TPoly "_pa"))));
      ("_p8", (TPoly "_pa")); ("_pc", TInt);
      ("_pb", (TFunction ((TFunction (TInt, (TPoly "_pa"))), (TPoly "_pa"))));
      ("_p0",
       (TFunction (TInt,
          (TFunction ((TFunction (TInt, (TPoly "_pa"))), (TPoly "_pa"))))));
      ("_p6", (TPoly "_pa")); ("_p7", TInt); ("_p3", (TPoly "_pa"));
      ("_p4", TInt); ("_p2", (TFunction (TInt, (TPoly "_pa")))); ("_p5", TInt);
      ("_p1", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp {|let id = fun x -> x in ((id 1), (id true))|};
  [%expect
    {|
    res: (TTuple [TInt; TBool])
     substs: [("_p4", TBool); ("_p5", TBool); ("_p2", TInt); ("_p3", TInt);
      ("_p1", (TFunction ((TPoly "_p0"), (TPoly "_p0"))))] |}]
;;

(* Declarations *)

let%expect_test _ =
  test_infer_prog empty_state {|let x = 1;;
    let y = 2;;|};
  [%expect {|
    [""x"": TInt,
     ""y"": TInt,
     ] |}]
;;

let%expect_test _ =
  test_infer_prog empty_state {|let id = fun x-> x;;|};
  [%expect {|
    [""id"": (TFunction ((TPoly "_p2"), (TPoly "_p2"))),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog empty_state {|let id = fun x-> x;;
    let (x, y) = (id true, id 2);;|};
  [%expect
    {|
    [""id"": (TFunction ((TPoly "_p9"), (TPoly "_p9"))),
     ""x"": TBool,
     ""y"": TInt,
     ] |}]
;;

let%expect_test _ =
  test_infer_prog empty_state {|let rec f = fun x -> f;;|};
  [%expect
    {|
    Infer error: The type variable _p0 occurs inside (TFunction ((TPoly "_p1"), (TPoly "_p0"))) |}]
;;

let%expect_test _ =
  test_infer_prog
    empty_state
    {|let rec id = fun x -> x and dup = fun x y -> (id x, id y);;|};
  [%expect
    {|
    [""dup"": (TFunction ((TPoly "_p7"),
                 (TFunction ((TPoly "_p7"),
                    (TTuple [(TPoly "_p7"); (TPoly "_p7")])))
                 )),
     ""id"": (TFunction ((TPoly "_p8"), (TPoly "_p8"))),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    empty_state
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)));;
  let (a, b) = ((x (1, 2)), (x (true, false)));;|};
  [%expect
    {|
    [""a"": (TTuple [TInt; TInt]),
     ""b"": (TTuple [TBool; TBool]),
     ""x"": (TFunction ((TTuple [(TPoly "_pd"); (TPoly "_pd")]),
               (TTuple [(TPoly "_pd"); (TPoly "_pd")]))),
     ""y"": (TFunction ((TTuple [(TPoly "_pe"); (TPoly "_pe")]),
               (TTuple [(TPoly "_pe"); (TPoly "_pe")]))),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    empty_state
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)));;
  let (a, b) = ((x 1), (y (true, false)));;|};
  [%expect
    {|
    Infer error: Can not unify `TInt` and `(TTuple [(TPoly "_p7"); (TPoly "_p7")])` |}]
;;

let%expect_test _ =
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
  [%expect
    {|
    [""even"": (TFunction (TInt, TBool)),
     ""odd"": (TFunction (TInt, TBool)),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    empty_state
    {|
let (-) = fun (a:int) (b:int)->  a;;

let rec even = fun n -> match n with
    | 0 -> true
    | x -> odd (x - 1)
and odd = fun n -> match n with
    | 0 -> false
    | x -> even (x - 1)
|};
  [%expect
    {|
    [""( - )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""even"": (TFunction (TInt, TBool)),
     ""odd"": (TFunction (TInt, TBool)),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    empty_state
    {|
    let (-) = fun (a:int) (b:int)->  a;;
    let (+) = fun (a:int) (b:int)->  a;;


  let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
      in fiboCPS n (fun x -> x)
  |};
  [%expect
    {|
    [""( + )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""( - )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""fibo"": (TFunction (TInt, TInt)),
     ] |}]
;;

let%expect_test _ =
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
    [""( * )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""( + )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""( - )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""( / )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
     ""( :: )"": (TFunction ((TPoly "_p1d"),
                    (TFunction ((TList (TPoly "_p1d")), (TList (TPoly "_p1d"))))
                    )),
     ""( < )"": (TFunction ((TPoly "_p1e"), (TFunction ((TPoly "_p1e"), TBool)))),
     ""( <= )"": (TFunction ((TPoly "_p1f"), (TFunction ((TPoly "_p1f"), TBool))
                    )),
     ""( <> )"": (TFunction ((TPoly "_p20"), (TFunction ((TPoly "_p20"), TBool))
                    )),
     ""( = )"": (TFunction ((TPoly "_p21"), (TFunction ((TPoly "_p21"), TBool)))),
     ""( > )"": (TFunction ((TPoly "_p22"), (TFunction ((TPoly "_p22"), TBool)))),
     ""( >= )"": (TFunction ((TPoly "_p23"), (TFunction ((TPoly "_p23"), TBool))
                    )),
     ""fibo"": (TFunction (TInt, TInt)),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    start_state
    {|
  let rec map = fun f lst-> match lst with
  | [] -> []
  | (x :: xs) -> (f x) :: (map f xs)
  ;;
  let mulTwo = (fun i -> (i * 2));;
  let doubleList = fun lst -> map mulTwo lst;;|};
  [%expect
    {|
     [""( * )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( + )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( - )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( / )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( :: )"": (TFunction ((TPoly "_p1b"),
                     (TFunction ((TList (TPoly "_p1b")), (TList (TPoly "_p1b"))))
                     )),
      ""( < )"": (TFunction ((TPoly "_p1c"), (TFunction ((TPoly "_p1c"), TBool)))),
      ""( <= )"": (TFunction ((TPoly "_p1d"), (TFunction ((TPoly "_p1d"), TBool))
                     )),
      ""( <> )"": (TFunction ((TPoly "_p1e"), (TFunction ((TPoly "_p1e"), TBool))
                     )),
      ""( = )"": (TFunction ((TPoly "_p1f"), (TFunction ((TPoly "_p1f"), TBool)))),
      ""( > )"": (TFunction ((TPoly "_p20"), (TFunction ((TPoly "_p20"), TBool)))),
      ""( >= )"": (TFunction ((TPoly "_p21"), (TFunction ((TPoly "_p21"), TBool))
                     )),
      ""doubleList"": (TFunction ((TList TInt), (TList TInt))),
      ""map"": (TFunction ((TFunction ((TPoly "_p22"), (TPoly "_p23"))),
                  (TFunction ((TList (TPoly "_p22")), (TList (TPoly "_p23")))))),
      ""mulTwo"": (TFunction (TInt, TInt)),
      ] |}]
;;

let%expect_test _ =
  test_infer_prog start_state {|
  let (a, b) = ((true < false), (3 < 4));;|};
  [%expect
    {|
     [""( * )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( + )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( - )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( / )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
      ""( :: )"": (TFunction ((TPoly "_p9"),
                     (TFunction ((TList (TPoly "_p9")), (TList (TPoly "_p9")))))),
      ""( < )"": (TFunction ((TPoly "_pa"), (TFunction ((TPoly "_pa"), TBool)))),
      ""( <= )"": (TFunction ((TPoly "_pb"), (TFunction ((TPoly "_pb"), TBool)))),
      ""( <> )"": (TFunction ((TPoly "_pc"), (TFunction ((TPoly "_pc"), TBool)))),
      ""( = )"": (TFunction ((TPoly "_pd"), (TFunction ((TPoly "_pd"), TBool)))),
      ""( > )"": (TFunction ((TPoly "_pe"), (TFunction ((TPoly "_pe"), TBool)))),
      ""( >= )"": (TFunction ((TPoly "_pf"), (TFunction ((TPoly "_pf"), TBool)))),
      ""a"": TBool,
      ""b"": TBool,
      ] |}]
;;

let%expect_test _ =
  test_infer_prog
    start_state
    {|
  let ( < ) = fun a b ->  (b = 2);;
  let (a, b) = ((true < false), (3 < 4));;|};
  [%expect {|
     Infer error: Can not unify `TInt` and `TBool` |}]
;;
