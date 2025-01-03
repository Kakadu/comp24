open Kreml_lib.Parser
open Kreml_lib.Inferencer

  
let parse_program input =
  let w p = R.run (infer_program p) in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All program input with
  | Ok rest ->
    let r = w rest in 
    (match r with
    | Result.Ok (s, env) -> 
      let fmt = Stdlib.Format.std_formatter in 
      let() = TypeEnv.pp fmt env in
      let () = Format.fprintf fmt "\n" in
      Subst.pp fmt s
    | Error e -> pp_error (Stdlib.Format.std_formatter) e)
  | Error _ -> print_endline "Parser failed"


let%expect_test "fac" =
  let fac = 
  "let rec fac n = if n<=1 then 1 else n * fac (n-1)

   let main =
     let () = print_int (fac 4) in
     0" in
 parse_program fac;
  [%expect {|
    [ fac -> [ ]int -> int
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> int -> int
    , 1 -> int
    , 2 -> bool
    , 3 -> int -> bool
    , 4 -> int
    , 5 -> int -> int
    , 6 -> int
    , 7 -> int
    , 8 -> int -> int
    , 9 -> unit
    , 10 -> int
    , 11 -> int
     ] |}]


let%expect_test "fac_cps" =
  let fac_cps = 
  "let rec fac_cps n k =
    if n=1 then k 1 else
    fac_cps (n-1) (fun p -> k (p*n))

  let main =
    let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
    0" in
 parse_program fac_cps;
  [%expect {|
    [ fac_cps -> [ 6; ]int -> (int -> 6) -> 6
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> int -> (int -> 6) -> 6
    , 1 -> int
    , 2 -> int -> 6
    , 3 -> bool
    , 4 -> int -> bool
    , 5 -> 6
    , 7 -> (int -> 6) -> 6
    , 8 -> int
    , 9 -> int -> int
    , 10 -> int
    , 11 -> 6
    , 12 -> int
    , 13 -> int -> int
    , 14 -> unit
    , 15 -> int
    , 16 -> (int -> int) -> int
    , 17 -> int
    , 18 -> int
    , 19 -> int
     ] |}]

let%expect_test "fib" =
  let fib = "
    let rec fib_acc a b n =
      if n=1 then b
      else
        let n1 = n-1 in
        let ab = a+b in
        fib_acc b ab n1

    let rec fib n =
      if n<2
      then n
      else fib (n - 1) + fib (n - 2) 

    let main =
      let () = print_int (fib_acc 0 1 4) in
      let () = print_int (fib 4) in
      0" in
  parse_program fib;
  [%expect {|
    [ fib -> [ ]int -> int
    , fib_acc -> [ ]int -> int -> int -> int
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 15 -> int -> int
    , 16 -> int
    , 17 -> bool
    , 18 -> int -> bool
    , 19 -> int
    , 20 -> int -> int
    , 21 -> int
    , 22 -> int
    , 23 -> int -> int
    , 24 -> int
    , 25 -> int
    , 26 -> int -> int
    , 27 -> unit
    , 28 -> int
    , 29 -> int -> int
    , 30 -> int -> int -> int
    , 31 -> unit
    , 32 -> int
    , 33 -> int
     ] |}]

let%expect_test "many_args" =
  let many_args = "let wrap f = if 1 = 1 then f else f

  let test3 a b c =
    let a = print_int a in
    let b = print_int b in
    let c = print_int c in
    0

  let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

  let main =
    let rez =
        (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
          1000000000)
    in
    let () = print_int rez in
    let temp2 = wrap test3 1 10 100 in
    0" in
  parse_program many_args;
  [%expect {|
    [ main -> [ ]int
    , print_int -> [ ]int -> unit
    , test10 -> [ ]int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    , test3 -> [ ]int -> int -> int -> int
    , wrap -> [ 0; ]0 -> 0
     ]
    [ 1 -> bool
    , 2 -> int -> bool
    , 3 -> 0 -> 0
    , 4 -> int
    , 5 -> int
    , 6 -> int
    , 7 -> unit
    , 8 -> unit
    , 9 -> unit
    , 10 -> unit
    , 11 -> unit
    , 12 -> unit
    , 13 -> int -> int -> int -> int
    , 14 -> int
    , 15 -> int
    , 16 -> int
    , 17 -> int
    , 18 -> int
    , 19 -> int
    , 20 -> int
    , 21 -> int
    , 22 -> int
    , 23 -> int
    , 24 -> int
    , 25 -> int -> int
    , 26 -> int
    , 27 -> int -> int
    , 28 -> int
    , 29 -> int -> int
    , 30 -> int
    , 31 -> int -> int
    , 32 -> int
    , 33 -> int -> int
    , 34 -> int
    , 35 -> int -> int
    , 36 -> int
    , 37 -> int -> int
    , 38 -> int
    , 39 -> int -> int
    , 40 -> int
    , 41 -> int -> int
    , 42 -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    , 43 -> int
    , 44 -> int -> int
    , 45 -> int -> int -> int
    , 46 -> int -> int -> int -> int
    , 47 -> int -> int -> int -> int -> int
    , 48 -> int -> int -> int -> int -> int -> int
    , 49 -> int -> int -> int -> int -> int -> int -> int
    , 50 -> int -> int -> int -> int -> int -> int -> int -> int
    , 51 -> int -> int -> int -> int -> int -> int -> int -> int -> int
    , 52 -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    , 53 -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    , 54 -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    , 55 -> int
    , 56 -> unit
    , 57 -> int
    , 58 -> int -> int
    , 59 -> int -> int -> int
    , 60 -> int -> int -> int -> int
    , 61 -> int -> int -> int -> int
    , 62 -> int
    , 63 -> int
     ] |}]

let rec fix f x = f (fix f) x

let%expect_test "fix" =
  let fix = "
    let rec fix f x = f (fix f) x
    let fac self n = if n<=1 then 1 else n * self (n-1)

  let main =
    let () = print_int (fix fac 6) in
    0" in
    parse_program fix;
  [%expect {|
    [ fac -> [ ](int -> int) -> int -> int
    , fix -> [ 2; 3; ]((2 -> 3) -> 2 -> 3) -> 2 -> 3
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> ((2 -> 3) -> 2 -> 3) -> 2 -> 3
    , 1 -> (2 -> 3) -> 2 -> 3
    , 4 -> 2 -> 3
    , 5 -> 2 -> 3
    , 6 -> int -> int
    , 7 -> int
    , 8 -> bool
    , 9 -> int -> bool
    , 10 -> int
    , 11 -> int -> int
    , 12 -> int
    , 13 -> int
    , 14 -> int -> int
    , 15 -> (int -> int) -> int -> int
    , 16 -> unit
    , 17 -> int
    , 18 -> int -> int
    , 19 -> int
    , 20 -> int
    , 21 -> int
     ] |}]

let%expect_test "partial" =
    let partial = "
    let foo1 b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

    let foo x = foo1 true (foo1 false (foo1 true (foo1 false x)))
    let main =
      let () = print_int (foo 11) in
      0" in
      parse_program partial;
  [%expect {|
    [ foo -> [ ]int -> int
    , foo1 -> [ ]bool -> int -> int
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> bool
    , 1 -> int
    , 2 -> int
    , 3 -> int -> int
    , 4 -> int
    , 5 -> int
    , 6 -> int -> int
    , 7 -> bool -> int -> int
    , 8 -> int
    , 9 -> int
    , 10 -> int -> int
    , 11 -> int
    , 12 -> int -> int
    , 13 -> int
    , 14 -> int -> int
    , 15 -> int
    , 16 -> int -> int
    , 17 -> int -> int
    , 18 -> unit
    , 19 -> int
    , 20 -> int
     ] |}]

let%expect_test "partial2" =
  let partial2 = "let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + b * c

  let main =
    let foo = foo 1 in
    let foo = foo 2 in
    let foo = foo 3 in
    let () = print_int foo in
    0" in
  parse_program partial2;
  [%expect {|
    [ foo -> [ ]int -> int -> int -> int
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> int
    , 1 -> int
    , 2 -> int
    , 3 -> unit
    , 4 -> unit
    , 5 -> unit
    , 6 -> int
    , 7 -> int -> int
    , 8 -> int
    , 9 -> int -> int
    , 10 -> int -> int -> int -> int
    , 11 -> int -> int -> int
    , 12 -> int -> int -> int
    , 13 -> int -> int
    , 14 -> int -> int
    , 15 -> int
    , 16 -> int
    , 17 -> unit
    , 18 -> int
     ] |}]

let%expect_test "partial3" =
  let partial3 = "
  let foo a =
    let () = print_int a in fun b ->
    let () = print_int b in fun c ->
    print_int c

  let main =
    let () = foo 4 8 9 in
    0" in
    parse_program partial3;
  [%expect {|
    [ foo -> [ ]int -> int -> int -> unit
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> int
    , 1 -> unit
    , 2 -> int
    , 3 -> unit
    , 4 -> int
    , 5 -> unit
    , 6 -> int -> int -> int -> unit
    , 7 -> unit
    , 8 -> int -> unit
    , 9 -> int -> int -> unit
    , 10 -> int
     ] |}]
let somef f g x = f x ( g x : bool )



let%expect_test "ascription" =
  let ascription = "
    let addi = fun f g x -> (f x (g x: bool) : int)

    let main =
      let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
      0" in
  parse_program ascription;
  [%expect {|
    [ addi -> [ 2; ](2 -> bool -> int) -> (2 -> bool) -> 2 -> int
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> 2 -> bool -> int
    , 1 -> 2 -> bool
    , 3 -> int
    , 4 -> bool -> int
    , 5 -> bool
    , 6 -> (2 -> bool -> int) -> (2 -> bool) -> 2 -> int
    , 7 -> unit
    , 8 -> int
    , 9 -> int -> int
    , 10 -> (int -> bool) -> int -> int
    , 11 -> int
    , 12 -> int
    , 13 -> bool
    , 14 -> int
    , 15 -> int -> int
    , 16 -> int
    , 17 -> int -> int
    , 18 -> int
    , 19 -> bool
    , 20 -> int -> bool
    , 21 -> int
    , 22 -> int -> int
    , 23 -> int
     ] |}]

 let%expect_test "poly" =
    let poly = "
    let temp =
      let f = fun x -> x in
      (f 1, f true)" in
    parse_program poly;
   [%expect {|
     [ print_int -> [ ]int -> unit
     , temp -> [ ]int * bool
      ]
     [ 1 -> 0 -> 0
     , 2 -> int
     , 3 -> int
     , 4 -> bool
     , 5 -> bool
     , 6 -> int * bool
      ] |}]
let map f p = let (a, b) = p in (f a, f b)

let%expect_test "tuples" =
    let tuples = "let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l
  let feven p n =
    let (e, o) = p in
    if n = 0 then 1 else o (n - 1)
  let fodd p n =
    let (e, o) = p in
    if n = 0 then 0 else e (n - 1)
  let tie = fixpoly (feven, fodd)

  let rec meven n = if n = 0 then 1 else modd (n - 1)
  and modd n = if n = 0 then 1 else meven (n - 1)
  let main =
    let () = print_int (modd 1) in
    let () = print_int (meven 2) in
    let (even,odd) = tie in
    let () = print_int (odd 3) in
    let () = print_int (even 4) in
    0" in
  parse_program tuples;
  [%expect {|
    [ feven -> [ 32; ]32 * (int -> int) -> int -> int
    , fix -> [ 2; 3; ]((2 -> 3) -> 2 -> 3) -> 2 -> 3
    , fixpoly -> [ 25; 26; ]((25 -> 26) * (25 -> 26) -> 25 -> 26) * ((25 -> 26) * (25 -> 26) -> 25 -> 26) -> (25 -> 26) * (25 -> 26)
    , fodd -> [ 43; ](int -> int) * 43 -> int -> int
    , main -> [ ]int
    , map -> [ 9; 11; ](9 -> 11) -> 9 * 9 -> 11 * 11
    , meven -> [ ]int -> int
    , modd -> [ ]int -> int
    , print_int -> [ ]int -> unit
    , tie -> [ ](int -> int) * (int -> int)
     ]
    [ 56 -> int -> int
    , 57 -> int -> int
    , 58 -> int
    , 59 -> bool
    , 60 -> int -> bool
    , 61 -> int
    , 62 -> int
    , 63 -> int -> int
    , 64 -> int
    , 65 -> bool
    , 66 -> int -> bool
    , 67 -> int
    , 68 -> int
    , 69 -> int -> int
    , 70 -> unit
    , 71 -> int
    , 72 -> unit
    , 73 -> int
    , 74 -> int -> int
    , 75 -> int -> int
    , 76 -> unit
    , 77 -> int
    , 78 -> unit
    , 79 -> int
    , 80 -> int
     ] |}]

(* let%expect_test "lists" =
  let lists = "
  let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

  let rec map f xs =
    match xs with
    | [] -> []
    | a::[] -> [f a]
    | a::b::[] -> [f a; f b]
    | a::b::c::[] -> [f a; f b; f c]
    | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

  let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

  let concat =
    let rec helper xs =
      match xs with
      | [] -> []
      | h::tl -> append h (helper tl)
    in helper

  let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

  let rec cartesian xs ys =
    match xs with
    | [] -> []
    | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

  let main =
    let () = iter print_int [1;2;3] in
    let () = print_int (length_tail (cartesian [1;2] [1;2;3;4])) in
    0" in
  parse_program lists *)

  let%expect_test "lists" =
  let lists = "
  let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

  let concat =
    let rec helper xs =
      match xs with
      | [] -> []
      | h::tl -> append h (helper tl)
    in helper
" in
  parse_program lists;
    [%expect {|
      [ append -> [ 4; ]4 list -> 4 list -> 4 list
      , concat -> [ 18; ]18 list list -> 18 list
      , print_int -> [ ]int -> unit
       ]
      [ 0 -> 4 list -> 4 list -> 4 list
      , 1 -> 4 list
      , 2 -> 4 list
      , 3 -> 4 list
      , 5 -> 4
      , 6 -> 4 list
      , 7 -> 4 list
      , 8 -> 4 list -> 4 list
      , 9 -> 18 list list -> 18 list
      , 10 -> 18 list list
      , 11 -> 18 list
      , 12 -> 18 list
      , 13 -> 18
      , 14 -> 18 list
      , 15 -> 18 list list
      , 16 -> 18 list
      , 17 -> 18 list -> 18 list
      , 19 -> 18 list
      , 20 -> 18 list list -> 18 list
       ] |}]

 let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

  let concat =
    let rec helper xs =
      match xs with
      | [] -> []
      | h::tl -> append h (helper tl)
    in helper





