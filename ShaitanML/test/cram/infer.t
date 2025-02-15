  $ infer < manytests/do_not_type/001.ml
  Infer error: Unbound variable 'fac'

  $ infer < manytests/do_not_type/002if.ml
  Infer error: Unification failed on int and bool

  $ infer < manytests/do_not_type/003occurs.ml
  Infer error: Occurs check failed: 2 occurs inside '2 -> '6

  $ infer < manytests/do_not_type/004let_poly.ml
  Infer error: Unification failed on bool and int

  $ infer < manytests/do_not_type/015tuples.ml
  Infer error: Left-hand side of let rec should be a variable

  $ infer < manytests/do_not_type/015tuples.ml
  Infer error: Left-hand side of let rec should be a variable

  $ infer < manytests/do_not_type/015tuples.ml
  Infer error: Left-hand side of let rec should be a variable

  $ infer < manytests/do_not_type/016lists.ml
  cannot open manytests/do_not_type/016lists.ml: No such file
  [2]

  $ infer < manytests/typed/001fac.ml
  val fac: int -> int
  val main: int

  $ infer < manytests/typed/002fac.ml
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int

  $ infer < manytests/typed/003fib.ml
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int

  $ infer < manytests/typed/004manyargs.ml
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a

  $ infer < manytests/typed/005fix.ml
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int

  $ infer < manytests/typed/006partial.ml
  val foo: int -> int
  val main: int

  $ infer < manytests/typed/006partial2.ml
  val foo: int -> int -> int -> int
  val main: int

  $ infer < manytests/typed/006partial3.ml
  val foo: int -> int -> int -> unit
  val main: int

  $ infer < manytests/typed/007order.ml
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: int -> unit

  $ infer < manytests/typed/008ascription.ml
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int

  $ infer < manytests/typed/009let_poly.ml
  val temp: int * bool

  $ infer < manytests/typed/015tuples.ml
  Infer error: Unbound variable '( == )'

  $ infer < manytests/typed/016lists.ml
  Infer error: Occurs check failed: 41 occurs inside '41

  $ dune exec infer << EOF
  > let f ((x : int) : int) ((y : int) : int) = x + y;;
  > EOF
  val f: int -> int -> int

  $ dune exec infer << EOF
  > let a::b = [1; 2; 3];;
  > EOF
  val a: int
  val b: int list

  $ dune exec infer << EOF
  > let f ((x : int):int) (((y : int):string):int) = x;;
  > EOF
  Infer error: Unification failed on int and string

  $ dune exec infer << EOF
  > let map_cps f l =
  >   let rec helper k xs =
  >     match xs with
  >     | [] -> k []
  >     | h :: tl -> helper (fun r -> k ((f h) :: r)) tl
  >   in
  >   helper (fun x -> x) l
  > ;;
  > EOF
  val map_cps: ('a -> 'b) -> 'a list -> 'b list
  $ dune exec infer << EOF
  > let f a b c d e = a b c d e;;
  > EOF
  val f: ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e


  $ dune exec infer << EOF
  > let (f : int -> int) = fun x -> x;;
  > EOF
  val f: int -> int

  $ dune exec infer << EOF
  > let x = let rec (f : int -> int) = fun x -> x in f 3;;
  > EOF
  val x: int

  $ dune exec infer << EOF
  > let f = let ((x : int):int) = 5 in x;;
  > EOF
  val f: int

  $ dune exec infer << EOF
  > let (x:int) = 5;;
  > EOF
  val x: int

  $ dune exec infer << EOF
  > let rec fix f x = f (fix f) x ;;
  > EOF
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

  $ dune exec infer << EOF
  > let rec fold_left f acc l =
  >    match l with
  >    | [] -> acc
  >    | h :: tl -> fold_left f (f acc h) tl
  > ;;
  > EOF
  val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  $ dune exec infer << EOF
  > let f x y = (x + y, [x; y])
  > EOF
  val f: int -> int -> int * int list

  $ dune exec infer << EOF
  > let f x = x + y;;
  > let y = 3;;
  > EOF
  Infer error: Unbound variable 'y'
