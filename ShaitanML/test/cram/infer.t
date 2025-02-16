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

  $ infer < manytests/typed/001fac.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fac : int -> int
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/002fac.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fac_cps : int -> (int -> '7) -> '7
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/003fib.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/004manyargs.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val main : int
  val print_int : int -> unit
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : '0 -> '0

  $ infer < manytests/typed/005fix.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fac : (int -> int) -> int -> int
  val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/006partial.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val foo : int -> int
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/006partial2.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val foo : int -> int -> int -> int
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/006partial3.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val foo : int -> int -> int -> unit
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/007order.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : int -> unit
  val print_int : int -> unit

  $ infer < manytests/typed/008ascription.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val addi : ('2 -> bool -> int) -> ('2 -> bool) -> '2 -> int
  val main : int
  val print_int : int -> unit

  $ infer < manytests/typed/009let_poly.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val print_int : int -> unit
  val temp : int * bool

  $ infer < manytests/typed/015tuples.ml
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val feven : '32 * (int -> int) -> int -> int
  val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  val fixpoly : (('25 -> '26) * ('25 -> '26) -> '25 -> '26) * (('25 -> '26) * ('25 -> '26) -> '25 -> '26) -> ('25 -> '26) * ('25 -> '26)
  val fodd : (int -> int) * '44 -> int -> int
  val main : int
  val map : ('8 -> '10) -> '8 * '8 -> '10 * '10
  val meven : int -> int
  val modd : int -> int
  val print_int : int -> unit
  val tie : (int -> int) * (int -> int)

  $ infer < manytests/typed/016lists.ml
  Infer error: Occurs check failed: 41 occurs inside '41

  $ dune exec infer << EOF
  > let f ((x : int) : int) ((y : int) : int) = x + y;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int -> int -> int
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let a::b = [1; 2; 3];;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val a : int
  val b : int list
  val print_int : int -> unit

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
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val map_cps : ('6 -> '8) -> '6 list -> '8 list
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let f a b c d e = a b c d e;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5
  val print_int : int -> unit


  $ dune exec infer << EOF
  > let (f : int -> int) = fun x -> x;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int -> int
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let x = let rec (f : int -> int) = fun x -> x in f 3;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val print_int : int -> unit
  val x : int

  $ dune exec infer << EOF
  > let f = let ((x : int):int) = 5 in x;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let (x:int) = 5;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val print_int : int -> unit
  val x : int

  $ dune exec infer << EOF
  > let rec fix f x = f (fix f) x ;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let rec fold_left f acc l =
  >    match l with
  >    | [] -> acc
  >    | h :: tl -> fold_left f (f acc h) tl
  > ;;
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fold_left : ('4 -> '5 -> '4) -> '4 -> '5 list -> '4
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let f x y = (x + y, [x; y])
  > EOF
  val * : int -> int -> int
  val + : int -> int -> int
  val - : int -> int -> int
  val / : int -> int -> int
  val < : '1 -> '1 -> bool
  val <= : '1 -> '1 -> bool
  val <> : '1 -> '1 -> bool
  val = : '1 -> '1 -> bool
  val == : '1 -> '1 -> bool
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int -> int -> int * int list
  val print_int : int -> unit

  $ dune exec infer << EOF
  > let f x = x + y;;
  > let y = 3;;
  > EOF
  Infer error: Unbound variable 'y'
