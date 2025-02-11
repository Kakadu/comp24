MANYTESTS
  $ ./run_infer.exe < manytests/do_not_type/001.ml
  Type inference error: Undefined variable "fac"

  $ ./run_infer.exe < manytests/do_not_type/002if.ml
  Type inference error: Unification failed on int and bool

  $ ./run_infer.exe < manytests/do_not_type/003occurs.ml
  Type inference error: Occurs check failed

  $ ./run_infer.exe < manytests/do_not_type/004let_poly.ml
  Type inference error: Unification failed on int and bool

  $ ./run_infer.exe < manytests/do_not_type/015tuples.ml
  Type inference error: Only variables are allowed as left-hand side of 'let rec'

  $ ./run_infer.exe < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ./run_infer.exe < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ./run_infer.exe < manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int

  $ ./run_infer.exe < manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a

  $ ./run_infer.exe < manytests/typed/005fix.ml
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int

  $ ./run_infer.exe < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int
 
  $ ./run_infer.exe < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ./run_infer.exe < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ./run_infer.exe < manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ./run_infer.exe < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ./run_infer.exe < manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ./run_infer.exe < manytests/typed/015tuples.ml
  val feven : 'a * (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val fodd : (int -> int) * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int) * (int -> int)

  $ ./run_infer.exe < manytests/typed/016lists.ml
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list

OTHER TESTS
  $ ./run_infer.exe << EOF 
  > let tuple_fun = fun (a,b,c,d) -> a + d
  > let tuple_fun2 (a,b,(2::t),d) = a + d
  > let rec f (a, b) = if a + b < 10 then a + b else f (a-1,b-1)
  > let unit1 = ()
  val f : int * int -> int
  val tuple_fun : int * 'a * 'b * int -> int
  val tuple_fun2 : int * 'a * int list * int -> int
  val unit1 : unit

  $ ./run_infer.exe << EOF 
  > let (a, ((b : 'a), c)) = (1, (true, 2))
  val a : int
  val b : bool
  val c : int

  $ ./run_infer.exe << EOF 
  > let ( * ) = ( && )
  > let x = true * false
  > let inc x = (+) 1 x
  val * : bool -> bool -> bool
  val inc : int -> int
  val x : bool

  $ ./run_infer.exe << EOF
  > let rec fac (n: int): int = if n <= 1 then 1 else n * fac (n - 1)
  > let id (x: int) = x
  > let inc : int -> int = fun x -> (+) x 1
  > let f1 a (b: int) (c: 'a): 'b = if c then inc a else b
  > let f2 (x, y) = if x = y then (+) else (-)
  val f1 : int -> int -> bool -> int
  val f2 : 'a * 'a -> int -> int -> int
  val fac : int -> int
  val id : int -> int
  val inc : int -> int

  $ ./run_infer.exe << EOF
  > let rec is_even n =
  > if n = 0 then true
  > else is_odd (n - 1)
  > 
  > and is_odd n =
  > if n = 0 then false
  > else is_even (n - 1)
  > 
  > and all_even lst =
  > match lst with
  > | [] -> true
  > | hd :: tl -> is_even hd && all_even tl
  val all_even : int list -> bool
  val is_even : int -> bool
  val is_odd : int -> bool

  $ ./run_infer.exe << EOF
  > let rec first a b = second a
  > and second a = first a a
  val first : 'a -> 'a -> 'b
  val second : 'a -> 'b
  $ ./run_infer.exe << EOF
  > let return_bool a b : bool = a + b
  Type inference error: Unification failed on int and bool

  $ ./run_infer.exe << EOF
  > let (a,b) = 1
  Type inference error: Unification failed on int and 'a * 'b

  $ ./run_infer.exe << EOF
  > let f (a: bool) c = a + c
  Type inference error: Unification failed on int and bool

  $ ./run_infer.exe << EOF
  > let t : int = ()
  Type inference error: Unification failed on unit and int

 
