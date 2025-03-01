  $ cat << EOF | ./inferencer_runner.exe -
  > let a = 5 * 9 / 7
  > EOF
  val a: int

  $ cat << EOF | ./inferencer_runner.exe -
  > let a = [1; 2; 3451; 12]
  > EOF
  val a: int list

  $ cat << EOF | ./inferencer_runner.exe -
  > let a = (true, 55 - 892, [(1, 2); (3, 5)])
  > EOF
  val a: bool * int * (int * int) list

  $ cat << EOF | ./inferencer_runner.exe -
  > let a = [(true, 55 - 892, [(1, 2); (3, 5)])]
  > EOF
  val a: bool * int * (int * int) list list

  $ cat << EOF | ./inferencer_runner.exe -
  > let rec fact x = if x = 0 then 1 else x * fact (x - 1)
  > EOF
  val fact: int -> int

  $ cat << EOF | ./inferencer_runner.exe -
  > let a x = x :: [1; 2; 3]
  > EOF
  val a: int -> int list

  $ cat << EOF | ./inferencer_runner.exe -
  > let (5::a) = [3; 4; 6]
  > EOF
  val a: int list

  $ cat << EOF | ./inferencer_runner.exe -
  > let (a::b) = [3; 4; 6]
  > EOF
  val a: int
  val b: int list

  $ cat << EOF | ./inferencer_runner.exe -
  > let tuple = (22, 23)
  > let (_, b) = tuple
  > EOF
  val b: int
  val tuple: int * int

  $ cat << EOF | ./inferencer_runner.exe -
  > let pat_matching x = 
  >   match x with 
  >   | (52, 52) -> true
  >   | _ -> false
  > EOF
  val pat_matching: int * int -> bool

  $ cat << EOF | ./inferencer_runner.exe -
  > let check_equal x y = 
  >   let sum a b = a + b in 
  >   let sub a b = a - b in 
  >   let rev x = -x in 
  >   if rev x = rev y then sub x y else sum x y
  > EOF
  val check_equal: int -> int -> int

  $ cat << EOF | ./inferencer_runner.exe -
  > let a = (fun x y -> (x, y)) 1 2
  > EOF
  val a: int * int

  $ cat << EOF | ./inferencer_runner.exe -
  > let list = (0, [(-1, [(-2, [], [])], [])], [])
  > EOF
  val list: int * (int * (int * 'a list * 'b list) list * 'c list) list * 'd list

  $ cat << EOF | ./inferencer_runner.exe -
  > let reverse = 
  >   let rec helper acc list = 
  >     match list with 
  >       | [] -> acc
  >       | (h :: tl) -> helper (h :: acc) tl
  >   in
  >   let reverse_int = helper [] ([1; 2; 3; 4; 5]) in 
  >   let reverse_intt = helper [] ([true; false]) in
  >   (reverse_int, reverse_intt)
  > EOF
  val reverse: int list * bool list

  $ cat << EOF | ./inferencer_runner.exe -
  > let f x = x
  > EOF
  val f: 'a -> 'a

  $ cat << EOF | ./inferencer_runner.exe -
  > let (a, b) = (fun a x -> a x), (fun a x -> a x)
  > EOF
  val a: ('a -> 'b) -> 'a -> 'b
  val b: ('a -> 'b) -> 'a -> 'b

  $ cat << EOF | ./inferencer_runner.exe -
  > let f g a b = g a b
  > EOF
  val f: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

  $ cat << EOF | ./inferencer_runner.exe -
  > let a x = let id x = x in let (a, b) = (id x, id x) in (a, b)
  > EOF
  val a: 'a -> 'a * 'a

  $ cat << EOF | ./inferencer_runner.exe -
  > let a x = x x
  > EOF
  The type variable 'a occurs inside 'a -> 'b

  $ cat << EOF | ./inferencer_runner.exe -
  > let dummy = 12 + true
  > EOF
  This expression has type bool but an expression was expected of type int

  $ cat << EOF | ./inferencer_runner.exe -
  > let (x, x) = (1, 1)
  > EOF
  Variable x is bound several times

  $ cat << EOF | ./inferencer_runner.exe -
  > let several_bounds x = let (y::y) = x in y
  > EOF
  Variable y is bound several times

  $ cat << EOF | ./inferencer_runner.exe -
  > let nested_bounds x = let ((h::tl), h) = x in h
  > EOF
  Variable h is bound several times

  $ cat << EOF | ./inferencer_runner.exe -
  > let rec (a, b) = 5
  > EOF
  Only variables are allowed as left-side of 'let rec'

  $ cat << EOF | ./inferencer_runner.exe -
  > let increase = x + 1
  > EOF
  Unbound value 'x'

  $ cat << EOF | ./inferencer_runner.exe -
  > let rec f x y = if x = y then f x else f y
  > EOF
  The type variable 'b occurs inside 'a -> 'b

  $ cat << EOF | ./inferencer_runner.exe -
  > let a x = fun y -> y x
  > EOF
  val a: 'a -> ('a -> 'b) -> 'b

  $ cat << EOF | ./inferencer_runner.exe -
  > let id (x : int) : int = x
  > let double = id 42
  > EOF
  val double: int
  val id: int -> int

  $ cat << EOF | ./inferencer_runner.exe -
  > let apply_pair (f : int -> int) (x : int * int) : int * int =
  >   let (a, b) = x in
  >   (f a, f b)
  > let result = apply_pair (fun x -> x * 2) (3, 4)
  > EOF
  val apply_pair: (int -> int) -> int * int -> int * int
  val result: int * int

  $ cat << EOF | ./inferencer_runner.exe -
  > let map_triple (f : int -> bool) ((x, y, z) : int * int * int) : bool * bool * bool = 
  >   (f x, f y, f z)
  > let result = map_triple (fun x -> x > 0) (1, -2, 3)
  > EOF
  val map_triple: (int -> bool) -> int * int * int -> bool * bool * bool
  val result: bool * bool * bool

  $ cat << EOF | ./inferencer_runner.exe -
  > let rec sum_list (lst : int list) : int =
  >   match lst with
  >   | [] -> 0
  >   | h :: t -> h + sum_list t
  > let result = sum_list [1;2;3;4]
  > EOF
  val result: int
  val sum_list: int list -> int

  $ cat << EOF | ./inferencer_runner.exe -
  > let ( + ) a b = a && b
  > EOF
  val +: bool -> bool -> bool

  $ cat << EOF | ./inferencer_runner.exe -
  > let pack a b c d e f g h i j k l m n o p q r s t u v w x y z
  >  aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as1 at au av aw ax ay az
  >  ba bb =
  > (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
  > aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as1, at, au, av, aw, ax, ay, az,
  > ba, bb)
  > EOF
  val pack: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> 'a1 -> 'b1 -> 'c1 -> 'd1 -> 'e1 -> 'f1 -> 'g1 -> 'h1 -> 'i1 -> 'j1 -> 'k1 -> 'l1 -> 'm1 -> 'n1 -> 'o1 -> 'p1 -> 'q1 -> 'r1 -> 's1 -> 't1 -> 'u1 -> 'v1 -> 'w1 -> 'x1 -> 'y1 -> 'z1 -> 'a2 -> 'b2 -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o * 'p * 'q * 'r * 's * 't * 'u * 'v * 'w * 'x * 'y * 'z * 'a1 * 'b1 * 'c1 * 'd1 * 'e1 * 'f1 * 'g1 * 'h1 * 'i1 * 'j1 * 'k1 * 'l1 * 'm1 * 'n1 * 'o1 * 'p1 * 'q1 * 'r1 * 's1 * 't1 * 'u1 * 'v1 * 'w1 * 'x1 * 'y1 * 'z1 * 'a2 * 'b2
