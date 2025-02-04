  $ cat << EOF | dune exec inferencer_runner -
  > let a = 5 * 9 / 7
  > EOF
  var a: int

  $ cat << EOF | dune exec inferencer_runner -
  > let a = [1; 2; 3451; 12]
  > EOF
  var a: int list

  $ cat << EOF | dune exec inferencer_runner -
  > let a = (true, 55 - 892, [(1, 2); (3, 5)])
  > EOF
  var a: bool * int * (int * int) list

  $ cat << EOF | dune exec inferencer_runner -
  > let a = [(true, 55 - 892, [(1, 2); (3, 5)])]
  > EOF
  var a: bool * int * (int * int) list list

  $ cat << EOF | dune exec inferencer_runner -
  > let rec fact x = if x = 0 then 1 else x * fact (x - 1)
  > EOF
  var fact: int -> int

  $ cat << EOF | dune exec inferencer_runner -
  > let a x = x :: [1; 2; 3]
  > EOF
  var a: int -> int list

  $ cat << EOF | dune exec inferencer_runner -
  > let (5::a) = [3; 4; 6]
  > EOF
  var a: int list

  $ cat << EOF | dune exec inferencer_runner -
  > let (a::b) = [3; 4; 6]
  > EOF
  var a: int
  var b: int list

  $ cat << EOF | dune exec inferencer_runner -
  > let tuple = (22, 23)
  > let (_, b) = tuple
  > EOF
  var b: int
  var tuple: int * int

  $ cat << EOF | dune exec inferencer_runner -
  > let pat_matching x = 
  >   match x with 
  >   | (52, 52) -> true
  >   | _ -> false
  > EOF
  var pat_matching: int * int -> bool

  $ cat << EOF | dune exec inferencer_runner -
  > let check_equal x y = 
  >   let sum a b = a + b in 
  >   let sub a b = a - b in 
  >   let rev x = -x in 
  >   if rev x = rev y then sub x y else sum x y
  > EOF
  var check_equal: int -> int -> int

  $ cat << EOF | dune exec inferencer_runner -
  > let a = (fun x y -> (x, y)) 1 2
  > EOF
  var a: int * int

  $ cat << EOF | dune exec inferencer_runner -
  > let list = (0, [(-1, [(-2, [], [])], [])], [])
  > EOF
  var list: int * (int * (int * 'a list * 'b list) list * 'c list) list * 'd list

  $ cat << EOF | dune exec inferencer_runner -
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
  var reverse: int list * bool list

  $ cat << EOF | dune exec inferencer_runner -
  > let f x = x
  > EOF
  var f: 'a -> 'a

  $ cat << EOF | dune exec inferencer_runner -
  > let (a, b) = (fun a x -> a x), (fun a x -> a x)
  > EOF
  var a: ('a -> 'b) -> 'a -> 'b
  var b: ('a -> 'b) -> 'a -> 'b

  $ cat << EOF | dune exec inferencer_runner -
  > let f g a b = g a b
  > EOF
  var f: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

  $ cat << EOF | dune exec inferencer_runner -
  > let a x = let id x = x in let (a, b) = (id x, id x) in (a, b)
  > EOF
  var a: 'a -> 'a * 'a

  $ cat << EOF | dune exec inferencer_runner -
  > let a x = x x
  > EOF
  The type variable 'a occurs inside 'a -> 'b

  $ cat << EOF | dune exec inferencer_runner -
  > let dummy = 12 + true
  > EOF
  This expression has type bool but an expression was expected of type int

  $ cat << EOF | dune exec inferencer_runner -
  > let (x, x) = (1, 1)
  > EOF
  Variable x is bound several times

  $ cat << EOF | dune exec inferencer_runner -
  > let several_bounds x = let (y::y) = x in y
  > EOF
  Variable y is bound several times

  $ cat << EOF | dune exec inferencer_runner -
  > let nested_bounds x = let ((h::tl), h) = x in h
  > EOF
  Variable h is bound several times

  $ cat << EOF | dune exec inferencer_runner -
  > let rec (a, b) = 5
  > EOF
  Only variables are allowed as left-side of 'let rec'

  $ cat << EOF | dune exec inferencer_runner -
  > let increase = x + 1
  > EOF
  Unbound value 'x'

  $ cat << EOF | dune exec inferencer_runner -
  > let rec f x y = if x = y then f x else f y
  > EOF
  The type variable 'b occurs inside 'a -> 'b

  $ cat << EOF | dune exec inferencer_runner -
  > let a x = fun y -> y x
  > EOF
  var a: 'a -> ('a -> 'b) -> 'b

  $ cat << EOF | dune exec inferencer_runner -
  > let id (x : int) : int = x
  > let double = id 42
  > EOF
  var double: int
  var id: int -> int

  $ cat << EOF | dune exec inferencer_runner -
  > let apply_pair (f : int -> int) (x : int * int) : int * int =
  >   let (a, b) = x in
  >   (f a, f b)
  > let result = apply_pair (fun x -> x * 2) (3, 4)
  > EOF
  var apply_pair: (int -> int) -> int * int -> int * int
  var result: int * int

  $ cat << EOF | dune exec inferencer_runner -
  > let map_triple (f : int -> bool) ((x, y, z) : int * int * int) : bool * bool * bool = 
  >   (f x, f y, f z)
  > let result = map_triple (fun x -> x > 0) (1, -2, 3)
  > EOF
  var map_triple: (int -> bool) -> int * int * int -> bool * bool * bool
  var result: bool * bool * bool

  $ cat << EOF | dune exec inferencer_runner -
  > let rec sum_list (lst : int list) : int =
  >   match lst with
  >   | [] -> 0
  >   | h :: t -> h + sum_list t
  > let result = sum_list [1;2;3;4]
  > EOF
  var result: int
  var sum_list: int list -> int

  $ cat << EOF | dune exec inferencer_runner -
  > let ( + ) a b = a && b
  > EOF
  var result: int
  var sum_list: int list -> int
