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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int -> int -> int

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val a : int
  val b : int list

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val map_cps : ('7 -> '9) -> '7 list -> '9 list

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5


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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int -> int

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val fold_left : ('4 -> '5 -> '4) -> '4 -> '5 list -> '4

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
  val > : '1 -> '1 -> bool
  val >= : '1 -> '1 -> bool
  val f : int -> int -> int * int list

  $ dune exec infer << EOF
  > let f x = x + y;;
  > let y = 3;;
  > EOF
  Infer error: Unbound variable 'y'
