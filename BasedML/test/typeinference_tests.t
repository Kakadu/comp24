  $ dune exec typeinference_demo << EOF
  > let (-) = fun (a:int) (b:int)->  a
  > let (+) = fun (a:int) (b:int)->  a
  > 
  > let fibo = fun n ->
  >  let rec fiboCPS = fun n acc -> match n with
  >  | 0 -> acc 0
  >  | 1 -> acc 1
  >  | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  >    in fiboCPS n (fun x -> x)
  > 
  > let __ = 5
  > let __ = 5
  > let ___ = 1
  > let ___ = []
  > let six = __ + 1
  > EOF
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('p2f -> (('p2f list) -> ('p2f list))),
   ""( < )"": ('p30 -> ('p30 -> bool)),
   ""( <= )"": ('p31 -> ('p31 -> bool)),
   ""( <> )"": ('p32 -> ('p32 -> bool)),
   ""( = )"": ('p33 -> ('p33 -> bool)),
   ""( > )"": ('p34 -> ('p34 -> bool)),
   ""( >= )"": ('p35 -> ('p35 -> bool)),
   ""__"": int,
   ""___"": ('p36 list),
   ""fibo"": (int -> int),
   ""six"": int,
   ]

  $ ./typeinference_demo.exe < manytests/do_not_type/001.ml
  Infer error: Unbound value: fac

  $ ./typeinference_demo.exe < manytests/do_not_type/002if.ml
  Infer error: Can not unify `TInt` and `TBool`

  $ ./typeinference_demo.exe < manytests/do_not_type/003occurs.ml
  Infer error: The type variable _p2 occurs inside ('_p2 -> '_p6)

  $ ./typeinference_demo.exe < manytests/do_not_type/004let_poly.ml
  Infer error: Can not unify `TInt` and `TBool`

  $ ./typeinference_demo.exe < manytests/do_not_type/015tuples.ml
  Infer error:  Only variables are allowed as left-hand side of `let rec'

  $ ./typeinference_demo.exe < manytests/do_not_type/099.ml
  Infer error:  Only variables are allowed as left-hand side of `let rec'
