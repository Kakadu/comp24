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

