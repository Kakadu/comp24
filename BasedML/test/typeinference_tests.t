  $ dune exec typeinference_demo << EOF
  > let (-) = fun (a:int) (b:int)->  a;;
  > let (+) = fun (a:int) (b:int)->  a;;
  > 
  > let fibo = fun n ->
  >  let rec fiboCPS = fun n acc -> match n with
  >  | 0 -> acc 0
  >  | 1 -> acc 1
  >  | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  >    in fiboCPS n (fun x -> x)
  > ;;
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
   ""( :: )"": ('_p2f -> (('_p2f list) -> ('_p2f list))),
   ""( < )"": ('_p30 -> ('_p30 -> bool)),
   ""( <= )"": ('_p31 -> ('_p31 -> bool)),
   ""( <> )"": ('_p32 -> ('_p32 -> bool)),
   ""( = )"": ('_p33 -> ('_p33 -> bool)),
   ""( > )"": ('_p34 -> ('_p34 -> bool)),
   ""( >= )"": ('_p35 -> ('_p35 -> bool)),
   ""__"": int,
   ""___"": ('_p36 list),
   ""fibo"": (int -> int),
   ""six"": int,
   ]

