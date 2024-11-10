  $ dune exec typeinference_demo << EOF
  > let (-) = fun (a:int) (b:int)->  a;;
  > let (+) = fun (a:int) (b:int)->  a;;
  > 
  > let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
  >  | 0 -> acc 0
  >  | 1 -> acc 1
  >  | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  >    in fiboCPS n (fun x -> x)
  > EOF
  [""( * )"": (int -> (int -> int)),
   ""( + )"": (int -> (int -> int)),
   ""( - )"": (int -> (int -> int)),
   ""( / )"": (int -> (int -> int)),
   ""( :: )"": ('_p27 -> (('_p27 list) -> ('_p27 list))),
   ""( < )"": ('_p28 -> ('_p28 -> bool)),
   ""( <= )"": ('_p29 -> ('_p29 -> bool)),
   ""( <> )"": ('_p2a -> ('_p2a -> bool)),
   ""( = )"": ('_p2b -> ('_p2b -> bool)),
   ""( > )"": ('_p2c -> ('_p2c -> bool)),
   ""( >= )"": ('_p2d -> ('_p2d -> bool)),
   ""fibo"": (int -> int),
   ]

