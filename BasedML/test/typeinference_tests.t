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
  [""( * )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
   ""( + )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
   ""( - )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
   ""( / )"": (TFunction (TInt, (TFunction (TInt, TInt)))),
   ""( :: )"": (TFunction ((TPoly "_p27"),
                  (TFunction ((TList (TPoly "_p27")), (TList (TPoly "_p27"))))
                  )),
   ""( < )"": (TFunction ((TPoly "_p28"), (TFunction ((TPoly "_p28"), TBool)))),
   ""( <= )"": (TFunction ((TPoly "_p29"), (TFunction ((TPoly "_p29"), TBool))
                  )),
   ""( <> )"": (TFunction ((TPoly "_p2a"), (TFunction ((TPoly "_p2a"), TBool))
                  )),
   ""( = )"": (TFunction ((TPoly "_p2b"), (TFunction ((TPoly "_p2b"), TBool)))),
   ""( > )"": (TFunction ((TPoly "_p2c"), (TFunction ((TPoly "_p2c"), TBool)))),
   ""( >= )"": (TFunction ((TPoly "_p2d"), (TFunction ((TPoly "_p2d"), TBool))
                  )),
   ""fibo"": (TFunction (TInt, TInt)),
   ]

