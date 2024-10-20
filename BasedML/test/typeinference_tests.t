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
   ""( :: )"": (TFunction ((TPoly "_p1f"),
                  (TFunction ((TList (TPoly "_p1f")), (TList (TPoly "_p1f"))))
                  )),
   ""( < )"": (TFunction ((TPoly "_p20"), (TFunction ((TPoly "_p20"), TBool)))),
   ""( <= )"": (TFunction ((TPoly "_p21"), (TFunction ((TPoly "_p21"), TBool))
                  )),
   ""( <> )"": (TFunction ((TPoly "_p22"), (TFunction ((TPoly "_p22"), TBool))
                  )),
   ""( = )"": (TFunction ((TPoly "_p23"), (TFunction ((TPoly "_p23"), TBool)))),
   ""( > )"": (TFunction ((TPoly "_p24"), (TFunction ((TPoly "_p24"), TBool)))),
   ""( >= )"": (TFunction ((TPoly "_p25"), (TFunction ((TPoly "_p25"), TBool))
                  )),
   ""fibo"": (TFunction (TInt, TInt)),
   ]

