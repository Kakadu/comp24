  $ dune exec closure_conv <<- EOF
  > let add x y = x + y
  > let main =
  >   let add5 = add 5 in
  >   let res = add5 10 in
  > 0