  $ dune exec llvm_codegen <<- EOF
  > let rec fac n = if n < 1 then 1 else n * fac (n-1)