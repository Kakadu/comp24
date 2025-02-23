  $ dune exec rv_codegen <<- EOF
  > let f x = x
  .global f:
  sub sp, sp, 16
  sw s0, 8(sp) 
  addi s0, sp, 16
  lw s0, 8(sp) 
  add sp, sp, 16
  ret

  $ dune exec rv_codegen <<- EOF
  > let add x y = x + y
  .global add:
  sub sp, sp, 16
  sw s0, 8(sp) 
  addi s0, sp, 16
  add a0, a0, a1
  lw s0, 8(sp) 
  add sp, sp, 16
  ret

  $ dune exec rv_codegen <<- EOF
  > let rec fac x =
  >   if x < 2 then 1 else x * fac (x - 1) 
