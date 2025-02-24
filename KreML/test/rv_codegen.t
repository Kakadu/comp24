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
  .global fac:
  	sub sp, sp, 16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a0, 2
  	sw t0, 0(sp) 
  	sub sp, sp, 16
  	beq  t0, x0, .L_else_0
  	li a0, 1
  	j .L_join_1
  .L_else_0:
  	addi t0, a0, -1
  	sub sp, sp, 32
  	sw a0, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	mv a0, t0
  	call fac
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	mv t0, a0
  	lw a0, 8(sp) 
  	add sp, sp, 16
  .L_join_1:
  	add sp, sp, 16
  	lw t0, 0(sp) 
  	lw s0, 8(sp) 
  	add sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let adder a b c d e f g h j k l =
  >   a + b + c + d + e + f + g + h + j  + k + l