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
  	mv t1, t0
  	beq  t1, x0, .L_else_0
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
  	add sp, sp, 32
  	mul a0, a0, t0
  .L_join_1:
  	lw s0, 8(sp) 
  	add sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fac acc x =
  >   if x < 2 then acc else fac (acc * x) (x - 1)

  $ dune exec rv_codegen <<- EOF
  > let adder a b c d e f g h j k l =
  >   a + b + c + d + e + f + g + h + j  + k + l
  .global adder:
  	sub sp, sp, 32
  	sw s0, 8(sp) 
  	addi s0, sp, 32
  	add t0, a0, a1
  	add t0, t0, a2
  	add t0, t0, a3
  	add t0, t0, a4
  	add t0, t0, a5
  	add t0, t0, a6
  	add t0, t0, a7
  	lw t1, 8(s0) 
  	add t0, t0, t1
  	lw t1, 16(s0) 
  	add t0, t0, t1
  	lw t1, 24(s0) 
  	add a0, t0, t1
  	lw s0, 8(sp) 
  	add sp, sp, 32
  	ret

  $ dune exec rv_codegen <<- EOF
  > let f a =
  >   let x = 5 in
  >   fun y -> a + x + y
  .global fresh_fun_0:
  	sub sp, sp, 32
  	sw s0, 8(sp) 
  	addi s0, sp, 32
  	lw t1, 0(a0) 
  	lw t2, 8(a0) 
  	add t0, t1, t2
  	add a0, t0, a1
  	lw s0, 8(sp) 
  	add sp, sp, 32
  	ret
  .global f:
  	sub sp, sp, 16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	li t0, 5
  	sub sp, sp, 32
  	sw a0, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	li a0, 3
  	call alloc_tuple
  	sw a0, 8(a0) 
  	sw t0, 16(a0) 
  	mv a1, a0
  	la  a0, fresh_fun_0
  	li a2, 1
  	li a3, 2
  	call rv_alloc_closure
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	lw a0, 8(sp) 
  	add sp, sp, 32
  	lw s0, 8(sp) 
  	add sp, sp, 16
  	ret
