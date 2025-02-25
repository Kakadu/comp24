  $ dune exec rv_codegen <<- EOF
  > let f x = x
  .global main
  f:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let main =
  >   let () = print_int 1 in
  >   0	 
  .global main
  main:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	addi sp, sp, -16
  	sw ra, 8(sp) 
  	li a0, 1
  	call print_int
  	mv x0, a0
  	lw ra, 8(sp) 
  	addi sp, sp, 16
  	li a0, 0
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let add x y = x + y
  .global main
  add:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	add a0, a0, a1
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fac x =
  >   if x < 2 then 1 else x * fac (x - 1) 
  .global main
  fac:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a0, 2
  	mv t1, t0
  	beq  t1, x0, .L_else_0
  	li a0, 1
  	j .L_join_1
  .L_else_0:
  	addi t0, a0, -1
  	addi sp, sp, -16
  	sw a0, 8(sp) 
  	sw ra, 16(sp) 
  	mv a0, t0
  	call fac
  	mv t0, a0
  	lw a0, 8(sp) 
  	lw ra, 16(sp) 
  	addi sp, sp, 16
  	mul a0, a0, t0
  .L_join_1:
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fac acc x =
  >   if x < 2 then acc else fac (acc * x) (x - 1)
  .global main
  fac:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a1, 2
  	mv t2, t0
  	beq  t2, x0, .L_else_0
  	j .L_join_1
  .L_else_0:
  	mul t0, a0, a1
  	addi t1, a1, -1
  	addi sp, sp, -32
  	sw a1, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	sw t1, 32(sp) 
  	mv a0, t0
  	mv a1, t1
  	call fac
  	lw a1, 8(sp) 
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	lw t1, 32(sp) 
  	addi sp, sp, 32
  .L_join_1:
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fack x k =
  >   if x < 2 then k 1 else
  >   fack (x - 1) (fun r -> k (r * x))
  .global main
  fresh_fun_0:
  	addi sp, sp, -32
  	sw s0, 8(sp) 
  	addi s0, sp, 32
  	lw t1, 8(a0) 
  	mul t0, a1, t1
  	addi sp, sp, -32
  	sw a1, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	lw a0, 0(a0) 
  	mv a1, t0
  	call call_closure
  	lw a1, 8(sp) 
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	addi sp, sp, 32
  	lw s0, 8(sp) 
  	addi sp, sp, 32
  	ret
  fack:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a0, 2
  	mv t1, t0
  	beq  t1, x0, .L_else_0
  	addi sp, sp, -32
  	sw a1, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	mv a0, a1
  	li a1, 1
  	call call_closure
  	lw a1, 8(sp) 
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	addi sp, sp, 32
  	j .L_join_1
  .L_else_0:
  	addi t0, a0, -1
  	addi sp, sp, -32
  	sw a1, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	mv a0, t0
  	addi sp, sp, -32
  	sw a0, 8(sp) 
  	sw ra, 16(sp) 
  	sw t0, 24(sp) 
  	li a0, 3
  	call alloc_tuple
  	sw a1, 8(a0) 
  	sw a0, 16(a0) 
  	mv a1, a0
  	la  a0, fresh_fun_0
  	li a2, 1
  	li a3, 2
  	call rv_alloc_closure
  	mv a1, a0
  	lw a0, 8(sp) 
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	addi sp, sp, 32
  	call fack
  	lw a1, 8(sp) 
  	lw ra, 16(sp) 
  	lw t0, 24(sp) 
  	addi sp, sp, 32
  .L_join_1:
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let adder a b c d e f g h j k l =
  >   a + b + c + d + e + f + g + h + j  + k + l
  .global main
  adder:
  	addi sp, sp, -32
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
  	addi sp, sp, 32
  	ret

  $ dune exec rv_codegen <<- EOF
  > let f a =
  >   let x = 5 in
  >   fun y -> a + x + y
  .global main
  fresh_fun_0:
  	addi sp, sp, -32
  	sw s0, 8(sp) 
  	addi s0, sp, 32
  	lw t1, 0(a0) 
  	lw t2, 8(a0) 
  	add t0, t1, t2
  	add a0, t0, a1
  	lw s0, 8(sp) 
  	addi sp, sp, 32
  	ret
  f:
  	addi sp, sp, -16
  	sw s0, 8(sp) 
  	addi s0, sp, 16
  	li t0, 5
  	addi sp, sp, -16
  	sw ra, 8(sp) 
  	sw t0, 16(sp) 
  	li a0, 3
  	call alloc_tuple
  	sw a0, 8(a0) 
  	sw t0, 16(a0) 
  	mv a1, a0
  	la  a0, fresh_fun_0
  	li a2, 1
  	li a3, 2
  	call rv_alloc_closure
  	lw ra, 8(sp) 
  	lw t0, 16(sp) 
  	addi sp, sp, 16
  	lw s0, 8(sp) 
  	addi sp, sp, 16
  	ret

