  $ dune exec rv_codegen <<- EOF
  > let f x = x
  .global main
  f:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let main =
  >   let () = print_int 1 in
  >   0	 
  .global main
  main:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	addi sp, sp, -16
  	sd ra, 8(sp) 
  	li a0, 1
  	call print_int
  	mv x0, a0
  	ld ra, 8(sp) 
  	addi sp, sp, 16
  	li a0, 0
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	call fflush
  	li a7, 93
  	li a0, 0
  	ecall

  $ dune exec rv_codegen <<- EOF
  > let add x y = x + y
  .global main
  add:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	add a0, a0, a1
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fac x =
  >   if x < 2 then 1 else x * fac (x - 1) 
  .global main
  fac:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a0, 2
  	mv t1, t0
  	beq  t1, x0, .L_else_0
  	li a0, 1
  	j .L_join_1
  .L_else_0:
  	addi t0, a0, -1
  	addi sp, sp, -32
  	sd a0, 8(sp) 
  	sd ra, 16(sp) 
  	mv a0, t0
  	call fac
  	mv t0, a0
  	ld a0, 8(sp) 
  	ld ra, 16(sp) 
  	addi sp, sp, 32
  	mul a0, a0, t0
  .L_join_1:
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fac acc x =
  >   if x < 2 then acc else fac (acc * x) (x - 1)
  .global main
  fac:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a1, 2
  	mv t2, t0
  	beq  t2, x0, .L_else_0
  	j .L_join_1
  .L_else_0:
  	mul t0, a0, a1
  	addi t1, a1, -1
  	addi sp, sp, -48
  	sd a1, 8(sp) 
  	sd ra, 16(sp) 
  	sd t0, 24(sp) 
  	sd t1, 32(sp) 
  	mv a0, t0
  	mv a1, t1
  	call fac
  	ld a1, 8(sp) 
  	ld ra, 16(sp) 
  	ld t0, 24(sp) 
  	ld t1, 32(sp) 
  	addi sp, sp, 48
  .L_join_1:
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fack x k =
  >   if x < 2 then k 1 else
  >   fack (x - 1) (fun r -> k (r * x))
  .global main
  fresh_fun_0:
  	addi sp, sp, -32
  	sd s0, 8(sp) 
  	addi s0, sp, 32
  	ld t1, 8(a0) 
  	mul t0, a1, t1
  	addi sp, sp, -32
  	sd a1, 8(sp) 
  	sd ra, 16(sp) 
  	sd t0, 24(sp) 
  	ld t1, 0(a0) 
  	li a2, 1
  	addi sp, sp, -16
  	addi a1, sp, 8
  	sd t0, 8(sp) 
  	mv a0, t1
  	call call_closure
  	addi sp, sp, 16
  	ld a1, 8(sp) 
  	ld ra, 16(sp) 
  	ld t0, 24(sp) 
  	addi sp, sp, 32
  	ld s0, 8(sp) 
  	addi sp, sp, 32
  	ret
  fack:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a0, 2
  	mv t2, t0
  	beq  t2, x0, .L_else_0
  	addi sp, sp, -48
  	sd a1, 8(sp) 
  	sd ra, 16(sp) 
  	sd t0, 24(sp) 
  	sd t1, 32(sp) 
  	mv t3, a1
  	li a2, 1
  	addi sp, sp, -16
  	addi a1, sp, 8
  	li t4, 1
  	sd t4, 8(sp) 
  	mv a0, t3
  	call call_closure
  	addi sp, sp, 16
  	ld a1, 8(sp) 
  	ld ra, 16(sp) 
  	ld t0, 24(sp) 
  	ld t1, 32(sp) 
  	addi sp, sp, 48
  	j .L_join_1
  .L_else_0:
  	addi t0, a0, -1
  	addi sp, sp, -48
  	sd a0, 8(sp) 
  	sd a1, 16(sp) 
  	sd ra, 24(sp) 
  	sd t0, 32(sp) 
  	sd t1, 40(sp) 
  	li a0, 3
  	call alloc_tuple
  	mv t3, a0
  	ld a0, 8(sp) 
  	ld a1, 16(sp) 
  	ld ra, 24(sp) 
  	ld t0, 32(sp) 
  	ld t1, 40(sp) 
  	addi sp, sp, 48
  	sd a1, 8(t3) 
  	sd a0, 16(t3) 
  	addi sp, sp, -48
  	sd a0, 8(sp) 
  	sd a1, 16(sp) 
  	sd ra, 24(sp) 
  	sd t0, 32(sp) 
  	mv a1, t3
  	la  a0, fresh_fun_0
  	li a2, 1
  	li a3, 2
  	call alloc_closure
  	mv t1, a0
  	ld a0, 8(sp) 
  	ld a1, 16(sp) 
  	ld ra, 24(sp) 
  	ld t0, 32(sp) 
  	addi sp, sp, 48
  	addi sp, sp, -48
  	sd a1, 8(sp) 
  	sd ra, 16(sp) 
  	sd t0, 24(sp) 
  	sd t1, 32(sp) 
  	mv a0, t0
  	mv a1, t1
  	call fack
  	ld a1, 8(sp) 
  	ld ra, 16(sp) 
  	ld t0, 24(sp) 
  	ld t1, 32(sp) 
  	addi sp, sp, 48
  .L_join_1:
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let rec fix f x = f (fix f) x
  > let fac self x =
  >   if x < 2 then 1 else x * self (x - 1)
  .global main
  fix:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	addi sp, sp, -32
  	sd a0, 8(sp) 
  	sd a1, 16(sp) 
  	sd ra, 24(sp) 
  	addi sp, sp, -48
  	sd a0, 8(sp) 
  	sd a1, 16(sp) 
  	sd ra, 24(sp) 
  	sd t0, 32(sp) 
  	li a0, 2
  	call alloc_tuple
  	mv t2, a0
  	ld a0, 8(sp) 
  	ld a1, 16(sp) 
  	ld ra, 24(sp) 
  	ld t0, 32(sp) 
  	addi sp, sp, 48
  	addi sp, sp, -48
  	sd a0, 8(sp) 
  	sd a1, 16(sp) 
  	sd ra, 24(sp) 
  	sd t0, 32(sp) 
  	mv a1, t2
  	la  a0, fix
  	li a2, 2
  	li a3, 0
  	call alloc_closure
  	mv t1, a0
  	ld a0, 8(sp) 
  	ld a1, 16(sp) 
  	ld ra, 24(sp) 
  	ld t0, 32(sp) 
  	addi sp, sp, 48
  	li a2, 1
  	addi sp, sp, -16
  	addi a1, sp, 8
  	sd a0, 8(sp) 
  	mv a0, t1
  	call call_closure
  	mv t0, a0
  	addi sp, sp, 16
  	ld a0, 8(sp) 
  	ld a1, 16(sp) 
  	ld ra, 24(sp) 
  	addi sp, sp, 32
  	addi sp, sp, -32
  	sd a1, 8(sp) 
  	sd ra, 16(sp) 
  	sd t0, 24(sp) 
  	mv t2, a0
  	li a2, 2
  	addi sp, sp, -32
  	addi a1, sp, 8
  	sd t0, 8(sp) 
  	sd a1, 16(sp) 
  	mv a0, t2
  	call call_closure
  	addi sp, sp, 32
  	ld a1, 8(sp) 
  	ld ra, 16(sp) 
  	ld t0, 24(sp) 
  	addi sp, sp, 32
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret
  fac:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	slti t0, a1, 2
  	mv t2, t0
  	beq  t2, x0, .L_else_0
  	li a0, 1
  	j .L_join_1
  .L_else_0:
  	addi t0, a1, -1
  	addi sp, sp, -32
  	sd a0, 8(sp) 
  	sd a1, 16(sp) 
  	sd ra, 24(sp) 
  	mv t3, a0
  	li a2, 1
  	addi sp, sp, -16
  	addi a1, sp, 8
  	sd t0, 8(sp) 
  	mv a0, t3
  	call call_closure
  	mv t0, a0
  	addi sp, sp, 16
  	ld a0, 8(sp) 
  	ld a1, 16(sp) 
  	ld ra, 24(sp) 
  	addi sp, sp, 32
  	mul a0, a1, t0
  .L_join_1:
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret


  $ dune exec rv_codegen <<- EOF
  > let adder a b c d e f g h j k l =
  >   a + b + c + d + e + f + g + h + j  + k + l
  .global main
  adder:
  	addi sp, sp, -48
  	sd s0, 8(sp) 
  	addi s0, sp, 48
  	add t0, a1, a2
  	add t0, t0, a3
  	add t0, t0, a4
  	add t0, t0, a5
  	add t0, t0, a6
  	add t0, t0, a7
  	ld t1, 0(a0) 
  	add t0, t0, t1
  	ld t1, 8(a0) 
  	add t0, t0, t1
  	ld t1, 16(a0) 
  	add t0, t0, t1
  	ld t1, 24(a0) 
  	add a0, t0, t1
  	ld s0, 8(sp) 
  	addi sp, sp, 48
  	ret

  $ dune exec rv_codegen <<- EOF
  > let f a =
  >   let x = 5 in
  >   fun y -> a + x + y
  .global main
  fresh_fun_0:
  	addi sp, sp, -32
  	sd s0, 8(sp) 
  	addi s0, sp, 32
  	ld t1, 0(a0) 
  	ld t2, 8(a0) 
  	add t0, t1, t2
  	add a0, t0, a1
  	ld s0, 8(sp) 
  	addi sp, sp, 32
  	ret
  f:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	li t0, 5
  	addi sp, sp, -32
  	sd a0, 8(sp) 
  	sd ra, 16(sp) 
  	sd t0, 24(sp) 
  	li a0, 3
  	call alloc_tuple
  	mv t1, a0
  	ld a0, 8(sp) 
  	ld ra, 16(sp) 
  	ld t0, 24(sp) 
  	addi sp, sp, 32
  	sd a0, 8(t1) 
  	sd t0, 16(t1) 
  	addi sp, sp, -32
  	sd ra, 8(sp) 
  	sd t0, 16(sp) 
  	mv a1, t1
  	la  a0, fresh_fun_0
  	li a2, 1
  	li a3, 2
  	call alloc_closure
  	ld ra, 8(sp) 
  	ld t0, 16(sp) 
  	addi sp, sp, 32
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret

  $ dune exec rv_codegen <<- EOF
  > let main =
  >   let a = 5 in
  >   let f x = x + a in
  >   let () = print_int (f 5) in
  >   0
  .global main
  f_2:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	ld t0, 0(a0) 
  	add a0, a1, t0
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	ret
  main:
  	addi sp, sp, -16
  	sd s0, 8(sp) 
  	addi s0, sp, 16
  	li t0, 5
  	addi sp, sp, -16
  	sd ra, 8(sp) 
  	addi sp, sp, -32
  	sd ra, 8(sp) 
  	sd t0, 16(sp) 
  	li a0, 2
  	call alloc_tuple
  	mv t2, a0
  	ld ra, 8(sp) 
  	ld t0, 16(sp) 
  	addi sp, sp, 32
  	sd t0, 8(t2) 
  	addi sp, sp, -32
  	sd ra, 8(sp) 
  	sd t0, 16(sp) 
  	mv a1, t2
  	la  a0, f_2
  	li a2, 1
  	li a3, 1
  	call alloc_closure
  	mv t1, a0
  	ld ra, 8(sp) 
  	ld t0, 16(sp) 
  	addi sp, sp, 32
  	li a2, 1
  	addi sp, sp, -16
  	addi a1, sp, 8
  	li t3, 5
  	sd t3, 8(sp) 
  	mv a0, t1
  	call call_closure
  	mv t0, a0
  	addi sp, sp, 16
  	ld ra, 8(sp) 
  	addi sp, sp, 16
  	addi sp, sp, -32
  	sd ra, 8(sp) 
  	sd t0, 16(sp) 
  	mv a0, t0
  	call print_int
  	mv x0, a0
  	ld ra, 8(sp) 
  	ld t0, 16(sp) 
  	addi sp, sp, 32
  	li a0, 0
  	ld s0, 8(sp) 
  	addi sp, sp, 16
  	call fflush
  	li a7, 93
  	li a0, 0
  	ecall

  $ dune exec closure_conv <<- EOF
  > let rec fix f x = f (fix f) x
  fix(f_0 x_1 ) {let t_0 = { name: fix, arity: 2 env_size: 0, arrange [ ]}  ([ 
                           f_0  ]) in 
                  f_0 ([ t_0 x_1  ]) } 
  
