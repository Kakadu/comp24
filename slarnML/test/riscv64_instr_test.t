  $ dune exec riscv64_instr_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
  > ;;
  > EOF
  f not found
  $ dune exec riscv64_instr_test << EOF
  > let fac n = 
  >   let rec fack n = if (n < 1) then n else n * (fack (n - 1)) in
  >   (fack n)
  > ;;
  > EOF
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  fack:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	li t0,1
  blt t0,a0,.tag_anf_op_1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-24(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t2,-88(s0)
  	mul t1,t2,a0
  	sd a0,-32(s0)
  	mv a0,t1
  .tag_anf_op_1_t:
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  fac:
  	addi sp,sp,-192
  	sd ra,176(sp)
  	sd s0,168(sp)
  	addi s0,sp,192
  	sd a0,-184(s0)
  	li t0,1
  blt t0,a0,.tag_anf_op_6
  	j .tag_anf_op_6_t
  .tag_anf_op_6:
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-24(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t2,-184(s0)
  	mul t1,t2,a0
  	sd a0,-32(s0)
  	mv a0,t1
  .tag_anf_op_6_t:
  	sd t1,-40(s0)
  	sd a0,-48(s0)
  	ld a0,-48(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-56(s0)
  	ld a0,-184(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-64(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,176(sp)
  	ld s0,168(sp)
  	addi sp,sp,192
  ret
  $ dune exec riscv64_instr_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
  > EOF
  e not found
  $ dune exec riscv64_instr_test < manytests/do_not_type/001.ml
  fac not exist
  $ dune exec riscv64_instr_test < manytests/do_not_type/002if.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	addi s0,sp,32
  	li t0,1
  beqz t0,.tag_if_bnch
  	li t1,1
  	mv a0,t1
  	j .tag_if_bnch_t
  .tag_if_bnch:
  	li t2,0
  	mv a0,t2
  .tag_if_bnch_t:
  	mv a0,a0
  	ld ra,16(sp)
  	ld s0,8(sp)
  	addi sp,sp,32
  ret
  $ dune exec riscv64_instr_test < manytests/do_not_type/003occurs.ml
  f not exist
  $ dune exec riscv64_instr_test < manytests/typed/001fac.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  fac:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	li t0,1
  ble t0,a0,.tag_anf_op_1
  	li t1,1
  	mv a0,t1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  	ld a0,-88(s0)
  	li t2,1
  	sub t3,a0,t2
  	sd t3,-24(s0)
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t3,-88(s0)
  	mul t2,t3,a0
  	sd a0,-32(s0)
  	mv a0,t2
  .tag_anf_op_1_t:
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  main:
  	addi sp,sp,-112
  	sd ra,104(sp)
  	sd s0,96(sp)
  	addi s0,sp,112
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	li a3,4
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,104(sp)
  	ld s0,96(sp)
  	addi sp,sp,112
  ret
  $ dune exec riscv64_instr_test < manytests/typed/002fac.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  anon_1:
  	addi sp,sp,-80
  	sd ra,72(sp)
  	sd s0,64(sp)
  	addi s0,sp,80
  	sd a2,-80(s0)
  	sd a1,-72(s0)
  	sd a0,-64(s0)
  	mul t0,a2,a0
  	sd t0,-24(s0)
  	ld a0,-72(s0)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,72(sp)
  	ld s0,64(sp)
  	addi sp,sp,80
  ret
  fac_cps:
  	addi sp,sp,-224
  	sd ra,208(sp)
  	sd s0,200(sp)
  	addi s0,sp,224
  	sd a1,-216(s0)
  	sd a0,-208(s0)
  	li t0,1
  beq a0,t0,.tag_anf_op_3
  	ld a0,-216(s0)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  	j .tag_anf_op_3_t
  .tag_anf_op_3:
  	ld t0,-208(s0)
  	li a1,1
  	sub t1,t0,a1
  	sd a0,-24(s0)
  	sd t1,-32(s0)
  	ld a0,-208(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-216(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a4,-48(s0)
  	ld a3,-40(s0)
  	li a2,2
  	li a1,3
  call part_app
  	sd a0,-56(s0)
  	lui a0,%hi(fac_cps)
  	addi a0,a0,%lo(fac_cps)
  	ld a4,-56(s0)
  	ld a3,-32(s0)
  	li a2,2
  	li a1,2
  call part_app
  .tag_anf_op_3_t:
  	sd a0,-64(s0)
  	mv a0,a0
  	ld ra,208(sp)
  	ld s0,200(sp)
  	addi sp,sp,224
  ret
  anon_1:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	addi s0,sp,32
  	sd a0,-24(s0)
  	mv a0,a0
  	ld ra,16(sp)
  	ld s0,8(sp)
  	addi sp,sp,32
  ret
  main:
  	addi sp,sp,-144
  	sd ra,136(sp)
  	sd s0,128(sp)
  	addi s0,sp,144
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(fac_cps)
  	addi a0,a0,%lo(fac_cps)
  	ld a4,-24(s0)
  	li a3,4
  	li a2,2
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,136(sp)
  	ld s0,128(sp)
  	addi sp,sp,144
  ret
  $ dune exec riscv64_instr_test < manytests/typed/003fib.ml
  : end_of_input
  $ dune exec riscv64_instr_test < manytests/typed/004manyargs.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  wrap:
  	addi sp,sp,-112
  	sd ra,96(sp)
  	sd s0,88(sp)
  	addi s0,sp,112
  	sd a0,-104(s0)
  	li t0,1
  	li t1,1
  beq t0,t1,.tag_anf_op_1
  	ld a0,-104(s0)
  	li a2,0
  	li a1,0
  call part_app
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  	sd a0,-24(s0)
  	ld a0,-104(s0)
  	li a2,0
  	li a1,0
  call part_app
  .tag_anf_op_1_t:
  	sd a0,-32(s0)
  	mv a0,a0
  	ld ra,96(sp)
  	ld s0,88(sp)
  	addi sp,sp,112
  ret
  a:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(a)
  	addi a0,a0,%lo(a)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  b:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(b)
  	addi a0,a0,%lo(b)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  c:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(c)
  	addi a0,a0,%lo(c)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  test3:
  	addi sp,sp,-336
  	sd ra,320(sp)
  	sd s0,312(sp)
  	addi s0,sp,336
  	sd a2,-328(s0)
  	sd a1,-320(s0)
  	sd a0,-312(s0)
  	lui a0,%hi(a)
  	addi a0,a0,%lo(a)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(b)
  	addi a0,a0,%lo(b)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-48(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-56(s0)
  	ld a0,-56(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	lui a0,%hi(c)
  	addi a0,a0,%lo(c)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-72(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-72(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-80(s0)
  	ld a0,-80(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li a1,0
  	mv a0,a1
  	ld ra,320(sp)
  	ld s0,312(sp)
  	addi sp,sp,336
  ret
  test10:
  	addi sp,sp,-480
  	sd ra,464(sp)
  	sd s0,456(sp)
  	addi s0,sp,480
  	sd a7,-472(s0)
  	sd a6,-464(s0)
  	sd a5,-456(s0)
  	sd a4,-448(s0)
  	sd a3,-440(s0)
  	sd a2,-432(s0)
  	sd a1,-424(s0)
  	sd a0,-416(s0)
  	lui a0,%hi(a)
  	addi a0,a0,%lo(a)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(b)
  	addi a0,a0,%lo(b)
  	li a2,0
  	li a1,1
  call part_app
  	ld a1,-24(s0)
  	add a2,a1,a0
  	sd a0,-32(s0)
  	sd a2,-40(s0)
  	lui a0,%hi(c)
  	addi a0,a0,%lo(c)
  	li a2,0
  	li a1,1
  call part_app
  	ld a2,-40(s0)
  	add a1,a2,a0
  	sd a0,-48(s0)
  	sd a1,-56(s0)
  	ld a0,-440(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a1,-56(s0)
  	add a2,a1,a0
  	sd a0,-64(s0)
  	sd a2,-72(s0)
  	ld a0,-448(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a2,-72(s0)
  	add a1,a2,a0
  	sd a0,-80(s0)
  	sd a1,-88(s0)
  	ld a0,-456(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a1,-88(s0)
  	add a2,a1,a0
  	sd a0,-96(s0)
  	sd a2,-104(s0)
  	ld a0,-464(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a2,-104(s0)
  	add a1,a2,a0
  	sd a0,-112(s0)
  	sd a1,-120(s0)
  	ld a0,-472(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a1,-120(s0)
  	add a2,a1,a0
  	sd a0,-128(s0)
  	sd a2,-136(s0)
  	ld a0,0(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a2,-136(s0)
  	add a1,a2,a0
  	sd a0,-144(s0)
  	sd a1,-152(s0)
  	ld a0,8(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a1,-152(s0)
  	add a2,a1,a0
  	mv a0,a2
  	ld ra,464(sp)
  	ld s0,456(sp)
  	addi sp,sp,480
  ret
  temp0:
  	addi sp,sp,-112
  	sd ra,96(sp)
  	sd s0,88(sp)
  	addi s0,sp,112
  	lui a0,%hi(test10)
  	addi a0,a0,%lo(test10)
  	li a2,0
  	li a1,10
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(wrap)
  	addi a0,a0,%lo(wrap)
  	li t6,10000
  	sd t6,0(sp)
  	li t6,100000
  	sd t6,8(sp)
  	li t6,1000000
  	sd t6,16(sp)
  	li t6,10000000
  	sd t6,24(sp)
  	li t6,100000000
  	sd t6,32(sp)
  	li t6,1000000000
  	sd t6,40(sp)
  	li a7,1000
  	li a6,100
  	li a5,10
  	li a4,1
  	ld a3,-24(s0)
  	li a2,11
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,96(sp)
  	ld s0,88(sp)
  	addi sp,sp,112
  ret
  temp1:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(temp0)
  	addi a0,a0,%lo(temp0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  temp2:
  	addi sp,sp,-80
  	sd ra,72(sp)
  	sd s0,64(sp)
  	addi s0,sp,80
  	lui a0,%hi(test3)
  	addi a0,a0,%lo(test3)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(wrap)
  	addi a0,a0,%lo(wrap)
  	li a6,100
  	li a5,10
  	li a4,1
  	ld a3,-24(s0)
  	li a2,4
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,72(sp)
  	ld s0,64(sp)
  	addi sp,sp,80
  ret
  main:
  	addi sp,sp,-336
  	sd ra,320(sp)
  	sd s0,312(sp)
  	addi s0,sp,336
  	lui a0,%hi(test10)
  	addi a0,a0,%lo(test10)
  	li a2,0
  	li a1,10
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(wrap)
  	addi a0,a0,%lo(wrap)
  	li t6,10000
  	sd t6,0(sp)
  	li t6,100000
  	sd t6,8(sp)
  	li t6,1000000
  	sd t6,16(sp)
  	li t6,10000000
  	sd t6,24(sp)
  	li t6,100000000
  	sd t6,32(sp)
  	li t6,1000000000
  	sd t6,40(sp)
  	li a7,1000
  	li a6,100
  	li a5,10
  	li a4,1
  	ld a3,-24(s0)
  	li a2,11
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(temp0)
  	addi a0,a0,%lo(temp0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-48(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-56(s0)
  	ld a0,-56(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	lui a0,%hi(test3)
  	addi a0,a0,%lo(test3)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-72(s0)
  	lui a0,%hi(wrap)
  	addi a0,a0,%lo(wrap)
  	li a6,100
  	li a5,10
  	li a4,1
  	ld a3,-72(s0)
  	li a2,4
  	li a1,1
  call part_app
  	sd a0,-80(s0)
  	ld a0,-80(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,320(sp)
  	ld s0,312(sp)
  	addi sp,sp,336
  ret
  $ dune exec riscv64_instr_test < manytests/typed/005fix.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  fix:
  	addi sp,sp,-160
  	sd ra,152(sp)
  	sd s0,144(sp)
  	addi s0,sp,160
  	sd a1,-160(s0)
  	sd a0,-152(s0)
  	ld a0,-152(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(fix)
  	addi a0,a0,%lo(fix)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	ld a0,-160(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-152(s0)
  	ld a4,-40(s0)
  	ld a3,-32(s0)
  	li a2,2
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,152(sp)
  	ld s0,144(sp)
  	addi sp,sp,160
  ret
  fac:
  	addi sp,sp,-192
  	sd ra,184(sp)
  	sd s0,176(sp)
  	addi s0,sp,192
  	sd a1,-192(s0)
  	sd a0,-184(s0)
  	ld a0,-192(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li a1,1
  ble a1,a0,.tag_anf_op_6
  	li t0,1
  	sd a0,-24(s0)
  	mv a0,t0
  	j .tag_anf_op_6_t
  .tag_anf_op_6:
  	ld a0,-192(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-32(s0)
  	ld a0,-192(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,1
  	sub a1,a0,t0
  	sd a0,-40(s0)
  	sd a1,-48(s0)
  	ld a0,-184(s0)
  	ld a3,-48(s0)
  	li a2,1
  	li a1,0
  call part_app
  	ld a1,-32(s0)
  	mul t0,a1,a0
  	sd a0,-56(s0)
  	mv a0,t0
  .tag_anf_op_6_t:
  	mv a0,a0
  	ld ra,184(sp)
  	ld s0,176(sp)
  	addi sp,sp,192
  ret
  main:
  	addi sp,sp,-144
  	sd ra,136(sp)
  	sd s0,128(sp)
  	addi s0,sp,144
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	li a2,0
  	li a1,2
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(fix)
  	addi a0,a0,%lo(fix)
  	li a4,6
  	ld a3,-24(s0)
  	li a2,2
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,136(sp)
  	ld s0,128(sp)
  	addi sp,sp,144
  ret
  $ dune exec riscv64_instr_test < manytests/typed/006partial.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  anon_1:
  	addi sp,sp,-32
  	sd ra,24(sp)
  	sd s0,16(sp)
  	addi s0,sp,32
  	sd a0,-32(s0)
  	li t0,2
  	add t1,a0,t0
  	mv a0,t1
  	ld ra,24(sp)
  	ld s0,16(sp)
  	addi sp,sp,32
  ret
  anon_2:
  	addi sp,sp,-32
  	sd ra,24(sp)
  	sd s0,16(sp)
  	addi s0,sp,32
  	sd a0,-32(s0)
  	li t0,10
  	mul t1,a0,t0
  	mv a0,t1
  	ld ra,24(sp)
  	ld s0,16(sp)
  	addi sp,sp,32
  ret
  foo:
  	addi sp,sp,-128
  	sd ra,120(sp)
  	sd s0,112(sp)
  	addi s0,sp,128
  	sd a0,-128(s0)
  	ld a0,-128(s0)
  	li a2,0
  	li a1,0
  call part_app
  beqz a0,.tag_if_bnch
  	sd a0,-24(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	li a2,0
  	li a1,1
  call part_app
  	j .tag_if_bnch_t
  .tag_if_bnch:
  	sd a0,-32(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  .tag_if_bnch_t:
  	sd a0,-40(s0)
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
  ret
  foo:
  	addi sp,sp,-192
  	sd ra,176(sp)
  	sd s0,168(sp)
  	addi s0,sp,192
  	sd a0,-184(s0)
  	ld a0,-184(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-24(s0)
  	li a3,0
  	li a2,2
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-32(s0)
  	li a3,1
  	li a2,2
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-40(s0)
  	li a3,0
  	li a2,2
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-48(s0)
  	li a3,1
  	li a2,2
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,176(sp)
  	ld s0,168(sp)
  	addi sp,sp,192
  ret
  main:
  	addi sp,sp,-112
  	sd ra,104(sp)
  	sd s0,96(sp)
  	addi s0,sp,112
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,11
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,104(sp)
  	ld s0,96(sp)
  	addi sp,sp,112
  ret
  $ dune exec riscv64_instr_test < manytests/typed/006partial2.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  foo:
  	addi sp,sp,-448
  	sd ra,432(sp)
  	sd s0,424(sp)
  	addi s0,sp,448
  	sd a2,-440(s0)
  	sd a1,-432(s0)
  	sd a0,-424(s0)
  	ld a0,-424(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-432(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-48(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-56(s0)
  	ld a0,-56(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	ld a0,-440(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-72(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-72(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-80(s0)
  	ld a0,-80(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-88(s0)
  	ld a0,-424(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-96(s0)
  	ld a0,-432(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-104(s0)
  	ld a0,-440(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a1,-104(s0)
  	mul a2,a1,a0
  	ld t0,-96(s0)
  	add t1,t0,a2
  	mv a0,t1
  	ld ra,432(sp)
  	ld s0,424(sp)
  	addi sp,sp,448
  ret
  foo:
  	addi sp,sp,-48
  	sd ra,40(sp)
  	sd s0,32(sp)
  	addi s0,sp,48
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,40(sp)
  	ld s0,32(sp)
  	addi sp,sp,48
  ret
  foo:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a4,2
  	ld a3,-24(s0)
  	li a2,2
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  foo:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a4,3
  	ld a3,-24(s0)
  	li a2,2
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  main:
  	addi sp,sp,-368
  	sd ra,360(sp)
  	sd s0,352(sp)
  	addi s0,sp,368
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,1
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-24(s0)
  	ld a0,-24(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a4,2
  	ld a3,-40(s0)
  	li a2,2
  	li a1,3
  call part_app
  	sd a0,-48(s0)
  	ld a0,-48(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-56(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-64(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a4,3
  	ld a3,-64(s0)
  	li a2,2
  	li a1,3
  call part_app
  	sd a0,-72(s0)
  	ld a0,-72(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-80(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-88(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-88(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-96(s0)
  	ld a0,-96(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,360(sp)
  	ld s0,352(sp)
  	addi sp,sp,368
  ret
  $ dune exec riscv64_instr_test < manytests/typed/006partial3.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global _start
  _start:
  	addi sp,sp,-24
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,24
  call init_part_apps
  call main
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,24
  	li a7,93
  ecall
  anon_2:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	ld a0,-88(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  anon_1:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	sd a0,-152(s0)
  	ld a0,-152(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
  ret
  foo:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	sd a0,-152(s0)
  	ld a0,-152(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-24(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	li a2,0
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
  ret
  main:
  	addi sp,sp,-80
  	sd ra,72(sp)
  	sd s0,64(sp)
  	addi s0,sp,80
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a5,9
  	li a4,8
  	li a3,4
  	li a2,3
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	ld a0,-24(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,0
  	mv a0,t0
  	ld ra,72(sp)
  	ld s0,64(sp)
  	addi sp,sp,80
  ret
  $ dune exec riscv64_instr_test < manytests/typed/007order.ml
  : end_of_input
  $ dune exec riscv64_instr_test < manytests/typed/008ascription.ml
  : end_of_input
  $ dune exec riscv64_instr_test < manytests/typed/015tuples.ml
  : end_of_input
  $ dune exec riscv64_instr_test < manytests/typed/016lists.ml
  : end_of_input
