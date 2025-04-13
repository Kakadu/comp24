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
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  fack:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	li t0,1
  blt a0,t0,.tag_anf_op_1
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-32(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t2,-88(s0)
  	mul t1,t2,a0
  	sd a0,-40(s0)
  	mv a0,t1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  	mv a0,t2
  .tag_anf_op_1_t:
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  fac:
  	addi sp,sp,-128
  	sd ra,120(sp)
  	sd s0,112(sp)
  	addi s0,sp,128
  	sd a0,-128(s0)
  	li t0,1
  blt a0,t0,.tag_anf_op_6
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-32(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t2,-128(s0)
  	mul t1,t2,a0
  	sd a0,-40(s0)
  	mv a0,t1
  	j .tag_anf_op_6_t
  .tag_anf_op_6:
  	mv a0,t2
  .tag_anf_op_6_t:
  	sd a0,-48(s0)
  	sd t1,-56(s0)
  	sd a0,-64(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-128(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
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
  Id fac not found in env
  fac not found

  $ dune exec riscv64_instr_test < manytests/do_not_type/002if.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  main2:
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
  Id f not found in env
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  anon_2:
  	addi sp,sp,-96
  	sd ra,88(sp)
  	sd s0,80(sp)
  	addi s0,sp,96
  	sd a1,-96(s0)
  	sd a0,-88(s0)
  	ld a0,-88(s0)
  	mv a3,a0
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	ld a3,-96(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
  ret
  anon_1:
  	addi sp,sp,-128
  	sd ra,120(sp)
  	sd s0,112(sp)
  	addi s0,sp,128
  	sd a1,-128(s0)
  	sd a0,-120(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	ld a3,-128(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	ld a3,-120(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-120(s0)
  	ld a3,-40(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
  ret
  fix:
  	addi sp,sp,-64
  	sd ra,48(sp)
  	sd s0,40(sp)
  	addi s0,sp,64
  	sd a0,-56(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a3,-56(s0)
  	li a2,1
  	li a1,2
  call part_app
  	mv a0,a0
  	ld ra,48(sp)
  	ld s0,40(sp)
  	addi sp,sp,64
  ret
  anon_4:
  	addi sp,sp,-96
  	sd ra,88(sp)
  	sd s0,80(sp)
  	addi s0,sp,96
  	sd a1,-96(s0)
  	sd a0,-88(s0)
  	ld a0,-88(s0)
  	mv a3,a0
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	ld a3,-96(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
  ret
  anon_3:
  	addi sp,sp,-128
  	sd ra,112(sp)
  	sd s0,104(sp)
  	addi s0,sp,128
  	sd a0,-120(s0)
  	lui a0,%hi(f)
  	addi a0,a0,%lo(f)
  	ld a3,-120(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(anon_4)
  	addi a0,a0,%lo(anon_4)
  	ld a3,-120(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-40(s0)
  	ld a0,-32(s0)
  	ld a3,-40(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,112(sp)
  	ld s0,104(sp)
  	addi sp,sp,128
  ret

  $ dune exec riscv64_instr_test < manytests/typed/001fac.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  fac:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	li t0,1
  ble a0,t0,.tag_anf_op_1
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-32(s0)
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t2,-88(s0)
  	mul t1,t2,a0
  	sd a0,-40(s0)
  	mv a0,t1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  	li a0,1
  .tag_anf_op_1_t:
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  main2:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	li a3,4
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret

  $ dune exec riscv64_instr_test < manytests/typed/002fac.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
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
  	sd t0,-32(s0)
  	ld a0,-72(s0)
  	ld a3,-32(s0)
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
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-32(s0)
  	lui a0,%hi(fac_cps)
  	addi a0,a0,%lo(fac_cps)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a3,-208(s0)
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-48(s0)
  	ld a0,-48(s0)
  	ld a3,-216(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-56(s0)
  	ld a0,-40(s0)
  	ld a3,-56(s0)
  	li a2,1
  	li a1,0
  call part_app
  	j .tag_anf_op_3_t
  .tag_anf_op_3:
  	sd a0,-64(s0)
  	ld a0,-216(s0)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  .tag_anf_op_3_t:
  	sd a0,-72(s0)
  	mv a0,a0
  	ld ra,208(sp)
  	ld s0,200(sp)
  	addi sp,sp,224
  ret
  anon_2:
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
  main2:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	lui a0,%hi(fac_cps)
  	addi a0,a0,%lo(fac_cps)
  	li a3,4
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	ld a0,-32(s0)
  	ld a3,-40(s0)
  	li a2,1
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
  	li t0,0
  	mv a0,t0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
  ret

  $ dune exec riscv64_instr_test < manytests/typed/003fib.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  n1:
  	addi sp,sp,-48
  	sd ra,40(sp)
  	sd s0,32(sp)
  	addi s0,sp,48
  	sd a2,-48(s0)
  	sd a1,-40(s0)
  	sd a0,-32(s0)
  	li t0,1
  	sub t1,a2,t0
  	mv a0,t1
  	ld ra,40(sp)
  	ld s0,32(sp)
  	addi sp,sp,48
  ret
  ab:
  	addi sp,sp,-48
  	sd ra,40(sp)
  	sd s0,32(sp)
  	addi s0,sp,48
  	sd a2,-48(s0)
  	sd a1,-40(s0)
  	sd a0,-32(s0)
  	add t0,a0,a1
  	mv a0,t0
  	ld ra,40(sp)
  	ld s0,32(sp)
  	addi sp,sp,48
  ret
  fib_acc:
  	addi sp,sp,-384
  	sd ra,368(sp)
  	sd s0,360(sp)
  	addi s0,sp,384
  	sd a2,-376(s0)
  	sd a1,-368(s0)
  	sd a0,-360(s0)
  	li t0,1
  beq a2,t0,.tag_anf_op_3
  	li t1,1
  	sub t2,a2,t1
  	sd t2,-32(s0)
  	add t3,a0,a1
  	sd t3,-40(s0)
  	sd t2,-48(s0)
  	sd t3,-56(s0)
  	lui a0,%hi(fib_acc)
  	addi a0,a0,%lo(fib_acc)
  	ld a3,-368(s0)
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-64(s0)
  	lui a0,%hi(ab)
  	addi a0,a0,%lo(ab)
  	ld a3,-360(s0)
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-72(s0)
  	ld a0,-72(s0)
  	ld a3,-368(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-80(s0)
  	ld a0,-80(s0)
  	ld a3,-376(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-88(s0)
  	ld a0,-64(s0)
  	ld a3,-88(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-96(s0)
  	lui a0,%hi(n1)
  	addi a0,a0,%lo(n1)
  	ld a3,-360(s0)
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-104(s0)
  	ld a0,-104(s0)
  	ld a3,-368(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-112(s0)
  	ld a0,-112(s0)
  	ld a3,-376(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-120(s0)
  	ld a0,-96(s0)
  	ld a3,-120(s0)
  	li a2,1
  	li a1,0
  call part_app
  	j .tag_anf_op_3_t
  .tag_anf_op_3:
  	ld t3,-368(s0)
  	sd a0,-128(s0)
  	mv a0,t3
  .tag_anf_op_3_t:
  	mv a0,a0
  	ld ra,368(sp)
  	ld s0,360(sp)
  	addi sp,sp,384
  ret
  fib:
  	addi sp,sp,-128
  	sd ra,120(sp)
  	sd s0,112(sp)
  	addi s0,sp,128
  	sd a0,-128(s0)
  	li t0,2
  blt a0,t0,.tag_anf_op_16
  	li t1,1
  	sub t2,a0,t1
  	sd t2,-32(s0)
  	lui a0,%hi(fib)
  	addi a0,a0,%lo(fib)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t2,-128(s0)
  	li t1,2
  	sub t0,t2,t1
  	sd a0,-40(s0)
  	sd t0,-48(s0)
  	lui a0,%hi(fib)
  	addi a0,a0,%lo(fib)
  	ld a3,-48(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t0,-40(s0)
  	add t1,t0,a0
  	sd a0,-56(s0)
  	mv a0,t1
  	j .tag_anf_op_16_t
  .tag_anf_op_16:
  	ld a0,-128(s0)
  .tag_anf_op_16_t:
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
  ret
  main2:
  	addi sp,sp,-224
  	sd ra,216(sp)
  	sd s0,208(sp)
  	addi s0,sp,224
  	lui a0,%hi(fib_acc)
  	addi a0,a0,%lo(fib_acc)
  	li a3,0
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a3,4
  	li a2,1
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
  	sd a0,-64(s0)
  	lui a0,%hi(fib)
  	addi a0,a0,%lo(fib)
  	li a3,4
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-72(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-72(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-80(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,216(sp)
  	ld s0,208(sp)
  	addi sp,sp,224
  ret

  $ dune exec riscv64_instr_test < manytests/typed/004manyargs.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  wrap:
  	addi sp,sp,-48
  	sd ra,32(sp)
  	sd s0,24(sp)
  	addi s0,sp,48
  	sd a0,-40(s0)
  	li t0,1
  	li t1,1
  beq t0,t1,.tag_anf_op_1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  .tag_anf_op_1_t:
  	mv a0,a0
  	ld ra,32(sp)
  	ld s0,24(sp)
  	addi sp,sp,48
  ret
  a_0:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a2,-72(s0)
  	sd a1,-64(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-56(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,64(sp)
  	ld s0,56(sp)
  	addi sp,sp,80
  ret
  b_0:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a2,-72(s0)
  	sd a1,-64(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-64(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,64(sp)
  	ld s0,56(sp)
  	addi sp,sp,80
  ret
  c_0:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a2,-72(s0)
  	sd a1,-64(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-72(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,64(sp)
  	ld s0,56(sp)
  	addi sp,sp,80
  ret
  test3:
  	addi sp,sp,-160
  	sd ra,152(sp)
  	sd s0,144(sp)
  	addi s0,sp,160
  	sd a2,-160(s0)
  	sd a1,-152(s0)
  	sd a0,-144(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-144(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	sd a0,-40(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-152(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-160(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-64(s0)
  	li a1,0
  	mv a0,a1
  	ld ra,152(sp)
  	ld s0,144(sp)
  	addi sp,sp,160
  ret
  test10:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	sd a7,-152(s0)
  	sd a6,-144(s0)
  	sd a5,-136(s0)
  	sd a4,-128(s0)
  	sd a3,-120(s0)
  	sd a2,-112(s0)
  	sd a1,-104(s0)
  	sd a0,-96(s0)
  	add t0,a0,a1
  	add t1,t0,a2
  	add t2,t1,a3
  	add t3,t2,a4
  	add t4,t3,a5
  	add t5,t4,a6
  	add t6,t5,a7
  	ld a7,0(s0)
  	add a6,t6,a7
  	ld a5,8(s0)
  	add a4,a6,a5
  	mv a0,a4
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
  ret
  main2:
  	addi sp,sp,-720
  	sd ra,712(sp)
  	sd s0,704(sp)
  	addi s0,sp,720
  	lui a0,%hi(test10)
  	addi a0,a0,%lo(test10)
  	li a2,0
  	li a1,10
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(wrap)
  	addi a0,a0,%lo(wrap)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	ld a0,-48(s0)
  	li a3,10
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-56(s0)
  	ld a0,-56(s0)
  	li a3,100
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	ld a0,-64(s0)
  	li a3,1000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-72(s0)
  	ld a0,-72(s0)
  	li a3,10000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-80(s0)
  	ld a0,-80(s0)
  	li a3,100000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-88(s0)
  	ld a0,-88(s0)
  	li a3,1000000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-96(s0)
  	ld a0,-96(s0)
  	li a3,10000000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-104(s0)
  	ld a0,-104(s0)
  	li a3,100000000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-112(s0)
  	ld a0,-112(s0)
  	li a3,1000000000
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-120(s0)
  	sd a0,-128(s0)
  	ld a0,-128(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-136(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-136(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-144(s0)
  	sd a0,-152(s0)
  	lui a0,%hi(test3)
  	addi a0,a0,%lo(test3)
  	li a2,0
  	li a1,3
  call part_app
  	sd a0,-160(s0)
  	lui a0,%hi(wrap)
  	addi a0,a0,%lo(wrap)
  	ld a3,-160(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-168(s0)
  	ld a0,-168(s0)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-176(s0)
  	ld a0,-176(s0)
  	li a3,10
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-184(s0)
  	ld a0,-184(s0)
  	li a3,100
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-192(s0)
  	sd a0,-200(s0)
  	ld a0,-200(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-208(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-208(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-216(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,712(sp)
  	ld s0,704(sp)
  	addi sp,sp,720
  ret

  $ dune exec riscv64_instr_test < manytests/typed/005fix.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  fix:
  	addi sp,sp,-128
  	sd ra,120(sp)
  	sd s0,112(sp)
  	addi s0,sp,128
  	sd a1,-128(s0)
  	sd a0,-120(s0)
  	lui a0,%hi(fix)
  	addi a0,a0,%lo(fix)
  	ld a3,-120(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(f)
  	addi a0,a0,%lo(f)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	ld a3,-128(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
  ret
  fac:
  	addi sp,sp,-96
  	sd ra,88(sp)
  	sd s0,80(sp)
  	addi s0,sp,96
  	sd a1,-96(s0)
  	sd a0,-88(s0)
  	li t0,1
  ble a1,t0,.tag_anf_op_4
  	li t1,1
  	sub t2,a1,t1
  	sd t2,-32(s0)
  	ld a0,-88(s0)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,0
  call part_app
  	ld t2,-96(s0)
  	mul t1,t2,a0
  	sd a0,-40(s0)
  	mv a0,t1
  	j .tag_anf_op_4_t
  .tag_anf_op_4:
  	li a0,1
  .tag_anf_op_4_t:
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
  ret
  main2:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	li a2,0
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(fix)
  	addi a0,a0,%lo(fix)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a3,6
  	li a2,1
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
  	li t0,0
  	mv a0,t0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
  ret

  $ dune exec riscv64_instr_test < manytests/typed/006partial.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  anon_1:
  	addi sp,sp,-48
  	sd ra,32(sp)
  	sd s0,24(sp)
  	addi s0,sp,48
  	sd a1,-40(s0)
  	sd a0,-32(s0)
  	li t0,2
  	add t1,a1,t0
  	mv a0,t1
  	ld ra,32(sp)
  	ld s0,24(sp)
  	addi sp,sp,48
  ret
  anon_2:
  	addi sp,sp,-48
  	sd ra,32(sp)
  	sd s0,24(sp)
  	addi s0,sp,48
  	sd a1,-40(s0)
  	sd a0,-32(s0)
  	li t0,10
  	mul t1,a1,t0
  	mv a0,t1
  	ld ra,32(sp)
  	ld s0,24(sp)
  	addi sp,sp,48
  ret
  foo:
  	addi sp,sp,-96
  	sd ra,88(sp)
  	sd s0,80(sp)
  	addi s0,sp,96
  	sd a0,-96(s0)
  beqz a0,.tag_if_bnch
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a3,-96(s0)
  	li a2,1
  	li a1,2
  call part_app
  	j .tag_if_bnch_t
  .tag_if_bnch:
  	sd a0,-32(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	ld a3,-96(s0)
  	li a2,1
  	li a1,2
  call part_app
  .tag_if_bnch_t:
  	sd a0,-40(s0)
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
  ret
  foo_0:
  	addi sp,sp,-288
  	sd ra,272(sp)
  	sd s0,264(sp)
  	addi s0,sp,288
  	sd a0,-280(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,1
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,0
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,1
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,0
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-56(s0)
  	ld a0,-56(s0)
  	ld a3,-280(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	ld a0,-48(s0)
  	ld a3,-64(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-72(s0)
  	ld a0,-40(s0)
  	ld a3,-72(s0)
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-80(s0)
  	ld a0,-32(s0)
  	ld a3,-80(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,272(sp)
  	ld s0,264(sp)
  	addi sp,sp,288
  ret
  main2:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	lui a0,%hi(foo_0)
  	addi a0,a0,%lo(foo_0)
  	li a3,11
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret

  $ dune exec riscv64_instr_test < manytests/typed/006partial2.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  foo:
  	addi sp,sp,-176
  	sd ra,168(sp)
  	sd s0,160(sp)
  	addi s0,sp,176
  	sd a2,-176(s0)
  	sd a1,-168(s0)
  	sd a0,-160(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-160(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	sd a0,-40(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-168(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-176(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-56(s0)
  	ld a1,-168(s0)
  	ld a2,-176(s0)
  	mul t0,a1,a2
  	ld t1,-160(s0)
  	add t2,t1,t0
  	mv a0,t2
  	ld ra,168(sp)
  	ld s0,160(sp)
  	addi sp,sp,176
  ret
  main2:
  	addi sp,sp,-208
  	sd ra,200(sp)
  	sd s0,192(sp)
  	addi s0,sp,208
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,1
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-32(s0)
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a3,2
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	sd a0,-56(s0)
  	ld a0,-56(s0)
  	li a3,3
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-64(s0)
  	sd a0,-72(s0)
  	ld a0,-72(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-80(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-80(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-88(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,200(sp)
  	ld s0,192(sp)
  	addi sp,sp,208
  ret

  $ dune exec riscv64_instr_test < manytests/typed/006partial3.ml
  .attribute unaligned_access, 0
  .attribute stack_align, 16
  .global main
  main:
  	addi sp,sp,-32
  	sd ra,16(sp)
  	sd s0,8(sp)
  	sd s1,0(sp)
  	addi s0,sp,32
  call init_part_apps
  call main2
  	sd a0,24(sp)
  call cleanup_part_apps
  	ld a0,24(sp)
  	ld ra,16(sp)
  	ld s0,8(sp)
  	ld s1,0(sp)
  	addi sp,sp,32
  	li a7,93
  ecall
  anon_2:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a2,-72(s0)
  	sd a1,-64(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-72(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,64(sp)
  	ld s0,56(sp)
  	addi sp,sp,80
  ret
  anon_1:
  	addi sp,sp,-144
  	sd ra,128(sp)
  	sd s0,120(sp)
  	addi s0,sp,144
  	sd a1,-136(s0)
  	sd a0,-128(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-136(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	sd a0,-40(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	ld a3,-136(s0)
  	li a2,1
  	li a1,3
  call part_app
  	sd a0,-48(s0)
  	ld a0,-48(s0)
  	ld a3,-128(s0)
  	li a2,1
  	li a1,0
  call part_app
  	mv a0,a0
  	ld ra,128(sp)
  	ld s0,120(sp)
  	addi sp,sp,144
  ret
  foo:
  	addi sp,sp,-96
  	sd ra,88(sp)
  	sd s0,80(sp)
  	addi s0,sp,96
  	sd a0,-96(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-96(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	sd a0,-40(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a3,-96(s0)
  	li a2,1
  	li a1,2
  call part_app
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
  ret
  main2:
  	addi sp,sp,-128
  	sd ra,112(sp)
  	sd s0,104(sp)
  	addi s0,sp,128
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a3,4
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	ld a0,-32(s0)
  	li a3,8
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-40(s0)
  	li a3,9
  	li a2,1
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,112(sp)
  	ld s0,104(sp)
  	addi sp,sp,128
  ret

  $ dune exec riscv64_instr_test < manytests/typed/007order.ml
  : end_of_input

  $ dune exec riscv64_instr_test < manytests/typed/008ascription.ml
  : end_of_input

  $ dune exec riscv64_instr_test < manytests/typed/015tuples.ml
  : end_of_input

  $ dune exec riscv64_instr_test < manytests/typed/016lists.ml
  : end_of_input

