  $ dune exec riscv64_instr_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
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
  anon_1:
  	addi sp,sp,-80
  	sd ra,72(sp)
  	sd s0,64(sp)
  	addi s0,sp,80
  	sd a2,-80(s0)
  	sd a1,-72(s0)
  	sd a0,-64(s0)
  	ld a0,-72(s0)
  	ld a3,-64(s0)
  	li a2,1
  	li a1,0
  call part_app
  	ld a1,-80(s0)
  	mul a2,a1,a0
  	mv a0,a2
  	ld ra,72(sp)
  	ld s0,64(sp)
  	addi sp,sp,80
  ret
  fack:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	sd a1,-152(s0)
  	sd a0,-144(s0)
  	li t0,1
  ble t0,a0,.tag_anf_op_3
  	lui a0,%hi(f)
  	addi a0,a0,%lo(f)
  	li a3,1
  	li a2,1
  	li a1,2
  call part_app
  	j .tag_anf_op_3_t
  .tag_anf_op_3:
  	ld t0,-144(s0)
  	li a1,1
  	sub t1,t0,a1
  	sd a0,-32(s0)
  	sd t1,-40(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a4,-152(s0)
  	ld a3,-144(s0)
  	li a2,2
  	li a1,3
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a4,-48(s0)
  	ld a3,-40(s0)
  	li a2,2
  	li a1,2
  call part_app
  .tag_anf_op_3_t:
  	sd a0,-56(s0)
  	mv a0,a0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
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
  fac:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	sd a0,-88(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a4,-32(s0)
  	ld a3,-88(s0)
  	li a2,2
  	li a1,2
  call part_app
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
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
  blt t0,a0,.tag_anf_op_1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
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
  .tag_anf_op_1_t:
  	mv a0,a0
  	ld ra,80(sp)
  	ld s0,72(sp)
  	addi sp,sp,96
  ret
  fac:
  	addi sp,sp,-64
  	sd ra,48(sp)
  	sd s0,40(sp)
  	addi s0,sp,64
  	sd a0,-56(s0)
  	lui a0,%hi(fack)
  	addi a0,a0,%lo(fack)
  	ld a3,-56(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,48(sp)
  	ld s0,40(sp)
  	addi sp,sp,64
  ret
  $ dune exec riscv64_instr_test << EOF
  > let f a =
  >   let g c d =
  >     let h e = a * (c + d * e) in
  >     (h 4)
  >   in
  >   (g 2 3)
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
  h:
  	addi sp,sp,-208
  	sd ra,192(sp)
  	sd s0,184(sp)
  	addi s0,sp,208
  	sd a3,-200(s0)
  	sd a2,-192(s0)
  	sd a1,-184(s0)
  	sd a0,-176(s0)
  	ld a0,-176(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-32(s0)
  	ld a0,-184(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-192(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-48(s0)
  	ld a0,-200(s0)
  	li a2,0
  	li a1,0
  call part_app
  	ld a1,-48(s0)
  	mul a2,a1,a0
  	ld a3,-40(s0)
  	add t0,a3,a2
  	ld t1,-32(s0)
  	mul t2,t1,t0
  	mv a0,t2
  	ld ra,192(sp)
  	ld s0,184(sp)
  	addi sp,sp,208
  ret
  g:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a2,-72(s0)
  	sd a1,-64(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(h)
  	addi a0,a0,%lo(h)
  	li a6,4
  	ld a5,-72(s0)
  	ld a4,-64(s0)
  	ld a3,-56(s0)
  	li a2,4
  	li a1,4
  call part_app
  	mv a0,a0
  	ld ra,64(sp)
  	ld s0,56(sp)
  	addi sp,sp,80
  ret
  f:
  	addi sp,sp,-64
  	sd ra,48(sp)
  	sd s0,40(sp)
  	addi s0,sp,64
  	sd a0,-56(s0)
  	lui a0,%hi(g)
  	addi a0,a0,%lo(g)
  	li a5,3
  	li a4,2
  	ld a3,-56(s0)
  	li a2,3
  	li a1,3
  call part_app
  	mv a0,a0
  	ld ra,48(sp)
  	ld s0,40(sp)
  	addi sp,sp,64
  ret
  $ dune exec riscv64_instr_test < manytests/do_not_type/001.ml
  fac not exist

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
  f not exist

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
  ble t0,a0,.tag_anf_op_1
  	li t1,1
  	mv a0,t1
  	j .tag_anf_op_1_t
  .tag_anf_op_1:
  	ld a0,-88(s0)
  	li t2,1
  	sub t3,a0,t2
  	sd t3,-32(s0)
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	ld a3,-32(s0)
  	li a2,1
  	li a1,1
  call part_app
  	ld t3,-88(s0)
  	mul t2,t3,a0
  	sd a0,-40(s0)
  	mv a0,t2
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
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	sd a1,-152(s0)
  	sd a0,-144(s0)
  	li t0,1
  beq a0,t0,.tag_anf_op_3
  	ld a0,-152(s0)
  	li a3,1
  	li a2,1
  	li a1,0
  call part_app
  	j .tag_anf_op_3_t
  .tag_anf_op_3:
  	ld t0,-144(s0)
  	li a1,1
  	sub t1,t0,a1
  	sd a0,-32(s0)
  	sd t1,-40(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	ld a4,-152(s0)
  	ld a3,-144(s0)
  	li a2,2
  	li a1,3
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(fac_cps)
  	addi a0,a0,%lo(fac_cps)
  	ld a4,-48(s0)
  	ld a3,-40(s0)
  	li a2,2
  	li a1,2
  call part_app
  .tag_anf_op_3_t:
  	sd a0,-56(s0)
  	mv a0,a0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
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
  	addi sp,sp,-128
  	sd ra,112(sp)
  	sd s0,104(sp)
  	addi s0,sp,128
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(fac_cps)
  	addi a0,a0,%lo(fac_cps)
  	ld a4,-32(s0)
  	li a3,4
  	li a2,2
  	li a1,2
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-40(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,112(sp)
  	ld s0,104(sp)
  	addi sp,sp,128
  ret

  $ dune exec riscv64_instr_test < manytests/typed/003fib.ml
  ab not found

  $ dune exec riscv64_instr_test < manytests/typed/004manyargs.ml
  rez not found

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
  	addi sp,sp,-96
  	sd ra,88(sp)
  	sd s0,80(sp)
  	addi s0,sp,96
  	sd a1,-96(s0)
  	sd a0,-88(s0)
  	lui a0,%hi(fix)
  	addi a0,a0,%lo(fix)
  	ld a3,-88(s0)
  	li a2,1
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(f)
  	addi a0,a0,%lo(f)
  	ld a4,-96(s0)
  	ld a3,-32(s0)
  	li a2,2
  	li a1,2
  call part_app
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
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
  ble a1,a0,.tag_anf_op_4
  	li t0,1
  	sd a0,-32(s0)
  	mv a0,t0
  	j .tag_anf_op_4_t
  .tag_anf_op_4:
  	ld a0,-192(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-40(s0)
  	ld a0,-192(s0)
  	li a2,0
  	li a1,0
  call part_app
  	li t0,1
  	sub a1,a0,t0
  	sd a0,-48(s0)
  	sd a1,-56(s0)
  	ld a0,-184(s0)
  	ld a3,-56(s0)
  	li a2,1
  	li a1,0
  call part_app
  	ld a1,-40(s0)
  	mul t0,a1,a0
  	sd a0,-64(s0)
  	mv a0,t0
  .tag_anf_op_4_t:
  	mv a0,a0
  	ld ra,184(sp)
  	ld s0,176(sp)
  	addi sp,sp,192
  ret
  main2:
  	addi sp,sp,-128
  	sd ra,112(sp)
  	sd s0,104(sp)
  	addi s0,sp,128
  	lui a0,%hi(fac)
  	addi a0,a0,%lo(fac)
  	li a2,0
  	li a1,2
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(fix)
  	addi a0,a0,%lo(fix)
  	li a4,6
  	ld a3,-32(s0)
  	li a2,2
  	li a1,2
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-40(s0)
  	li a2,1
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,112(sp)
  	ld s0,104(sp)
  	addi sp,sp,128
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
  	sd a0,-32(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	li a2,0
  	li a1,1
  call part_app
  	j .tag_if_bnch_t
  .tag_if_bnch:
  	sd a0,-40(s0)
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  .tag_if_bnch_t:
  	sd a0,-48(s0)
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
  ret
  foo_1:
  	addi sp,sp,-192
  	sd ra,176(sp)
  	sd s0,168(sp)
  	addi s0,sp,192
  	sd a0,-184(s0)
  	ld a0,-184(s0)
  	li a2,0
  	li a1,0
  call part_app
  	sd a0,-32(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-32(s0)
  	li a3,0
  	li a2,2
  	li a1,1
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-40(s0)
  	li a3,1
  	li a2,2
  	li a1,1
  call part_app
  	sd a0,-48(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-48(s0)
  	li a3,0
  	li a2,2
  	li a1,1
  call part_app
  	sd a0,-56(s0)
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	ld a4,-56(s0)
  	li a3,1
  	li a2,2
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,176(sp)
  	ld s0,168(sp)
  	addi sp,sp,192
  ret
  main2:
  	addi sp,sp,-96
  	sd ra,80(sp)
  	sd s0,72(sp)
  	addi s0,sp,96
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
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
  foo_1 not found

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
  	addi sp,sp,-64
  	sd ra,48(sp)
  	sd s0,40(sp)
  	addi s0,sp,64
  	sd a0,-56(s0)
  	lui a0,%hi(print_int)
  	addi a0,a0,%lo(print_int)
  	ld a3,-56(s0)
  	li a2,1
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,48(sp)
  	ld s0,40(sp)
  	addi sp,sp,64
  ret
  anon_1:
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
  	lui a0,%hi(anon_2)
  	addi a0,a0,%lo(anon_2)
  	li a2,0
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,88(sp)
  	ld s0,80(sp)
  	addi sp,sp,96
  ret
  foo:
  	addi sp,sp,-128
  	sd ra,120(sp)
  	sd s0,112(sp)
  	addi s0,sp,128
  	sd a0,-128(s0)
  	lui a0,%hi(a)
  	addi a0,a0,%lo(a)
  	li a2,0
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
  	sd a0,-48(s0)
  	lui a0,%hi(anon_1)
  	addi a0,a0,%lo(anon_1)
  	li a2,0
  	li a1,1
  call part_app
  	mv a0,a0
  	ld ra,120(sp)
  	ld s0,112(sp)
  	addi sp,sp,128
  ret
  main2:
  	addi sp,sp,-64
  	sd ra,48(sp)
  	sd s0,40(sp)
  	addi s0,sp,64
  	lui a0,%hi(foo)
  	addi a0,a0,%lo(foo)
  	li a5,9
  	li a4,8
  	li a3,4
  	li a2,3
  	li a1,1
  call part_app
  	sd a0,-32(s0)
  	li t0,0
  	mv a0,t0
  	ld ra,48(sp)
  	ld s0,40(sp)
  	addi sp,sp,64
  ret

  $ dune exec riscv64_instr_test < manytests/typed/007order.ml
  : end_of_input

  $ dune exec riscv64_instr_test < manytests/typed/008ascription.ml
  : end_of_input

  $ dune exec riscv64_instr_test < manytests/typed/015tuples.ml
  : end_of_input

  $ dune exec riscv64_instr_test < manytests/typed/016lists.ml
  : end_of_input

