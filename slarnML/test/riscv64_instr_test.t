  $ dune exec riscv64_instr_test << EOF
  > let fac n = 
  >   let rec fack n f = if (n <= 1) then (f 1) else (fack (n - 1) (fun x -> x * (f n))) in
  >   (fack n (fun x -> x))
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
  anon_1_fack_fac:
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
  fack_fac:
  	addi sp,sp,-160
  	sd ra,144(sp)
  	sd s0,136(sp)
  	addi s0,sp,160
  	sd a1,-152(s0)
  	sd a0,-144(s0)
  	li t0,1
  ble t0,a0,.tag_anf_op_3
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
  	sd a0,-24(s0)
  	sd t1,-32(s0)
  	lui a0,%hi(anon_1_fack_fac)
  	addi a0,a0,%lo(anon_1_fack_fac)
  	ld a4,-152(s0)
  	ld a3,-144(s0)
  	li a2,2
  	li a1,3
  call part_app
  	sd a0,-40(s0)
  	lui a0,%hi(fack_fac)
  	addi a0,a0,%lo(fack_fac)
  	ld a4,-40(s0)
  	ld a3,-32(s0)
  	li a2,2
  	li a1,2
  call part_app
  .tag_anf_op_3_t:
  	sd a0,-48(s0)
  	mv a0,a0
  	ld ra,144(sp)
  	ld s0,136(sp)
  	addi sp,sp,160
  ret
  anon_2_fac:
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
  	lui a0,%hi(anon_2_fac)
  	addi a0,a0,%lo(anon_2_fac)
  	li a2,0
  	li a1,1
  call part_app
  	sd a0,-24(s0)
  	lui a0,%hi(fack_fac)
  	addi a0,a0,%lo(fack_fac)
  	ld a4,-24(s0)
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
  fack_fac:
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
  	lui a0,%hi(fack_fac)
  	addi a0,a0,%lo(fack_fac)
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
  	addi sp,sp,-64
  	sd ra,48(sp)
  	sd s0,40(sp)
  	addi s0,sp,64
  	sd a0,-56(s0)
  	lui a0,%hi(fack_fac)
  	addi a0,a0,%lo(fack_fac)
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
  h_g_f:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a3,-72(s0)
  	sd a2,-64(s0)
  	sd a1,-56(s0)
  	sd a0,-48(s0)
  	mul t0,a2,a3
  	add t1,a1,t0
  	mul t2,a0,t1
  	mv a0,t2
  	ld ra,64(sp)
  	ld s0,56(sp)
  	addi sp,sp,80
  ret
  g_f:
  	addi sp,sp,-80
  	sd ra,64(sp)
  	sd s0,56(sp)
  	addi s0,sp,80
  	sd a2,-72(s0)
  	sd a1,-64(s0)
  	sd a0,-56(s0)
  	lui a0,%hi(h_g_f)
  	addi a0,a0,%lo(h_g_f)
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
  	lui a0,%hi(g_f)
  	addi a0,a0,%lo(g_f)
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
