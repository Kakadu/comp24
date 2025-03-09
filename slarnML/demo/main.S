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
main:
	addi sp,sp,-80
	sd ra,72(sp)
	sd s0,64(sp)
	addi s0,sp,80
	lui a0,%hi(fac)
	addi a0,a0,%lo(fac)
	li a3,3
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
	mv a0,a0
	ld ra,72(sp)
	ld s0,64(sp)
	addi sp,sp,80
ret
