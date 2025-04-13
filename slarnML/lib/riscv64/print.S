.global print_char
.global print_int

print_char:
    addi    sp,sp,-32
    sd      ra,24(sp)
    sd      s0,16(sp)
    addi    s0,sp,32

    sd      a0,-24(s0)
    addi    a1,s0,-24
    li      a0, 1
    li      a7, 64
    li      a2, 1
    ecall

    ld      ra,24(sp)
    ld      s0,16(sp)
    addi    sp,sp,32
    ret

print_uint:
    addi    sp,sp,-64
    sd      ra,56(sp)
    sd      s0,48(sp)
    addi    s0,sp,64

    li      t0,10   # const
    li      t1, 8
    li      a2, 0   # buffer_size
    addi    s1,s0,-24
    
    .loop1:
        li      a4,0
        li      t2,0
        .loop2:
            rem     a5,a0,t0
            addi    a5,a5,48
            slli    a4,a4, 8
            add     a4,a4,a5
            addi    t2,t2, 1
            div     a0,a0,t0

            beq     t1,t2, .end_loop2
            beqz    a0, .end_loop2
        j .loop2
        .end_loop2:
        sd      a4,0(s1)
        addi    a2,a2, 8
        addi    s1,s1,-8
        beqz    a0, .end_loop1
    j .loop1
    .end_loop1:

    li      a0, 1
    addi    a1,s1,8 # &
    li      a7, 64  # write
    ecall


    ld      ra,56(sp)
    ld      s0,48(sp)
    addi    sp,sp,64
    ret                    # make the syscall

print_int:
    addi    sp,sp,-48
    sd      ra,40(sp)
    sd      s0,32(sp)
    addi    s0,sp,48

    bge     a0,zero, .posit
    li      a3,-1
    mul     a0,a0,a3
    sd      a0,-24(s0)
    li      a0,45     # '-'
    call    print_char

    ld      a0,-24(s0)
    .posit:
    call print_uint

    ld      ra,40(sp)
    ld      s0,32(sp)
    addi    sp,sp,48
    ret
