set history save
set architecture riscv:rv64
set sysroot /usr/riscv64−linux−gnu
target remote :1234
tui enable
tui new-layout example {-horizontal regs 1 asm 1} 2 status 0 cmd 1
layout example
# tui disable
focus cmd
b _start
b .breakpoint0
b .breakpoint1
b .breakpoint2
b .breakpoint3
b .breakpoint4
b .breakpoint5
b .breakpoint6
b .breakpoint7
b .breakpoint8
b .breakpoint9
b print_int
# c
# x/8xg $sp