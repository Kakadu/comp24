#include "platform_spec_gc.h"

void __attribute__((naked)) save_callee_saved_registers() {
    __asm__ volatile("addi sp, sp, -96\n"

                     "sd s0, 0(sp)\n"
                     "sd s1, 8(sp)\n"
                     "sd s2, 16(sp)\n"
                     "sd s3, 24(sp)\n"
                     "sd s4, 32(sp)\n"
                     "sd s5, 40(sp)\n"
                     "sd s6, 48(sp)\n"
                     "sd s7, 56(sp)\n"
                     "sd s8, 64(sp)\n"
                     "sd s9, 72(sp)\n"
                     "sd s10, 80(sp)\n"
                     "sd s11, 88(sp)\n"
                     "ret");
}

void __attribute__((naked)) restore_callee_saved_registers() {
    __asm__ volatile("ld s0, 0(sp)\n"
                     "ld s1, 8(sp)\n"
                     "ld s2, 16(sp)\n"
                     "ld s3, 24(sp)\n"
                     "ld s4, 32(sp)\n"
                     "ld s5, 40(sp)\n"
                     "ld s6, 48(sp)\n"
                     "ld s7, 56(sp)\n"
                     "ld s8, 64(sp)\n"
                     "ld s9, 72(sp)\n"
                     "ld s10, 80(sp)\n"
                     "ld s11, 88(sp)\n"

                     "addi sp, sp, 96\n"
                     "ret");
}

int64_t __attribute__((naked)) get_stack_pointer() {
    __asm__ volatile("mv a0, sp\n"
                     "ret");
}
