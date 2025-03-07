#pragma once

#include <stdint.h>

#ifndef __riscv
    #error "Expect riscv as compilation target"
#endif

#define GC

void __attribute__((naked)) save_callee_saved_registers();
void __attribute__((naked)) restore_callee_saved_registers();
int64_t __attribute__((naked)) get_stack_pointer();