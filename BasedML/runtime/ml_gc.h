#pragma once
#include "ml_runtimelib.h"

#ifdef __riscv
    #include "riscv/platform_spec_gc.h"
    #define GC
#endif

#ifdef GC
typedef enum { COLOR_UNPROCESSED = 0, COLOR_PROCESSED = 1 } color_t;

    #define START_COLOR COLOR_UNPROCESSED

typedef void* (*malloc_f_t)(size_t);

void add_global_vars_to_gc(size_t n, ...);

void compact();

void print_gc_info();

malloc_f_t gc_malloc;
#endif