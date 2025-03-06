#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ffi.h>

typedef struct closure_t {
    int64_t fun;
    int64_t args_num;
    int64_t args_applied;
    int64_t applied_args[];
} closure_t;


closure_t* create_closure(int64_t fun, int64_t args_num, int64_t args_applied);

int64_t create_closure_by_src(closure_t* src_clos, int64_t new_args_num, va_list* new_args);

int64_t call_closure(closure_t* closure, int64_t new_args_num, va_list* new_args);

int64_t _apply_args_to_closure(closure_t* closure, int64_t new_args_num, va_list* new_args);

int64_t apply_args_to_closure(int64_t closure_ptr, int64_t new_args_num, ...);

int64_t print_int(int64_t x);

int64_t print_bool(int64_t x);

