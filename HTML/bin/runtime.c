#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ffi.h>
#include "runtime.h"

closure_t* create_closure(int64_t fun, int64_t args_num, int64_t args_applied) {
    size_t size = sizeof(closure_t) + args_num * sizeof(int64_t);
    closure_t* clos = (closure_t*)malloc(size);
    if (clos == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    clos->fun = fun;
    clos->args_num = args_num;
    clos->args_applied = args_applied;
    return clos;
}

int64_t create_closure_by_src(closure_t* src_clos, int64_t new_args_num, va_list* new_args) {
    int64_t total_args_applied = src_clos->args_applied + new_args_num;
    closure_t* new_clos = create_closure(src_clos->fun, src_clos->args_num, total_args_applied);

    for (int i = 0; i < src_clos->args_applied; i++) {
        new_clos->applied_args[i] = src_clos->applied_args[i];
    }

    for (int i = 0; i < new_args_num; i++) {
        new_clos->applied_args[src_clos->args_applied + i] = va_arg(*new_args, int64_t);
    }

    return (int64_t)new_clos;
}

int64_t call_closure(closure_t* closure, int64_t new_args_num, va_list* new_args) {
    size_t args_count = closure->args_num;

    ffi_cif cif;
    ffi_type* arg_types[args_count];
    int64_t* args[args_count];

    int64_t buffer_new_args[new_args_num];

    for (int i = 0; i < args_count; ++i) {
        arg_types[i] = &ffi_type_sint64;
        if (i < closure->args_applied) {
            args[i] = &(closure->applied_args[i]);
        }
        else {
            int na_num = i - closure->args_applied;
            buffer_new_args[na_num] = va_arg(*new_args, int64_t);
            args[i] = &(buffer_new_args[na_num]);
        }
    }

    int64_t res = 0;

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args_count, &ffi_type_sint64, arg_types) == FFI_OK) {
        ffi_call(&cif, (void*)closure->fun, &res, (void**)args);
    }
    else {
        fprintf(stderr, "call_closure: Failed to prepare call interface\n");
        exit(EXIT_FAILURE);
    }

    return res;
}

int64_t _apply_args_to_closure(closure_t* closure, int64_t new_args_num, va_list* new_args) {
    int64_t args_num_until_apply = closure->args_num - closure->args_applied;

    if (args_num_until_apply <= new_args_num) {
        int64_t call_res = call_closure(closure, args_num_until_apply, new_args);
        new_args_num -= args_num_until_apply;
        if (new_args_num == 0) {
            return call_res;
        }
        else {
            return _apply_args_to_closure((closure_t*)call_res, new_args_num, new_args);
        }
    }
    else {
        return create_closure_by_src(closure, new_args_num, new_args);
    }
}

int64_t apply_args_to_closure(int64_t closure_ptr, int64_t new_args_num, ...) {
    va_list new_args;
    va_start(new_args, new_args_num);
    int64_t res = _apply_args_to_closure((closure_t*)closure_ptr, new_args_num, &new_args);
    va_end(new_args);
    return res;
}

int64_t print_int(int64_t x)
{
    printf("%ld\n", x);
    return 0;
}

int64_t print_bool(int64_t x)
{
    if (x == 0)
    {
        printf("false\n");
    }
    else
    {
        printf("true\n");
    }
    return 0;
}

int64_t RTE_ERROR_MATCH_FAILURE(int64_t _) {
    fprintf(stderr, "Match failure\n");
    exit(EXIT_FAILURE);
    return 0;
}
int main() { // TODO: remove it 
    return 0;
}