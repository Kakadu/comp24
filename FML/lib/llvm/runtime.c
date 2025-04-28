#include <ffi.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int64_t print_int(int64_t x)
{
    printf("%ld", x);
    return 0;
}

int64_t fail_match(int64_t err)
{
    fprintf(stderr, "Match failure\n");
    exit(err);
}

int64_t rt_add(int64_t x, int64_t y) { return x + y; }
int64_t rt_sub(int64_t x, int64_t y) { return x - y; }
int64_t rt_mul(int64_t x, int64_t y) { return x * y; }
int64_t rt_divd(int64_t x, int64_t y) { return x / y; }
int64_t rt_eq(int64_t x, int64_t y) { return x == y; }
int64_t rt_neq(int64_t x, int64_t y) { return x != y; }
int64_t rt_less(int64_t x, int64_t y) { return x < y; }
int64_t rt_leq(int64_t x, int64_t y) { return x <= y; }
int64_t rt_gre(int64_t x, int64_t y) { return x > y; }
int64_t rt_geq(int64_t x, int64_t y) { return x >= y; }
int64_t rt_and(int64_t x, int64_t y) { return x && y; }
int64_t rt_or(int64_t x, int64_t y) { return x || y; }

typedef struct
{
    int64_t fun_ptr;
    int64_t args_num;
    int64_t args_applied;
    int64_t args[];
} closure_t;

int64_t
new_closure(int64_t f_ptr, int64_t args_num)
{
    closure_t *closure = malloc(sizeof(closure_t) + args_num * sizeof(int64_t));
    closure->fun_ptr = f_ptr;
    closure->args_num = args_num;
    closure->args_applied = 0;
    return (int64_t)closure;
}

int64_t call_closure(closure_t *closure)
{
    int64_t args_n = closure->args_num;
    ffi_cif cif;
    ffi_type *args_types[args_n];
    void *args[args_n];
    for (int64_t i = 0; i < args_n; i++)
    {
        args_types[i] = &ffi_type_sint64;
        args[i] = &closure->args[i];
    }

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args_n, &ffi_type_sint64, args_types) != FFI_OK)
    {
        fprintf(stderr, "Failed to prepare CIF\n");
        exit(1);
    };

    int64_t res;

    ffi_call(&cif, (void *)closure->fun_ptr, &res, args);

    return res;
}

int64_t _apply(closure_t *closure, int new_args_num, va_list *args)
{
    int64_t args_to_apply = closure->args_num - closure->args_applied;

    if (new_args_num < args_to_apply)
    {
        for (int64_t i = 0; i < new_args_num; i++)
        {
            int64_t arg = va_arg(*args, int64_t);
            closure->args[closure->args_applied + i] = arg;
            closure->args_applied += 1;
        }
        return (int64_t)closure;
    }
    else
    {
        for (int64_t i = 0; i < args_to_apply; i++)
        {
            int64_t arg = va_arg(*args, int64_t);
            closure->args[closure->args_applied + i] = arg;
        }

        int64_t res = call_closure(closure);

        new_args_num -= args_to_apply;
        // free(closure);
        if (new_args_num == 0)
        {
            return res;
        }

        closure_t *new_closure = (closure_t *)res;
        return _apply(new_closure, new_args_num, args);
    }
}

int64_t apply_args(closure_t *closure, int new_args_num, ...)
{
    va_list args;
    va_start(args, new_args_num);

    va_end(args);
    return _apply(closure, new_args_num, &args);
}
