#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <ffi.h>

#define TAG_INT 0b000
#define TAG_BOOL 0b001
#define TAG_CLOSURE 0b010

struct vals
{
    int8_t tag;
    union
    {
        int32_t int_val;
        bool bool_val;
        struct closure *closure_val;
    } type_val;
};

struct closure
{
    void *fun_ptr;
    u_int32_t args_num;
    u_int32_t applied_args_num;
    struct vals **applied_args;
};

struct vals *create_int_val(int32_t value)
{
    struct vals *val = malloc(sizeof(struct vals));
    if (val == NULL)
    {
        fprintf(stderr, "Exception create $create_int_val$\n");
        exit(1);
    }

    val->tag = TAG_INT;
    val->type_val.int_val = value;
    return val;
}

struct vals *create_bool_val(bool value)
{
    struct vals *val = malloc(sizeof(struct vals));
    if (val == NULL)
    {
        fprintf(stderr, "Exception create $create_bool_val$\n");
        exit(1);
    }

    val->tag = TAG_BOOL;

    if (value == 1)
    {
        val->type_val.bool_val = true;
    }
    else
    {
        val->type_val.bool_val = false;
    }

    return val;
}

bool get_bool(struct vals *val)
{
    if (val == NULL)
    {
        fprintf(stderr, "Exception get $get_bool$\n");
        exit(1);
    }

    if (val->tag != TAG_BOOL)
    {
        fprintf(stderr, "Exception get $get_bool$\n");
        exit(1);
    }

    return val->type_val.bool_val;
}

struct vals *eq(struct vals *val_a, struct vals *val_b);


struct vals *mul(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $mul$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $mul$\n");
    }

    int32_t a = val_a->type_val.int_val;
    int32_t b = val_b->type_val.int_val;

    return create_int_val(a * b);
}

struct vals *divv(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $div$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $div$\n");
        exit(1);
    }

    int32_t a = val_a->type_val.int_val;
    int32_t b = val_b->type_val.int_val;

    if (b == 0)
    {
        fprintf(stderr, "Exception $div$ division by zero\n");
        exit(1);
    }

    return create_int_val(a / b);
}

struct vals *plus(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $plus$n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $plus$\n");
        exit(1);
    }

    int32_t a = val_a->type_val.int_val;
    int32_t b = val_b->type_val.int_val;

    return create_int_val(a + b);
}

struct vals *minus(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $minus$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $minus$\n");
        exit(1);
    }

    int32_t a = val_a->type_val.int_val;
    int32_t b = val_b->type_val.int_val;

    return create_int_val(a - b);
}

struct vals *eq(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $eq$\n");
        exit(1);
    }

    if (val_a->tag != val_b->tag)
    {
        return create_bool_val(false);
    }

    if (val_a->tag == TAG_INT)
    {
        return create_bool_val(val_a->type_val.int_val == val_b->type_val.int_val);
    }

    if (val_a->tag == TAG_BOOL)
    {
        return create_bool_val(val_a->type_val.bool_val == val_b->type_val.bool_val);
    }

    fprintf(stderr, "Exception $eq$\n");
    exit(1);
}

struct vals *neq(struct vals *val_a, struct vals *val_b)
{
    struct vals *res = eq(val_a, val_b);

    return create_bool_val(res->type_val.bool_val);
}

struct vals *less(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $less$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $less$\n");
        exit(1);
    }

    return create_bool_val(val_a->type_val.int_val < val_b->type_val.int_val);
}

struct vals *leq(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $leq$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $leq$\n");
        exit(1);
    }

    return create_bool_val(val_a->type_val.int_val <= val_b->type_val.int_val);
}

struct vals *gre(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $gre$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gre$\n");
        exit(1);
    }

    return create_bool_val(val_a->type_val.int_val > val_b->type_val.int_val);
}

struct vals *greq(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $greq$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $greq$\n");
        exit(1);
    }

    return create_bool_val(val_a->type_val.int_val >= val_b->type_val.int_val);
}

struct vals *or_(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $llor$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gt$\n");
        exit(1);
    }

    return create_bool_val(val_a->type_val.int_val || val_b->type_val.int_val);
}

struct vals *and_(struct vals *val_a, struct vals *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $lland$\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gt$\n");
        exit(1);
    }

    return create_bool_val(val_a->type_val.int_val && val_b->type_val.int_val);
}

void print_int(struct vals *num)
{
    if (num == NULL)
    {
        fprintf(stderr, "Exception $print_int$\n");
        exit(1);
    }

    if (num->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $print_int$\n");
        exit(1);
    }

    printf("%d\n", num->type_val.int_val);
}

struct vals *new_closure(void *fun_ptr, int32_t fun_args_num)
{
    struct closure *closure = malloc(sizeof(struct closure));
    if (closure == NULL)
    {
        fprintf(stderr, "Exception $new_closure$n");
        exit(1);
    }

    closure->fun_ptr = fun_ptr;
    closure->args_num = fun_args_num;
    closure->applied_args_num = 0;
    closure->applied_args = malloc(0);

    struct vals *closure_val = malloc(sizeof(struct vals));
    if (closure_val == NULL)
    {
        fprintf(stderr, "Exception $new_closure$n");
        exit(1);
    }

    closure_val->tag = TAG_CLOSURE;
    closure_val->type_val.closure_val = closure;

    return closure_val;
}

struct vals *call_closure(struct vals *closure_val)
{
    if (closure_val == NULL)
    {
        fprintf(stderr, "Exception $call_closure$\n");
        exit(1);
    }

    if (closure_val->tag != TAG_CLOSURE)
    {
        fprintf(stderr, "Exception $call_closure$\n");
        exit(1);
    }

    struct closure *closure = closure_val->type_val.closure_val;
    if (closure == NULL)
    {
        fprintf(stderr, "Exception $call_closure$n");
        exit(1);
    }

    size_t args_count = closure->args_num;

    if (closure->applied_args_num != args_count)
    {
        fprintf(stderr, "Exception $call_closure$\n");
        exit(1);
    }

    ffi_cif cif;
    ffi_type **ffi_args_types = malloc(closure->args_num * sizeof(ffi_type *));
    if (ffi_args_types == NULL)
    {
        fprintf(stderr, "Exception $call_closure$n");
        exit(1);
    }

    void **args_val = malloc(closure->args_num * sizeof(void *));
    if (args_val == NULL)
    {
        fprintf(stderr, "Exception $call_closure$n");
        exit(1);
    }

    for (int i = 0; i < closure->args_num; i++)
    {
        ffi_args_types[i] = &ffi_type_pointer;
        args_val[i] = &closure->applied_args[i];
    }

    if (ffi_prep_cif(
            &cif,
            FFI_DEFAULT_ABI,
            closure->args_num,
            &ffi_type_pointer,
            ffi_args_types) != FFI_OK)
    {
        fprintf(stderr, "Exception $call_closure$n");
        exit(1);
    }

    struct vals *call_res = malloc(sizeof(struct vals));
    if (call_res == NULL)
    {
        fprintf(stderr, "Exception $call_closure$n");
        exit(1);
    }

    ffi_call(&cif, closure->fun_ptr, &call_res, args_val);

    if (call_res == NULL)
    {
        fprintf(stderr, "Exception $call_closure$\n");
        exit(1);
    }

    return call_res;
}

struct vals *_apply_closure(struct vals *closure_val, int32_t args_num, struct vals **new_args) {
    if (closure_val == NULL)
    {
        fprintf(stderr, "Exception $app_closure$\n");
        exit(1);
    }

    if (closure_val->tag != TAG_CLOSURE)
    {
        fprintf(stderr, "Exception $app_closure$\n");
        exit(1);
    }

    struct vals *new_closuse_val = malloc(sizeof(struct vals));
    if (new_closuse_val == NULL)
    {
        fprintf(stderr, "Exception $app_closure$n");
        exit(1);
    }

    new_closuse_val->tag = TAG_CLOSURE;

    struct closure *old_closure = closure_val->type_val.closure_val;
    if (old_closure == NULL)
    {
        fprintf(stderr, "Exception $app_closure$\n");
        exit(1);
    }

    struct closure *new_closure = malloc(sizeof(struct closure));
    if (new_closure == NULL)
    {
        fprintf(stderr, "Exception $app_closure$n");
        exit(1);
    }
    new_closure->fun_ptr = old_closure->fun_ptr;
    new_closure->args_num = old_closure->args_num;
    if(old_closure->applied_args_num + args_num > new_closure->args_num) {
        new_closure->applied_args_num = new_closure->args_num;
    } else {
        new_closure->applied_args_num = old_closure->applied_args_num + args_num;
    }

    struct vals **new_applied_args = malloc(new_closure->applied_args_num * sizeof(struct vals *));
    if (new_applied_args == NULL)
    {
        fprintf(stderr, "Exception $app_closure$n");
        exit(1);
    }

    for (int i = 0; i < old_closure->applied_args_num; i++)
    {
        new_applied_args[i] = old_closure->applied_args[i];
    }

    for (int i = 0; i < new_closure->applied_args_num - old_closure->applied_args_num; i++)
    {
        new_applied_args[old_closure->applied_args_num + i] = new_args[i];
    }

    new_closure->applied_args = new_applied_args;

    new_closuse_val->type_val.closure_val = new_closure;

    if (new_closure->applied_args_num == new_closure->args_num)
    {
        struct vals *res = call_closure(new_closuse_val);
        if (old_closure->applied_args_num + args_num > new_closure->args_num) {
            int32_t updated_args_num = old_closure->applied_args_num + args_num - new_closure->applied_args_num;

            return _apply_closure(res, updated_args_num, &new_args[new_closure->applied_args_num - old_closure->applied_args_num]);
        }

        return res;
    }

    return new_closuse_val;
}

struct vals *app_closure(struct vals *closure_val, int32_t args_num, ...)
{
    va_list args;
    va_start(args, args_num);

    struct vals **new_args = malloc(args_num * sizeof(struct vals *));
    for (int i = 0; i < args_num; i++)
    {
        struct vals *arg = va_arg(args, struct vals *);
        if (arg == NULL)
        {
            fprintf(stderr, "Exception $app_closure$n");
            exit(1);
        }
        new_args[i] = arg;
    }

    va_end(args);

    return _apply_closure(closure_val, args_num, new_args);
}