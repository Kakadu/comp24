#include <inttypes.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <ffi.h>

typedef int64_t i64;
typedef uint64_t u64;

typedef struct {
    void *func;

    u64 argc;
    u64 argc_applied;

    void *argv[];
} neml_closure;

#define max(x, y) x ? x > y : y
#define err(fmt, ...) fprintf(stderr, "err: " fmt "\n", ##__VA_ARGS__)

void *neml_create_closure(void *func, u64 argc)
{
    neml_closure *closure =
        calloc(1, sizeof(*closure) + argc * sizeof(void *));
    if (!closure) {
        err("failed to allocate closure");
        return NULL;
    }

    closure->func = func;
    closure->argc = argc;

    return closure;
}

static void destroy_closure(neml_closure *closure)
{
    free(closure);
}

static neml_closure *copy_closure(neml_closure *src)
{
    size_t sz = sizeof(*src) + src->argc * sizeof(void *);

    neml_closure *copy = calloc(1, sz);
    if (!copy) {
        err("failed to allocate copy of closure");
        return NULL;
    }

    memcpy(copy, src, sz);
    return copy;
}

static void *apply_closure(neml_closure *closure, const u64 argc, va_list vargs)
{
    neml_closure *copy = copy_closure(closure);
    if (!copy) {
        return NULL;
    }

    for (u64 i = 0; i < argc; i++) {
        copy->argv[copy->argc_applied++] = va_arg(vargs, void *);
        if (copy->argc_applied == copy->argc) {
            break;
        }
    }

    if (copy->argc_applied == copy->argc) {
        ffi_type *atypes[copy->argc];
        void *argv[copy->argc];

        for (u64 i = 0; i < copy->argc; i++) {
            atypes[i] = &ffi_type_sint64;
            argv[i] = &copy->argv[i];
        }

        ffi_cif cif = { 0 };
        if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, copy->argc,
                 &ffi_type_sint64, atypes)) {
            err("ffi_pre_cif failed");
            return NULL;
        };

        void *r = NULL;
        ffi_call(&cif, copy->func, &r, argv);

        if (argc > copy->argc) {
            r = apply_closure(r, argc - copy->argc, vargs);
        }

        destroy_closure(copy);
        return r;
    }

    return copy;
}

void *neml_apply_closure(neml_closure *closure, u64 argc, ...)
{
    va_list vargs;
    va_start(vargs, argc);

    void *r = apply_closure(closure, argc, vargs);

    va_end(vargs);
    return r;
}

u64 neml_print_int(i64 value)
{
    printf("%ld\n", value);
    return 0;
}
