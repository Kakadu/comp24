#include "ml_runtimelib.h"
#include <ffi.h>
#include <stdarg.h>

int8_t is_ml_ptr(int64_t arg) { return arg & 1; };

#ifndef GC
    #define START_COLOR 0
#endif

void* ml_malloc(size_t size) {
#ifdef GC
    // some GC_code
#else
    return malloc(size);
#endif
};

box_t* create_box_t(size_t size) {
    if (size % 8 != 0)
        size += 8 - (size % 8);

    box_t* res_box = ml_malloc(size);
    res_box->header.color = START_COLOR;
    res_box->header.size = size / 8;
    return res_box;
}

tag_t get_tag(int64_t obj) {
    if (is_ml_ptr(obj)) {
        return ((box_t*)obj)->header.tag;
    } else {
        return T_UNBOXED;
    }
}

typedef struct {
    int64_t val;
    int64_t next;
} cons_t;

int64_t mlrt_create_cons(int64_t data, int64_t next) {
    box_t* cons_box = create_box_t(sizeof(box_header_t) + sizeof(cons_t));
    cons_box->header.tag = T_CONS;
    cons_t* cons = (cons_t*)(&(cons_box->values));
    cons->val = data;
    cons->next = next;
    return (int64_t)cons_box;
}

int64_t mlrt_create_tuple(int64_t tuple_size, ...) {
    va_list tuple_elems;
    va_start(tuple_elems, tuple_size);

    box_t* tuple_box = create_box_t(sizeof(box_header_t) + sizeof(tuple_size * 8));
    tuple_box->header.tag = T_TUPLE;

    for (int i = 0; i < tuple_size; i++)
        tuple_box->values[i] = va_arg(tuple_elems, int64_t);

    va_end(tuple_elems);

    return (int64_t)tuple_box;
}

typedef struct {
    int64_t fun;
    int64_t args_num;
    int64_t args_applied;
    int64_t applied_args[];
} closure_t;

int64_t mlrt_create_empty_closure(int64_t fun, int64_t args_num) {

    box_t* closure_box = create_box_t(sizeof(box_header_t) + 0x18);
    closure_box->header.tag = T_CLOSURE;

    closure_t* clos = (closure_t*)&closure_box->values;
    clos->fun = fun;
    clos->args_applied = 0;
    clos->args_num = args_num;

    return (int64_t)closure_box;
}

int64_t create_closure_by_src(box_t* src_box, int64_t new_args_num, va_list new_args) {
    DEBUG_RUN(printf("## [START] create_closure_by_src()\n"););
    box_t* closure_box = create_box_t((src_box->header.size + new_args_num) * 8);
    closure_box->header.tag = T_CLOSURE;

    closure_t* src_clos = (closure_t*)&src_box->values;

    closure_t* clos = (closure_t*)&closure_box->values;
    clos->fun = src_clos->fun;
    clos->args_applied = src_clos->args_applied + new_args_num;
    clos->args_num = src_clos->args_num;

    for (int i = 0; i < src_clos->args_applied + new_args_num; i++) {
        if (i < src_clos->args_applied)
            clos->applied_args[i] = src_clos->applied_args[i];
        else
            clos->applied_args[i] = va_arg(new_args, int64_t);
        DEBUG_RUN(printf("## applied_args[%d] = %ld\n", i, clos->applied_args[i]););
    }

    DEBUG_RUN(printf("## [END] create_closure_by_src()\n\n"););
    return (int64_t)closure_box;
}

int64_t call_closure(box_t* closure_box, int64_t new_args_num, va_list new_args) {
    DEBUG_RUN(printf("## [START] call_closure()\n"););

    closure_t* closure = (closure_t*)&closure_box->values;
    size_t args_count = closure->args_num;

    ffi_cif cif;
    ffi_type* arg_types[args_count];
    int64_t* args[args_count];

    int64_t buffer_new_args[new_args_num];

    // Set up argument types (all are int64_t)
    for (int i = 0; i < args_count; ++i) {
        arg_types[i] = &ffi_type_sint64;
        if (i < closure->args_applied)
            args[i] = &(closure->applied_args[i]);
        else {
            int na_num = i - closure->args_applied;
            buffer_new_args[na_num] = va_arg(new_args, int64_t);
            args[i] = &(buffer_new_args[na_num]);
        }
        DEBUG_RUN(printf("## *(args[%d]) = %ld\n", i, *(args[i])););
    }

    int64_t res = 0;

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args_count, &ffi_type_sint64, arg_types) == FFI_OK) {
        ffi_call(&cif, (void*)closure->fun, &res, (void*)args);
    } else {
        EXCEPTION_FMT("call_closure: Failed to prepare call interface\n");
    }

    DEBUG_RUN(printf("## [END] call_closure()\n\n"););
    return res;
}

int64_t apply_args_to_closure(box_t* closure_box, int64_t new_args_num, va_list new_args) {
    closure_t* closure = (closure_t*)&closure_box->values;
    int64_t args_num_until_apply = closure->args_num - closure->args_applied;

    if (args_num_until_apply <= new_args_num) {
        int64_t call_res = call_closure(closure_box, args_num_until_apply, new_args);
        new_args_num -= args_num_until_apply;
        if (new_args_num == 0)
            return call_res;
        else {
            return apply_args_to_closure((box_t*)call_res, new_args_num, new_args);
        }
    } else {
        return create_closure_by_src(closure_box, new_args_num, new_args);
    }
}

int64_t mlrt_apply_args_to_closure(int64_t closure_box, int64_t new_args_num, ...) {
    va_list new_args;
    va_start(new_args, new_args_num);
    int64_t res = apply_args_to_closure((box_t*)closure_box, new_args_num, new_args);
    va_end(new_args);
    return res;
}

int64_t mlrt_get_box_field(int64_t box, int64_t field_num) {
    DEBUG_RUN(if (!is_ml_ptr(box)) EXCEPTION_FMT("mlrt_get_box_field: get unboxed value (%lx) as box\n", box););
    return ((box_t*)box)->values[field_num];
}

int64_t mlrt_check_tag(int64_t target, int64_t tag) { return CONVERT_INT_NATIVE_TO_ML(tag == get_tag(target)); }

void mltr_match_error() { EXCEPTION_FMT("Exception: Match_failure"); }