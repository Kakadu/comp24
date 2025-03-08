#include "ml_gc.h"
#include "stdarg.h"

#ifdef GC
    #define START_POOL_SIZE 0x100

box_t** stack_bottom = NULL;
box_t** stack_top;

box_t*** global_vars;
box_t*** global_vars_end;

typedef struct _pool_st {
    void* pool_bottom;
    void* pool_pointer;
    void* pool_top;
} pool_t;

pool_t* the_pool;

    #define POOL_MALLOC(pool, sz)                                                                                      \
        ({                                                                                                             \
            void* res = pool->pool_pointer;                                                                            \
            pool->pool_pointer += sz;                                                                                  \
            res;                                                                                                       \
        })

pool_t* create_pool_t(size_t sz) {
    void* start = malloc(sz);
    pool_t* pool = malloc(sizeof(pool));
    pool->pool_bottom = start;
    pool->pool_pointer = start;
    pool->pool_top = start + sz;
    return pool;
}

void free_pool_t(pool_t* pool) {
    free(pool->pool_bottom);
    free(pool);
}

uint8_t is_inside_pool(pool_t* pool, box_t* some_box) {
    if (is_ml_ptr((int64_t)some_box))
        if ((pool->pool_bottom <= (void*)some_box) && ((void*)some_box < pool->pool_top))
            return 1;

    return 0;
}

box_t* process_node(box_t* old_box, pool_t* old_pool) {
    if (!is_inside_pool(old_pool, old_box)) {
        return old_box;
    } else {
        if (old_box->header.color == COLOR_PROCESSED) {
            return (box_t*)old_box->values[0]; // new box address
        } else {
            box_t* res = POOL_MALLOC(the_pool, old_box->header.size * 8);
            res->header = old_box->header;

            // number of elems in box = header.size-1
            for (int i = 0; i < old_box->header.size - 1; i++) {
                res->values[i] = (int64_t)process_node((box_t*)old_box->values[i], old_pool);
            }
            old_box->header.color = COLOR_PROCESSED;
            old_box->values[0] = (int64_t)res; // set new box address
            return res;
        }
    }
}

void realloc_the_pool(size_t sz) {
    save_callee_saved_registers();
    stack_bottom = (box_t**)get_stack_pointer();
    pool_t* old_pool = the_pool;
    the_pool = create_pool_t(sz);

    for (box_t** iter = stack_bottom; iter < stack_top; iter++) {
        *iter = process_node(*iter, old_pool);
    }
    for (box_t*** iter = global_vars; iter < global_vars_end; iter++) {
        **iter = process_node(**iter, old_pool);
    }
    free_pool_t(old_pool);
    restore_callee_saved_registers();
}

void* safe_malloc(size_t sz) {
    if (the_pool->pool_pointer + sz <= the_pool->pool_top) {
        return POOL_MALLOC(the_pool, sz);
    } else {
        size_t cur_sz = (size_t)(the_pool->pool_top - the_pool->pool_bottom);
        size_t needed_sz = sz + (size_t)(the_pool->pool_pointer - the_pool->pool_bottom);
        while (cur_sz < needed_sz) {
            cur_sz <<= 1;
        }
        realloc_the_pool(cur_sz);
        return POOL_MALLOC(the_pool, sz);
    }
}
void* _init_gc_malloc(size_t sz) {
    stack_bottom = (box_t**)get_stack_pointer();
    the_pool = create_pool_t(START_POOL_SIZE);
    gc_malloc = (malloc_f_t)safe_malloc;
    return gc_malloc(sz);
}

malloc_f_t gc_malloc = _init_gc_malloc;

void add_global_vars_to_gc(size_t n, ...) {
    va_list globs;
    va_start(globs, n);
    global_vars = calloc(n, sizeof(*global_vars));
    global_vars_end = global_vars + n;
    for (int i = 0; i < n; i++) {
        global_vars[i] = va_arg(globs, void*);
    }
    va_end(globs);
}

void compact() {
    size_t sz = the_pool->pool_top - the_pool->pool_bottom;
    realloc_the_pool(sz);
}

void print_gc_info() {
    size_t used = the_pool->pool_pointer - the_pool->pool_bottom;
    size_t all = the_pool->pool_top - the_pool->pool_bottom;
    printf("Memory used: %8lx/%8lx", used, all);
}

#endif