#include "ml_gc.h"
#include <cstdint>
#include <set>

#ifdef GC
    #define START_POOL_SIZE 0x100

box_t** stack_bottom = NULL;
box_t** stack_top;

box_t*** global_vars = NULL;
box_t*** global_vars_end = NULL;

typedef struct _pool_st {
    uint8_t* pool_bottom;
    uint8_t* pool_pointer;
    uint8_t* pool_top;
    std::set<box_t*> used_addresses;
} pool_t;

pool_t* the_pool;

    #define POOL_MALLOC(pool, sz)                                                                                      \
        ({                                                                                                             \
            void* res = pool->pool_pointer;                                                                            \
            pool->pool_pointer += sz;                                                                                  \
            DEBUG_RUN(printf("[CR]: Added something with address %lx\n", (int64_t)res); print_gc_info();               \
                      fflush(stdout););                                                                                \
                                                                                                                       \
            pool->used_addresses.insert((box_t*)res);                                                                  \
            res;                                                                                                       \
        })

pool_t* create_pool_t(size_t sz) {
    uint8_t* start = (uint8_t*)malloc(sz);
    pool_t* pool = (pool_t*)malloc(sizeof(*pool));
    pool->pool_bottom = start;
    pool->pool_pointer = start;
    pool->pool_top = start + sz;
    pool->used_addresses = std::set<box_t*>();

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
    DEBUG_RUN(printf("[PROC]: Try process %lx: ", old_box); fflush(stdout););

    if (!is_inside_pool(old_pool, old_box)) {
        DEBUG_RUN(printf("NO\n"); fflush(stdout););
        return old_box;
    } else {

        DEBUG_RUN(printf("YES\n"); fflush(stdout););
    #undef DEBUG
        uint64_t orig_address = (uint64_t)old_box;
        old_box = *(--(old_pool->used_addresses.upper_bound(old_box)));
        uint64_t offset = orig_address - (uint64_t)old_box;

        if (old_box->header.color == COLOR_PROCESSED) {
            return (box_t*)old_box->values[0] + offset; // new box address
        } else {

            box_t* res = (box_t*)POOL_MALLOC(the_pool, old_box->header.size * 8);
            res->header = old_box->header;

            int64_t fst_value_buf = old_box->values[0];
            
            // number of elems in box = header.size-1
            for (int i = 0; i < old_box->header.size - 1; i++) {
                if (i == 0) {
                    old_box->header.color = COLOR_PROCESSED;
                    old_box->values[0] = (int64_t)res; // set new box address
                    res->values[i] = (int64_t)process_node((box_t*)fst_value_buf, old_pool);
                } else {
                    res->values[i] = (int64_t)process_node((box_t*)old_box->values[i], old_pool);
                }
            }

            DEBUG_RUN(if (old_box->header.tag == (int8_t)T_CLOSURE) {
                printf("Was | Became \n");
                printf("func: %lx  | %lx\n", fst_value_buf, res->values[0]);
                printf("num: %lx  | %lx\n", old_box->values[1], res->values[1]);
                printf("applied: %lx  | %lx\n", old_box->values[2], res->values[2]);
                for (int i = 3; i < old_box->header.size - 1; i++) {
                    printf("args[%d]: %lx  | %lx\n", i, old_box->values[i], res->values[i]);
                }
            });

            return (box_t*)((uint64_t)res + offset);
        }
    }
}

void realloc_the_pool(size_t sz) {
    DEBUG_RUN(printf("[GC] ---- REALOC!!\n"); fflush(stdout););
    save_callee_saved_registers();
    stack_top = (box_t**)get_stack_pointer();
    pool_t* old_pool = the_pool;
    the_pool = create_pool_t(sz);

    fflush(stdout);
    for (box_t** iter = stack_top; iter < stack_bottom; iter++) {
        *iter = process_node(*iter, old_pool);
    }
    for (box_t*** iter = global_vars; iter < global_vars_end; iter++) {
        **iter = process_node(**iter, old_pool);
    }
    free_pool_t(old_pool);
    restore_callee_saved_registers();
}

void* safe_malloc(size_t sz) {
    DEBUG_RUN(printf("[GC] ---- Safe malloc!! sz = %lx\n", sz); fflush(stdout););

    if ((the_pool->pool_pointer + sz) <= the_pool->pool_top) {
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

void gc_on_load() {
    stack_bottom = (box_t**)get_stack_pointer();
    the_pool = create_pool_t(START_POOL_SIZE);
}

void* _init_gc_malloc(size_t sz) {
    DEBUG_RUN(printf("[GC] ---- GC IS ALIVE\n"););
    // stack_bottom = (box_t**)get_stack_pointer() + 0x100;
    // the_pool = create_pool_t(START_POOL_SIZE);
    gc_malloc = (malloc_f_t)safe_malloc;
    return gc_malloc(sz);
}

malloc_f_t gc_malloc = _init_gc_malloc;

void add_global_vars_to_gc(int64_t n, va_list globs) {
    global_vars = (box_t***)calloc(n, sizeof(*global_vars));
    global_vars_end = global_vars + n;
    for (int i = 0; i < n; i++) {
        global_vars[i] = (box_t**)va_arg(globs, void*);
    }
}

void compact() {
    size_t sz = the_pool->pool_top - the_pool->pool_bottom;
    realloc_the_pool(sz);
}

void print_gc_info() {
    size_t used = the_pool->pool_pointer - the_pool->pool_bottom;
    size_t all = the_pool->pool_top - the_pool->pool_bottom;
    printf("Memory used: %#8lx/%#8lx\n", used, all);
    DEBUG_RUN(printf("Bottom: %#8lx |  Ptr: %#8lx | Top: %#8lx\n", (int64_t)the_pool->pool_bottom,
                     (int64_t)the_pool->pool_pointer, (int64_t)the_pool->pool_top);
              printf("Stack bot: %lx | cur: %lx\n", stack_bottom, get_stack_pointer()););
}

#endif