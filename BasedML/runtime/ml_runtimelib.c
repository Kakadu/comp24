#include "ml_runtimelib.h"

int8_t is_ml_ptr(int64_t arg) { return arg & 1; };

void* heap_start;
void* free_heap_ptr;
void* heap_end;

int8_t is_inside_heap(int64_t ptr) {
    return (((uint64_t)heap_start <= (uint64_t)ptr) && ((uint64_t)ptr < (uint64_t)heap_end));
};
box_t* get_box_t(int64_t ptr) { return (box_t*)ptr; }