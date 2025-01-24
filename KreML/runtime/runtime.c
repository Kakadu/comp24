#include<stdio.h>
#include<malloc.h>
#include<stdint.h>
#include<inttypes.h>


typedef struct
{
    int64_t (*fptr)(int64_t*, int64_t);
    int64_t* env;
    int64_t applied_count;
    int64_t arity;
} closure;

void print_int(int64_t n) {
    printf("%" PRId64 "\n", n);
    return;
}

int64_t* alloc_tuple(int64_t count) {
    return malloc(sizeof(int_fast64_t) * count);
}

int64_t* alloc_closure(int64_t (*fptr)(int64_t*, int64_t), int64_t* env, int64_t arity) {
    closure* closure = malloc(sizeof(closure));
    closure->fptr = fptr;
    closure->env = env;
    closure->arity  = arity;
    closure->applied_count = 0;
    return (int64_t*)closure;
}

int64_t call_closure(closure* c, int64_t* args, int64_t args_count) {
    int64_t res;
    int64_t* curr_arg = args;
    closure  *curr_c = c;
    for (size_t i = 0; i < args_count; i++)
    {
        if (curr_c->applied_count + 1 == curr_c-> arity) {
            
            int64_t call_res = curr_c->fptr(curr_c->env, *curr_arg);
            curr_c = (closure*)call_res;
        }
        else {
            curr_c->env[curr_c->applied_count++] = *curr_arg;
        }
        curr_arg++;
    }
    return (int64_t)curr_c;
}


int main() {
    print_int(0);
}