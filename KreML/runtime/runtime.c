#include <stdio.h>
#include <malloc.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>


typedef struct
{
    int64_t (*fptr)(void *);
    int64_t *env;
    int64_t applied_count;
    int64_t arity;
    int64_t envsize;
} closure;


int64_t print_int(int64_t n)
{
    printf("%" PRId64 "\n", n);
    return 0; // unit
}

void partial_match(int64_t o) {
    printf("Partial match error occured");
    exit(1);
}

int64_t *alloc_tuple(int64_t count)
{
    int64_t *tuple = malloc(sizeof(int64_t) * count);
    return tuple;
}


int64_t *alloc_closure(int64_t (*fptr)(void*), int64_t *env, int64_t arity, int64_t envsize)
{
    closure *c = malloc(sizeof(closure));
    c->fptr = fptr;
    c->env = env;
    c->arity = arity;
    c->applied_count = 0;
    c->envsize = envsize;
    return (int64_t *)c;
}

closure *copy_closure(closure* c) {
    closure *c_copy = malloc(sizeof(closure));
    c_copy->fptr = c->fptr;
    c_copy->arity = c->arity;
    c_copy->applied_count = c->applied_count;
    int64_t envsize = c->envsize;
    c_copy->envsize = envsize;
    int64_t* env_copy = alloc_tuple(envsize);
    c_copy->env = env_copy;

    int64_t* env = c->env;
    for (int64_t i = 0; i < c->envsize; i++)
    {
        int64_t* copy_elem = (env_copy + i);
        *copy_elem = *(env + i);   
    }
    return c_copy;
}

int64_t call_closure(closure *c, int64_t *args, int64_t args_count)
{
    if (args_count == 0) {
        int64_t (*callee)() = (int64_t (*)())c->fptr;
        return callee();
    }
    
    int64_t *curr_arg = args;
    closure *curr_c = copy_closure(c);
    // printf("applied_count: %d, arity: %d, args_count: %d, closure_addr: %d\n", (int)(curr_c->applied_count), (int)curr_c->arity, (int)args_count), (int)(curr_c);
    for (size_t i = 0; i < args_count; i++)
    {
        int64_t ac = curr_c->applied_count;
        if (ac + 1 == curr_c->arity)
        {
            int64_t call_res;
            if (curr_c->envsize == 0) {
                int64_t (*callee)(int64_t) = (int64_t (*)(int64_t))curr_c->fptr;
                call_res = callee(*curr_arg);
            } else {
                int64_t (*callee)(int64_t*, int64_t) = (int64_t (*)(int64_t*, int64_t)) curr_c->fptr;
                call_res = callee(curr_c->env, *curr_arg);
            }
            curr_c = (closure *)call_res;
        }
        else
        {
            curr_c->env[ac] = *curr_arg;
            curr_c->applied_count++;
        }
        curr_arg++;
    }
    return (int64_t)curr_c;
}

typedef struct list_node
{
    int64_t value;
    struct list_node *next;
} list_node;

list_node *list_cons(int64_t v, list_node* tail) {
    list_node* new_node = malloc(sizeof(list_node));
    new_node-> value = v;
    new_node->next = tail;
    return new_node;
}
