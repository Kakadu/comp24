#include <ffi.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG
#define DEBUG_RUN(code)                                                                            \
        do {                                                                                       \
                code fflush(stdout);                                                               \
        } while (0)
#else
#define DEBUG_RUN(code)
#endif

#define roundup2i64(sz_in_bytes)                                                                   \
        ((sz_in_bytes) % 8 != 0 ? ((sz_in_bytes) + 8 - ((sz_in_bytes) % 8)) : (sz_in_bytes))

#define item_num2sz(n) ((n) * 8)
#define IN_BYTES(sz) ((sz) / 8)

#define rt_(name) (rt_##name)
#define FAIL_WITH(exit_code, fmt, ...)                                                             \
        do {                                                                                       \
                fprintf(stderr, "Exception: ");                                                    \
                fprintf(stderr, fmt, ##__VA_ARGS__);                                               \
                fprintf(stderr, "\n");                                                             \
                exit(exit_code);                                                                   \
        } while (0)

typedef enum {
        T_SIMPLE = -1,
        T_TUPLE = 0,
        T_LIST = 1,
        T_CLOSURE = 2,
} tag_t;

/* =================== DATA TYPES =================== */

typedef int64_t elem_t;
typedef int64_t num_t;
typedef int64_t code_t;

#define REMOVE_INT_TAG(integer) ((integer) >> 1)
#define CREATE_INT_TAG(integer) (((num_t)(integer) << 1) + 1)
#define TRUE 1
#define FALSE 0

typedef struct {
        code_t code;
        int64_t args_n;
        int64_t applied_args;
        int64_t args[];
} closure_t;

#pragma pack(push, 1)
typedef struct {
        int64_t size : 56;
        tag_t tag : 8;
} box_head_t;
typedef struct {
        box_head_t head;
        int64_t body[];
} box_t;
#pragma pack(pop)

typedef struct {
        elem_t val;
        elem_t next;
        int64_t length;
} list_node_t;

box_t *alloc_box_t(size_t body_size) {
        body_size = roundup2i64(body_size) + sizeof(box_head_t);
        box_t *res_box = calloc(1, body_size);
        res_box->head.size = body_size;
        return res_box;
}

int is_ptr(elem_t arg) { return !(arg & 1); }

tag_t get_tag(elem_t elem) {
        if (elem == (elem_t)NULL)
                return T_SIMPLE;

        if (is_ptr(elem)) {
                return ((box_t *)elem)->head.tag;
        } else {
                return T_SIMPLE;
        }
}

/* =================== STD-LIB =================== */

int64_t compare(elem_t rt_a, elem_t rt_b);

int64_t compare_tuples(box_t *a_box, box_t *b_box) {
        int64_t res = TRUE;
        for (int i = 1; i < a_box->head.size; i++) {
                int idx = i - 1;
                res = compare(a_box->body[idx], b_box->body[idx]);
                if (res != TRUE)
                        break;
        }
        return res;
}

int64_t compare_lists(box_t *a_box, box_t *b_box) {
        list_node_t *a_head = (list_node_t *)(&(a_box->body));
        list_node_t *b_head = (list_node_t *)(&(b_box->body));

        if (a_head->length != b_head->length)
                return FALSE;

        if (compare(a_head->val, b_head->val) != TRUE)
                return FALSE;

        return compare_lists((box_t *)a_head->next, (box_t *)b_head->next);
}

int64_t compare(elem_t rt_a, elem_t rt_b) {
        DEBUG_RUN(printf("START compare a=%ld | b=%ld\n", rt_a, rt_b););

        tag_t a_tag = get_tag(rt_a);
        tag_t b_tag = get_tag(rt_b);

        if (a_tag != b_tag) {
                DEBUG_RUN(printf("END compare %d \n", FALSE); fflush(stdout););

                return FALSE;
        }

        if (a_tag == T_SIMPLE) {
                DEBUG_RUN(printf("END compare %d \n", rt_a == rt_b ? TRUE : FALSE);
                          fflush(stdout););
                return rt_a == rt_b ? TRUE : FALSE;
        }

        box_t *a_box = (box_t *)rt_a;
        box_t *b_box = (box_t *)rt_b;
        if (a_tag == T_CLOSURE)
                FAIL_WITH(-1, "Function comparison undefined");

        int64_t res;

        switch (a_tag) {
                case T_TUPLE:
                        res = compare_tuples(a_box, b_box);
                        break;
                case T_LIST:
                        res = compare_lists(a_box, b_box);
                        break;
                default:
                        FAIL_WITH(-1, "Impossible case");
                        return FALSE;
        }

        DEBUG_RUN(printf("END compare %ld \n", res););
        return res;
}

num_t rt_(eq)(elem_t rt_a, elem_t rt_b) { return CREATE_INT_TAG(compare(rt_a, rt_b)); }
num_t rt_(neq)(elem_t rt_a, elem_t rt_b) {
        int64_t res = compare(rt_a, rt_b);
        return CREATE_INT_TAG(res ^ 1);
}

/* ******************* BOOL ******************* */

num_t rt_(or)(num_t x, num_t y) { return CREATE_INT_TAG(REMOVE_INT_TAG(x) || REMOVE_INT_TAG(y)); };
num_t rt_ (and)(num_t x, num_t y) {
        return CREATE_INT_TAG(REMOVE_INT_TAG(x) && REMOVE_INT_TAG(y));
};

/* ******************* SPEC ******************* */

elem_t rt_(print_int)(num_t value) {
#ifdef DEBUG
        printf("[ INT: %ld ]\n", REMOVE_INT_TAG(value));
#else
        printf("%ld", REMOVE_INT_TAG(value));
#endif
        fflush(stdout);
        return (elem_t)NULL;
}

/* =================== INTERN =================== */

box_t *list_select_node(box_t *head_b, int64_t idx) {
        DEBUG_RUN(printf("START list_select_node | head=%p | idx=%ld |\n", head_b, idx););

        list_node_t *cur_head = (list_node_t *)(&(head_b->body));
        ;
        box_t *ret = head_b;
        for (int i = 0; i < idx; i++) {
                ret = (box_t *)cur_head->next;
                DEBUG_RUN(printf("list_select_node [cur_head] | next=%p | len=%ld | val=%ld |\n",
                                 ret, cur_head->length, cur_head->val););

                if (get_tag(cur_head->next) == T_SIMPLE) {
                        ret = (box_t *)NULL;
                        break;
                }
                if (get_tag(cur_head->next) != T_LIST)
                        FAIL_WITH(-1, "Impossible case");

                cur_head = (list_node_t *)(&(ret->body));
        }
        DEBUG_RUN(printf("END list_select_node: %p \n", ret););
        return ret;
}

elem_t rt_(get_list_tail)(elem_t l_ptr, num_t rt_n) {
        if (l_ptr == (elem_t)NULL)
                return l_ptr;

        DEBUG_RUN(printf("START get_list_tail \n"); fflush(stdout););

        int64_t idx = REMOVE_INT_TAG(rt_n);
        box_t *cur_box = list_select_node((box_t *)l_ptr, idx);

        DEBUG_RUN(printf("END get_list_tail: %p \n", cur_box););

        return (elem_t)cur_box;
}

num_t rt_(unminus)(num_t rt_a) { return CREATE_INT_TAG(-(rt_a >> 1)); }

elem_t rt_(get_by_idx)(elem_t iter_ptr, num_t rt_n) {
        int64_t idx = REMOVE_INT_TAG(rt_n);

        DEBUG_RUN(printf("START get_by_idx %ld \n", idx););

        box_t *box = (box_t *)iter_ptr;
        box_t *selcted_box = NULL;
        list_node_t *node;
        elem_t res;

        if (box == NULL)
                FAIL_WITH(-1, "Index out of bounds");

        switch (get_tag(iter_ptr)) {
                case T_TUPLE:
                        return box->body[idx];

                case T_LIST:
                        selcted_box = list_select_node(box, idx);
                        list_node_t *node = (list_node_t *)(&(selcted_box->body));
                        res = node->val;
                        return res;

                default:
                        FAIL_WITH(-1, "Illigal state (get_by_idx from "
                                      "primitive element)");
                        return -1;
        }
}

int64_t get_list_len(elem_t ptr) {
        if (ptr == (elem_t)NULL)
                return 0;

        box_t *box = (box_t *)ptr;
        list_node_t *node = (list_node_t *)(&(box->body));
        return node->length;
}

num_t rt_(get_list_len)(elem_t ptr) { return CREATE_INT_TAG((get_list_len(ptr) + 1)); }

elem_t rt_(fail)(elem_t something) {
        FAIL_WITH(-1, "Pattern-matching is not exhaustive");
        return (elem_t)NULL;
}

elem_t rt_(alloc_closure)(elem_t f_ptr, num_t rt_n) {
        int64_t n = REMOVE_INT_TAG(rt_n);
        DEBUG_RUN(printf("START alloc_closure args_n=%ld \n", n); fflush(stdout););

        box_t *new_box = alloc_box_t(sizeof(closure_t) + 8 * n);
        new_box->head.tag = T_CLOSURE;

        closure_t *clos = (closure_t *)(&new_box->body);
        clos->code = f_ptr;
        clos->args_n = n;
        clos->applied_args = 0;

        DEBUG_RUN(printf("END alloc_closure res=%p \n", new_box); fflush(stdout););

        return (elem_t)new_box;
}

elem_t extend_closure(box_t *origin_box, int64_t args_for_apply_n, va_list *new_args) {
        closure_t *origin_clos = (closure_t *)(&(origin_box->body));
        DEBUG_RUN(printf("START extend_closure old_code=%p | old_args_n=%ld \n",
                         (void *)origin_clos->code, args_for_apply_n);
                  fflush(stdout););

        box_t *new_box = alloc_box_t(sizeof(closure_t) + (origin_box->head.size));
        new_box->head.tag = T_CLOSURE;

        closure_t *new_clos = (closure_t *)(&(new_box->body));
        new_clos->code = origin_clos->code;
        new_clos->applied_args = origin_clos->applied_args + args_for_apply_n;
        new_clos->args_n = origin_clos->args_n;

        for (int i = 0; i < new_clos->applied_args; i++) {
                if (i < origin_clos->applied_args)
                        new_clos->args[i] = origin_clos->args[i];
                else
                        new_clos->args[i] = va_arg(*new_args, int64_t);
        }

        DEBUG_RUN(printf("END extend_closure new_box=%p | new_code=%p | new_args_n=%ld \n", new_box,
                         (void *)new_clos->code, new_clos->args_n);
                  fflush(stdout););
        return (elem_t)new_box;
}

elem_t call_closure(box_t *box, int64_t args_for_apply_n, va_list *new_args) {
        DEBUG_RUN(printf("START call_closure box=%p | for_app_n=%ld \n", box, args_for_apply_n);
                  fflush(stdout););

        closure_t *closure = (closure_t *)(&(box->body));
        int64_t args_cnt = closure->args_n;
        ffi_cif cif;
        ffi_type *arg_types[args_cnt];
        int64_t *args[args_cnt];
        int64_t new_args_buffer[args_for_apply_n];
        elem_t res = (elem_t)NULL;

        for (int i = 0; i < args_cnt; ++i) {
                arg_types[i] = &ffi_type_sint64;

                if (i < closure->applied_args)
                        args[i] = &(closure->args[i]);

                else {
                        int cur_arg = i - closure->applied_args;
                        new_args_buffer[cur_arg] = va_arg(*new_args, int64_t);
                        args[i] = &(new_args_buffer[cur_arg]);
                }
                DEBUG_RUN(printf("ARG: INT=%ld | PTR=%p \n", (*args[i]), ((void *)(*args[i])));
                          fflush(stdout););
        }

        if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args_cnt, &ffi_type_sint64, arg_types) == FFI_OK) {
                ffi_call(&cif, (void *)closure->code, &res, (void *)args);
        } else {
                FAIL_WITH(-1, "Problems with closure call\n");
        }

        DEBUG_RUN(printf("END call_closure res=%ld \n", res); fflush(stdout););

        return res;
}

elem_t apply_to_closure(box_t *box, int64_t args_for_apply_n, va_list *new_args) {

        closure_t *closure = (closure_t *)(&(box->body));
        int64_t not_applied_args_n = closure->args_n - closure->applied_args;

        if (not_applied_args_n <= args_for_apply_n) {
                elem_t call_res = call_closure(box, not_applied_args_n, new_args);
                args_for_apply_n -= not_applied_args_n;

                if (args_for_apply_n == 0)
                        return call_res;

                return apply_to_closure((box_t *)call_res, args_for_apply_n, new_args);
        }
        return extend_closure(box, args_for_apply_n, new_args);
}

elem_t rt_(apply_to_closure)(elem_t cl_ptr, num_t rt_n, ...) {
        int64_t args_for_apply_n = REMOVE_INT_TAG(rt_n);

        DEBUG_RUN(
            printf("START apply_clos clos=%p | args_n=%ld\n", (void *)cl_ptr, args_for_apply_n);
            fflush(stdout););

        va_list args;
        va_start(args, rt_n);
        elem_t res = apply_to_closure((box_t *)cl_ptr, args_for_apply_n, &args);
        va_end(args);

        DEBUG_RUN(printf("END apply_clos res=%p \n", (void *)res); fflush(stdout););

        return res;
}

elem_t rt_(append_to_list)(num_t rt_n, elem_t tl_ptr, ...) {
        int64_t n = REMOVE_INT_TAG(rt_n);
        box_t *box = (box_t *)tl_ptr;
        va_list list_elems;
        int64_t cur_length = get_list_len(tl_ptr);
        elem_t cur_tl = tl_ptr;

        va_start(list_elems, tl_ptr);

        DEBUG_RUN(printf("START append_to_list ptr=%ld | len=%ld \n", tl_ptr, cur_length);
                  fflush(stdout););

        for (int i = 0; i < n; i++) {
                box_t *new_box = alloc_box_t(sizeof(list_node_t));
                new_box->head.tag = T_LIST;

                list_node_t *new_node = (list_node_t *)(&(new_box->body));
                new_node->val = va_arg(list_elems, int64_t);

                DEBUG_RUN(printf("LIST ELEM=%ld \n", new_node->val); fflush(stdout););

                new_node->next = cur_tl;
                new_node->length = cur_length + 1;
                cur_length = new_node->length;

                cur_tl = (elem_t)new_box;
        }

        DEBUG_RUN(printf("END append_to_list len=%ld \n", cur_length); fflush(stdout););

        va_end(list_elems);
        return cur_tl;
}

elem_t rt_(alloc_tuple)(num_t rt_n, ...) {
        int64_t n = REMOVE_INT_TAG(rt_n);

        va_list tup_elems;
        va_start(tup_elems, rt_n);
        box_t *tuple_box = alloc_box_t(item_num2sz(n));
        tuple_box->head.tag = T_TUPLE;

        for (int i = 0; i < n; i++)
                tuple_box->body[i] = va_arg(tup_elems, int64_t);

        va_end(tup_elems);
        return (elem_t)tuple_box;
}

/* ******************* MATH  ******************* */

num_t rt_(mul)(num_t rt_a, num_t rt_b) { return (rt_a >> 1) * (rt_b - 1) + 1; }
num_t rt_(div)(num_t rt_a, num_t rt_b) { return (((rt_a >> 1) / (rt_b >> 1)) << 1) + 1; }
num_t rt_(add)(num_t rt_a, num_t rt_b) { return rt_a + rt_b - 1; }
num_t rt_(sub)(num_t rt_a, num_t rt_b) { return rt_a - rt_b + 1; }

/* EQ */
num_t rt_(leq)(num_t rt_a, num_t rt_b) { return CREATE_INT_TAG((rt_a <= rt_b)); }
num_t rt_(meq)(num_t rt_a, num_t rt_b) { return CREATE_INT_TAG((num_t)(rt_a >= rt_b)); }
num_t rt_(less)(num_t rt_a, num_t rt_b) { return CREATE_INT_TAG(rt_a < rt_b); }
num_t rt_(more)(num_t rt_a, num_t rt_b) { return CREATE_INT_TAG(rt_a > rt_b); }
num_t rt_(eq2)(num_t rt_a, num_t rt_b) { return CREATE_INT_TAG(rt_a == rt_b); }
