#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <ffi/ffi.h>

#define TAG_INT 0b000
#define TAG_STRING 0b001
#define TAG_BOOL 0b010
#define TAG_LIST 0b011
#define TAG_TUPLE 0b100
#define TAG_CLOSURE 0b101

struct TAGGED_VAL
{
    int8_t tag;
    union
    {
        int32_t int_val;
        bool bool_val;
        char *string_val;
        struct ObaML_LIST *list_val;
        struct ObaML_TUPLE *tuple_val;
        struct ObaML_CLOSURE *closure_val;
    } data;
};

struct ObaML_LIST
{
    u_int32_t size;
    struct TAGGED_VAL **items;
};

struct ObaML_TUPLE
{
    u_int32_t size;
    struct TAGGED_VAL **items;
};

struct ObaML_CLOSURE
{
    void *fun_ptr;
    u_int32_t args_num;
    u_int32_t applied_args_num;
    struct TAGGED_VAL **applied_args;
};

struct TAGGED_VAL *create_int_val(int32_t value)
{
    struct TAGGED_VAL *val = malloc(sizeof(struct TAGGED_VAL));
    if (val == NULL)
    {
        fprintf(stderr, "Exception $create_int_val$: Memory allocation for failed\n");
        exit(1);
    }

    val->tag = TAG_INT;
    val->data.int_val = value;
    return val;
}

struct TAGGED_VAL *create_bool_val(bool value)
{
    struct TAGGED_VAL *val = malloc(sizeof(struct TAGGED_VAL));
    if (val == NULL)
    {
        fprintf(stderr, "Exception $create_bool_val$: Memory allocation for failed\n");
        exit(1);
    }

    val->tag = TAG_BOOL;

    if (value == 1)
    {
        val->data.bool_val = true;
    }
    else
    {
        val->data.bool_val = false;
    }

    return val;
}

bool get_i1_val(struct TAGGED_VAL *val)
{
    if (val == NULL)
    {
        fprintf(stderr, "Exception $get_i1_val$: val is NULL\n");
        exit(1);
    }

    if (val->tag != TAG_BOOL)
    {
        fprintf(stderr, "Exception $get_i1_val$: unexpected type\n");
        exit(1);
    }

    return val->data.bool_val;
}

struct TAGGED_VAL *create_string_val(char *value)
{
    struct TAGGED_VAL *val = malloc(sizeof(struct TAGGED_VAL));
    if (val == NULL)
    {
        fprintf(stderr, "Exception $create_string_val$: Memory allocation for failed\n");
        exit(1);
    }

    val->tag = TAG_STRING;
    val->data.string_val = value;
    return val;
}

//  OPERATIONS WITH LIST

struct TAGGED_VAL *create_empty_list_val()
{
    struct ObaML_LIST *new_lst = malloc(sizeof(struct ObaML_LIST));
    if (new_lst == NULL)
    {
        fprintf(stderr, "Exception $create_empty_list_val$: Memory allocation for failed\n");
        exit(1);
    }

    new_lst->size = 0;

    struct TAGGED_VAL **new_lst_items = malloc(sizeof(struct TAGGED_VAL *));
    if (new_lst_items == NULL)
    {
        fprintf(stderr, "Exception $create_empty_list_val$: Memory allocation for failed\n");
        exit(1);
    }
    new_lst->items = new_lst_items;

    struct TAGGED_VAL *new_val_lst = malloc(sizeof(struct TAGGED_VAL));
    if (new_val_lst == NULL)
    {
        fprintf(stderr, "Exception $create_empty_list_val$: Memory allocation for failed\n");
        exit(1);
    }
    new_val_lst->tag = TAG_LIST;
    new_val_lst->data.list_val = new_lst;

    return new_val_lst;
}

struct TAGGED_VAL *eq(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b);

struct TAGGED_VAL *lst_eq(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $lst_eq$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_LIST || val_b->tag != TAG_LIST)
    {
        fprintf(stderr, "Exception $lst_eq$: unexpected type\n");
        exit(1);
    }

    struct ObaML_LIST *a = val_a->data.list_val;
    if (a == NULL)
    {
        fprintf(stderr, "Exception $lst_eq$: val is NULL\n");
        exit(1);
    }

    struct ObaML_LIST *b = val_b->data.list_val;
    if (b == NULL)
    {
        fprintf(stderr, "Exception $lst_eq$: val is NULL\n");
        exit(1);
    }

    if (a->size != b->size)
    {
        return create_bool_val(false);
    }

    for (int i = 0; i < a->size; i++)
    {
        struct TAGGED_VAL *rev_res = eq(a->items[i], b->items[i]);
        if (rev_res == NULL)
        {
            fprintf(stderr, "Exception $lst_eq$: val is NULL\n");
            exit(1);
        }

        if (!rev_res->data.bool_val)
        {
            return create_bool_val(false);
        }
    }

    return create_bool_val(true);
}

struct TAGGED_VAL *add_elem_to_list_val(struct TAGGED_VAL *val_to_add, struct TAGGED_VAL *list_val)
{
    if (val_to_add == NULL || list_val == NULL)
    {
        fprintf(stderr, "Exception $lst_eq$: val is NULL\n");
        exit(1);
    }

    if (list_val->tag != TAG_LIST)
    {
        fprintf(stderr, "Exception $add_elem_to_list_val$: unexpected type\n");
        exit(1);
    }

    struct ObaML_LIST *old_lst = list_val->data.list_val;
    if (old_lst == NULL)
    {
        fprintf(stderr, "Exception $add_elem_to_list_val$: Memory allocation for failed\n");
        exit(1);
    }

    struct ObaML_LIST *new_lst = malloc(sizeof(struct ObaML_LIST));
    if (new_lst == NULL)
    {
        fprintf(stderr, "Exception $add_elem_to_list_val$: Memory allocation for failed\n");
        exit(1);
    }

    new_lst->size = old_lst->size + 1;

    struct TAGGED_VAL **new_lst_items = malloc(new_lst->size * sizeof(struct TAGGED_VAL *));
    if (new_lst_items == NULL)
    {
        fprintf(stderr, "Exception $add_elem_to_list_val$: Memory allocation for failed\n");
        exit(1);
    }

    for (int i = 0; i < old_lst->size; i++)
    {
        new_lst_items[i + 1] = old_lst->items[i];
    }

    new_lst_items[0] = val_to_add;

    new_lst->items = new_lst_items;

    struct TAGGED_VAL *new_val_lst = malloc(sizeof(struct TAGGED_VAL));
    if (new_val_lst == NULL)
    {
        fprintf(stderr, "Exception $add_elem_to_list_val$: Memory allocation for failed\n");
        exit(1);
    }
    new_val_lst->tag = TAG_LIST;
    new_val_lst->data.list_val = new_lst;

    return new_val_lst;
}

struct TAGGED_VAL *list_length_getter(struct TAGGED_VAL *val)
{

    if (val->tag != TAG_LIST)
    {
        fprintf(stderr, "Exception $list_length_getter$: unexpected type\n");
        exit(1);
    }

    if (val->data.list_val == NULL || create_int_val(val->data.list_val->size) == NULL)
    {
        fprintf(stderr, "Exception $list_length_getter$: Error to create length answer\n");
        exit(1);
    }

    struct TAGGED_VAL *res = create_int_val(val->data.list_val->size);
    if (res == NULL)
    {
        fprintf(stderr, "Exception $list_length_getter$: Length result is NULL\n");
        exit(1);
    }

    return res;
}

struct TAGGED_VAL *list_head_getter(struct TAGGED_VAL *val)
{
    if (val == NULL)
    {
        fprintf(stderr, "Exception $list_head_getter$: val is NULL\n");
        exit(1);
    }

    if (val->tag != TAG_LIST)
    {
        fprintf(stderr, "Exception $list_head_getter$: unexpected type\n");
        exit(1);
    }

    struct ObaML_LIST *lst = val->data.list_val;
    if (lst == NULL)
    {
        fprintf(stderr, "Exception $list_head_getter$: lst is NULL\n");
        exit(1);
    }

    if (lst->size == 0)
    {
        fprintf(stderr, "Exception $list_head_getter$: empty list\n");
        exit(1);
    }

    if (lst->items[0] == NULL)
    {
        fprintf(stderr, "Exception $list_head_getter$: item is NULL\n");
        exit(1);
    }

    return lst->items[0];
}

struct TAGGED_VAL *list_tail_getter(struct TAGGED_VAL *val)
{
    if (val == NULL)
    {
        fprintf(stderr, "Exception $list_tail_getter$: val is NULL\n");
        exit(1);
    }

    if (val->tag != TAG_LIST)
    {
        fprintf(stderr, "Exception $list_tail_getter$: unexpected type\n");
        exit(1);
    }

    struct ObaML_LIST *old_lst = val->data.list_val;
    if (old_lst == NULL)
    {
        fprintf(stderr, "Exception $list_tail_getter$: old_lst is NULLt\n");
        exit(1);
    }

    if (old_lst->size < 1)
    {
        fprintf(stderr, "Exception $list_tail_getter$: empty list\n");
        exit(1);
    }

    struct ObaML_LIST *new_lst = malloc(sizeof(struct ObaML_LIST));
    if (new_lst == NULL)
    {
        fprintf(stderr, "Exception $list_tail_getter$: Memory allocation for failed\n\n");
        exit(1);
    }

    new_lst->size = old_lst->size - 1;

    struct TAGGED_VAL **new_lst_items = malloc(new_lst->size * sizeof(struct TAGGED_VAL *));
    if (new_lst_items == NULL)
    {
        fprintf(stderr, "Exception $list_tail_getter$: Memory allocation for failed\n\n");
    }

    for (int i = 0; i < new_lst->size; i++)
    {
        new_lst_items[i] = old_lst->items[i + 1];
    }

    new_lst->items = new_lst_items;

    struct TAGGED_VAL *new_val_lst = malloc(sizeof(struct TAGGED_VAL));
    if (new_val_lst == NULL)
    {
        fprintf(stderr, "Exception $list_tail_getter$: Memory allocation for failed\n\n");
        exit(1);
    }

    new_val_lst->tag = TAG_LIST;
    new_val_lst->data.list_val = new_lst;

    return new_val_lst;
}

// OPERATIONS WITH TUPLE

struct TAGGED_VAL *create_tuple(int32_t length, ...)
{
    va_list tup_elms;
    va_start(tup_elms, length);

    struct ObaML_TUPLE *tuple = malloc(sizeof(struct ObaML_TUPLE));
    if (tuple == NULL)
    {
        fprintf(stderr, "Exception $create_tuple$: Memory allocation for failed\n");
        exit(1);
    }

    tuple->size = length;

    struct TAGGED_VAL **tuple_elms = malloc(length * sizeof(struct TAGGED_VAL *));
    if (tuple_elms == NULL)
    {
        fprintf(stderr, "Exception $create_tuple$: Memory allocation for failed\n");
        exit(1);
    }

    for (int i = 0; i < length; i++)
    {
        tuple_elms[i] = va_arg(tup_elms, struct TAGGED_VAL *);
    }

    va_end(tup_elms);

    tuple->items = tuple_elms;

    struct TAGGED_VAL *tuple_val = malloc(sizeof(struct TAGGED_VAL));
    if (tuple_elms == NULL)
    {
        fprintf(stderr, "Exception $create_tuple$: Memory allocation for failed\n");
        exit(1);
    }

    tuple_val->tag = TAG_TUPLE;
    tuple_val->data.tuple_val = tuple;

    return tuple_val;
}

struct TAGGED_VAL *tuple_eq(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $tuple_eq$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_TUPLE || val_b->tag != TAG_TUPLE)
    {
        fprintf(stderr, "Exception $tuple_eq$: unexpected type\n");
        exit(1);
    }

    struct ObaML_TUPLE *a = val_a->data.tuple_val;
    if (a == NULL)
    {
        fprintf(stderr, "Exception $tuple_eq$: val is NULL\n");
        exit(1);
    }

    struct ObaML_TUPLE *b = val_b->data.tuple_val;
    if (b == NULL)
    {
        fprintf(stderr, "Exception $tuple_eq$: val is NULL\n");
        exit(1);
    }

    if (a->size != b->size)
    {
        return create_bool_val(false);
    }

    for (int i = 0; i < a->size; i++)
    {
        struct TAGGED_VAL *rev_res = eq(a->items[i], b->items[i]);
        if (rev_res == NULL)
        {
            fprintf(stderr, "Exception $tuple_eq$: val is NULL\n");
            exit(1);
        }

        if (!rev_res->data.bool_val)
        {
            return create_bool_val(false);
        }
    }

    return create_bool_val(true);
}

struct TAGGED_VAL *tuple_getter(struct TAGGED_VAL *ind_val, struct TAGGED_VAL *tuple_val)
{
    if (ind_val == NULL || tuple_val == NULL)
    {
        fprintf(stderr, "Exception $tuple_getter$: val is NULL\n");
        exit(1);
    }

    if (ind_val->tag != TAG_INT || tuple_val->tag != TAG_TUPLE)
    {
        fprintf(stderr, "Exception $tuple_getter$: unexpected type\n");
        exit(1);
    }

    int32_t ind = ind_val->data.int_val;

    struct ObaML_TUPLE *tuple = tuple_val->data.tuple_val;
    if (tuple == NULL)
    {
        fprintf(stderr, "Exception $tuple_getter$: val is NULL\n");
        exit(1);
    }

    if (tuple->size <= ind)
    {
        fprintf(stderr, "Exception $tuple_getter$: Index out of range\n");
        exit(1);
    }

    return tuple->items[ind];
}

struct TAGGED_VAL *mult(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $mult$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $mult$: unexpected type\n");
    }

    int32_t a = val_a->data.int_val;
    int32_t b = val_b->data.int_val;

    return create_int_val(a * b);
}

struct TAGGED_VAL *divv(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $div$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $div$: unexpected type\n");
        exit(1);
    }

    int32_t a = val_a->data.int_val;
    int32_t b = val_b->data.int_val;

    if (b == 0)
    {
        fprintf(stderr, "Exception $div$: division by zero\n");
        exit(1);
    }

    return create_int_val(a / b);
}

struct TAGGED_VAL *plus(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $plus$: one of the args is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $plus$: unexpected type\n");
        exit(1);
    }

    int32_t a = val_a->data.int_val;
    int32_t b = val_b->data.int_val;

    return create_int_val(a + b);
}

struct TAGGED_VAL *minus(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $minus$: one of the args is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $minus$: unexpected type\n");
        exit(1);
    }

    int32_t a = val_a->data.int_val;
    int32_t b = val_b->data.int_val;

    return create_int_val(a - b);
}

struct TAGGED_VAL *req(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $req$: one of the args is NULL\n");
        exit(1);
    }

    if (val_a->tag == val_b->tag && (val_a->tag == TAG_INT || val_a->tag == TAG_BOOL || val_a->tag == TAG_STRING))
    {
        return eq(val_a, val_b);
    }

    return create_bool_val(val_a == val_b);
}

struct TAGGED_VAL *eq(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $eq$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != val_b->tag)
    {
        return create_bool_val(false);
    }

    if (val_a->tag == TAG_INT)
    {
        return create_bool_val(val_a->data.int_val == val_b->data.int_val);
    }

    // if(val_a->tag == TAG_STRING) {
    //     return create_bool_val(false);
    // }

    if (val_a->tag == TAG_BOOL)
    {
        return create_bool_val(val_a->data.bool_val == val_b->data.bool_val);
    }

    if (val_a->tag == TAG_LIST)
    {
        return lst_eq(val_a, val_b);
    }

    if (val_a->tag == TAG_TUPLE)
    {
        return tuple_eq(val_a, val_b);
    }

    fprintf(stderr, "Exception $eq$: unexpected type\n");
    exit(1);
}

struct TAGGED_VAL *rneq(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    struct TAGGED_VAL *res = req(val_a, val_b);

    return create_bool_val(res->data.bool_val);
}

struct TAGGED_VAL *neq(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    struct TAGGED_VAL *res = eq(val_a, val_b);

    return create_bool_val(res->data.bool_val);
}

struct TAGGED_VAL *lt(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $lt$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $lt$: unexpected type\n");
        exit(1);
    }

    return create_bool_val(val_a->data.int_val < val_b->data.int_val);
}

struct TAGGED_VAL *lte(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $lte$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $lte$: unexpected type\n");
        exit(1);
    }

    return create_bool_val(val_a->data.int_val <= val_b->data.int_val);
}

struct TAGGED_VAL *gt(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $gt$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gt$: unexpected type\n");
        exit(1);
    }

    return create_bool_val(val_a->data.int_val > val_b->data.int_val);
}

struct TAGGED_VAL *gte(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $gte$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gte$: unexpected type\n");
        exit(1);
    }

    return create_bool_val(val_a->data.int_val >= val_b->data.int_val);
}

struct TAGGED_VAL *uplus(struct TAGGED_VAL *val_a)
{
    if (val_a == NULL)
    {
        fprintf(stderr, "Exception $uplus$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $uplus$: unexpected type\n");
        exit(1);
    }

    return create_int_val(val_a->data.int_val);
}

struct TAGGED_VAL *uminus(struct TAGGED_VAL *val_a)
{
    if (val_a == NULL)
    {
        fprintf(stderr, "Exception $uminus$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $uminus$: unexpected type\n");
        exit(1);
    }

    return create_int_val(-val_a->data.int_val);
}

struct TAGGED_VAL *llor(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $llor$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gt$: unexpected type\n");
        exit(1);
    }

    return create_bool_val(val_a->data.int_val || val_b->data.int_val);
}

struct TAGGED_VAL *lland(struct TAGGED_VAL *val_a, struct TAGGED_VAL *val_b)
{
    if (val_a == NULL || val_b == NULL)
    {
        fprintf(stderr, "Exception $lland$: val is NULL\n");
        exit(1);
    }

    if (val_a->tag != TAG_INT || val_b->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $gt$: unexpected type\n");
        exit(1);
    }

    return create_bool_val(val_a->data.int_val && val_b->data.int_val);
}

void matching_failed()
{
    fprintf(stderr, "Exception: Match failure\n");
    exit(1);
}

// PRINTERS

void print_int(struct TAGGED_VAL *num)
{
    if (num == NULL)
    {
        fprintf(stderr, "Exception $print_int$: num is NULL\n");
        exit(1);
    }

    if (num->tag != TAG_INT)
    {
        fprintf(stderr, "Exception $print_int$: unexpected type\n");
        exit(1);
    }

    printf("%d\n", num->data.int_val);
}

void print_string(struct TAGGED_VAL *str)
{
    if (str == NULL)
    {
        fprintf(stderr, "Exception $print_string$: num is NULL\n");
        exit(1);
    }

    if (str->tag != TAG_STRING)
    {
        fprintf(stderr, "Exception $print_string$: unexpected type\n");
        exit(1);
    }

    printf("%s\n", str->data.string_val);
}

// CLOSURE

struct TAGGED_VAL *create_closure(void *fun_ptr, int32_t fun_args_num)
{
    struct ObaML_CLOSURE *closure = malloc(sizeof(struct ObaML_CLOSURE));
    if (closure == NULL)
    {
        fprintf(stderr, "Exception $create_closure$: Memory allocation for failed\n");
        exit(1);
    }

    closure->fun_ptr = fun_ptr;
    closure->args_num = fun_args_num;
    closure->applied_args_num = 0;
    closure->applied_args = malloc(0);

    struct TAGGED_VAL *closure_val = malloc(sizeof(struct TAGGED_VAL));
    if (closure_val == NULL)
    {
        fprintf(stderr, "Exception $create_closure$: Memory allocation for failed\n");
        exit(1);
    }

    closure_val->tag = TAG_CLOSURE;
    closure_val->data.closure_val = closure;

    return closure_val;
}

struct TAGGED_VAL *call_closure(struct TAGGED_VAL *closure_val)
{
    if (closure_val == NULL)
    {
        fprintf(stderr, "Exception $call_closure$: num is NULL\n");
        exit(1);
    }

    if (closure_val->tag != TAG_CLOSURE)
    {
        fprintf(stderr, "Exception $call_closure$: unexpected type\n");
        exit(1);
    }

    struct ObaML_CLOSURE *closure = closure_val->data.closure_val;
    if (closure == NULL)
    {
        fprintf(stderr, "Exception $call_closure$: closure is NULL\n");
        exit(1);
    }

    size_t args_count = closure->args_num;

    if (closure->applied_args_num != args_count)
    {
        fprintf(stderr, "Exception $call_closure$: incorrect number of arguments\n");
        exit(1);
    }

    ffi_cif cif;
    ffi_type **ffi_args_types = malloc(closure->args_num * sizeof(ffi_type *));
    if (ffi_args_types == NULL)
    {
        fprintf(stderr, "Exception $call_closure$: args_val is NULL\n");
        exit(1);
    }

    void **args_val = malloc(closure->args_num * sizeof(void *));
    if (args_val == NULL)
    {
        fprintf(stderr, "Exception $call_closure$: args_val is NULL\n");
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
        fprintf(stderr, "Exception $call_closure$: ffi preparing failed\n");
        exit(1);
    }

    struct TAGGED_VAL *call_res = malloc(sizeof(struct TAGGED_VAL));
    if (call_res == NULL)
    {
        fprintf(stderr, "Exception $call_closure$: Memory allocation for failed\n");
        exit(1);
    }

    ffi_call(&cif, closure->fun_ptr, &call_res, args_val);

    if (call_res == NULL)
    {
        // fprintf(stderr, "fun_ptr: %p, args_num%d\n", closure->fun_ptr, closure->args_num);
        fprintf(stderr, "Exception $call_closure$: call result is NULL\n");
        exit(1);
    }

    return call_res;
}

struct TAGGED_VAL *apply_closure(struct TAGGED_VAL *closure_val, int32_t args_num, ...)
{
    va_list args;
    va_start(args, args_num);

    if (closure_val == NULL)
    {
        fprintf(stderr, "Exception $apply_closure$: num is NULL\n");
        exit(1);
    }

    if (closure_val->tag != TAG_CLOSURE)
    {
        fprintf(stderr, "Exception $apply_closure$: unexpected type\n");
        exit(1);
    }

    struct TAGGED_VAL *new_closuse_val = malloc(sizeof(struct TAGGED_VAL));
    if (new_closuse_val == NULL)
    {
        fprintf(stderr, "Exception $apply_closure$: Memory allocation for failed\n");
        exit(1);
    }

    new_closuse_val->tag = TAG_CLOSURE;

    struct ObaML_CLOSURE *old_closure = closure_val->data.closure_val;
    if (old_closure == NULL)
    {
        fprintf(stderr, "Exception $apply_closure$: old_closure is NULL\n");
        exit(1);
    }

    struct ObaML_CLOSURE *new_closure = malloc(sizeof(struct ObaML_CLOSURE));
    if (new_closure == NULL)
    {
        fprintf(stderr, "Exception $apply_closure$: Memory allocation for failed\n");
        exit(1);
    }
    new_closure->fun_ptr = old_closure->fun_ptr;
    new_closure->args_num = old_closure->args_num;
    new_closure->applied_args_num = old_closure->applied_args_num + args_num;

    struct TAGGED_VAL **new_applied_args = malloc(new_closure->applied_args_num * sizeof(struct TAGGED_VAL *));
    if (new_applied_args == NULL)
    {
        fprintf(stderr, "Exception $apply_closure$: Memory allocation for failed\n");
        exit(1);
    }

    for (int i = 0; i < old_closure->applied_args_num; i++)
    {
        new_applied_args[i] = old_closure->applied_args[i];
    }

    for (int i = 0; i < args_num; i++)
    {
        struct TAGGED_VAL *arg = va_arg(args, struct TAGGED_VAL *);
        if (arg == NULL)
        {
            fprintf(stderr, "Exception $apply_closure$: new argument is NULL\n");
            exit(1);
        }
        new_applied_args[old_closure->applied_args_num + i] = arg;
    }

    va_end(args);

    new_closure->applied_args = new_applied_args;

    new_closuse_val->data.closure_val = new_closure;

    if (new_closure->applied_args_num == new_closure->args_num)
    {
        return call_closure(new_closuse_val);
    }

    return new_closuse_val;
}
