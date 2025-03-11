#include "taggedValue.h"
#include <stdio.h>
#include <stdlib.h>

// Arithmetic

VAL *multip(VAL *a, VAL *b)
{
    tag_checker(a, TAG_INT);
    tag_checker(b, TAG_INT);

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = *((int32_t *)a->data) * *((int32_t *)b->data);

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

VAL *plus(VAL *a, VAL *b)
{
    tag_checker(a, TAG_INT);
    tag_checker(b, TAG_INT);

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = *((int32_t *)a->data) + *((int32_t *)b->data);

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

VAL *minus(VAL *a, VAL *b)
{
    tag_checker(a, TAG_INT);
    tag_checker(b, TAG_INT);

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = *((int32_t *)a->data) - *((int32_t *)b->data);

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

VAL *division(VAL *a, VAL *b)
{
    tag_checker(a, TAG_INT);
    tag_checker(b, TAG_INT);

    if (*((int32_t *)b->data) == 0)
    {
        fprintf(stderr, "Error: Division by zero is not allowed.\n");
        exit(EXIT_FAILURE);
    }

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = *((int32_t *)a->data) / *((int32_t *)b->data);

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

VAL *divRemainder(VAL *a, VAL *b)
{
    tag_checker(a, TAG_INT);
    tag_checker(b, TAG_INT);

    if (*((int32_t *)b->data) == 0)
    {
        fprintf(stderr, "Error: Division by zero is not allowed.\n");
        exit(EXIT_FAILURE);
    }

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = *((int32_t *)a->data) % *((int32_t *)b->data);

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

// Comparison

VAL *feq(VAL *a, VAL *b)
{
    tag_checker(a, b->tag);
    tag_checker(b, a->tag);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int32_t *)a->data) == *((int32_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *fneq(VAL *a, VAL *b)
{
    tag_checker(a, b->tag);
    tag_checker(b, a->tag);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int32_t *)a->data) != *((int32_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *lt(VAL *a, VAL *b)
{
    tag_checker(a, b->tag);
    tag_checker(b, a->tag);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int32_t *)a->data) < *((int32_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *le(VAL *a, VAL *b)
{
    tag_checker(a, b->tag);
    tag_checker(b, a->tag);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int32_t *)a->data) <= *((int32_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *gt(VAL *a, VAL *b)
{
    tag_checker(a, b->tag);
    tag_checker(b, a->tag);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int32_t *)a->data) > *((int32_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *ge(VAL *a, VAL *b)
{
    tag_checker(a, b->tag);
    tag_checker(b, a->tag);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int32_t *)a->data) >= *((int32_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *leq(VAL *a, VAL *b)
{
    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (a == b);

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *lneq(VAL *a, VAL *b)
{
    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (a != b);

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

// Logical operations

VAL *bAnd(VAL *a, VAL *b)
{
    tag_checker(a, TAG_BOOL);
    tag_checker(b, TAG_BOOL);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int8_t *)a->data)) & (*((int8_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}

VAL *bOr(VAL *a, VAL *b)
{
    tag_checker(a, TAG_BOOL);
    tag_checker(b, TAG_BOOL);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = (*((int8_t *)a->data)) | (*((int8_t *)b->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}
