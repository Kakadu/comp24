#include "taggedValue.h"
#include <stdio.h>
#include <stdlib.h>

VAL *uplus(VAL *a)
{
    tag_checker(a, TAG_INT);

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = *((int32_t *)a->data);

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

VAL *uminus(VAL *a)
{
    tag_checker(a, TAG_INT);

    int32_t *result_data = allocate_memory(sizeof(int32_t));
    *result_data = -(*((int32_t *)a->data));

    VAL *result = allocate_tagged_val(TAG_INT);
    set_tagged_val(result, result_data);

    return result;
}

VAL *unot(VAL *a)
{
    tag_checker(a, TAG_BOOL);

    int8_t *result_data = allocate_memory(sizeof(int8_t));
    *result_data = !(*((int8_t *)a->data));

    VAL *result = allocate_tagged_val(TAG_BOOL);
    set_tagged_val(result, result_data);

    return result;
}
