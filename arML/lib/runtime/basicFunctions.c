#include "taggedValue.h"
#include <string.h>
#include <stdbool.h>

VAL *ct_int_v(int32_t value)
{
    VAL *val = allocate_tagged_val(TAG_INT);
    int32_t *int_data = allocate_memory(sizeof(int32_t));
    *int_data = value;
    set_tagged_val(val, int_data);
    return val;
}

VAL *ct_bool_v(bool value)
{
    VAL *val = allocate_tagged_val(TAG_BOOL);
    bool *bool_data = allocate_memory(sizeof(bool));
    *bool_data = value;
    set_tagged_val(val, bool_data);
    return val;
}

VAL *ct_str_v(const char *value)
{
    VAL *val = allocate_tagged_val(TAG_STR);
    char *str_data = allocate_memory(strlen(value) + 1);
    strcpy(str_data, value);
    set_tagged_val(val, str_data);
    return val;
}

VAL *ct_char_v(char value)
{
    VAL *val = allocate_tagged_val(TAG_CHAR);
    char *char_data = allocate_memory(sizeof(char));
    *char_data = value;
    set_tagged_val(val, char_data);
    return val;
}

VAL *ct_unit_v()
{
    VAL *val = allocate_tagged_val(TAG_UNIT);
    set_tagged_val(val, NULL);
    return val;
}

bool check_cond(VAL *val)
{
    tag_checker(val, TAG_BOOL);
    return val->data;
}
