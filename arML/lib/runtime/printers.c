#include "taggedValue.h"

void print_int(VAL *value)
{
    tag_checker(value, TAG_INT);
    printf("%d\n", *((int32_t *)value->data));
}

void print_string(VAL *value)
{
    tag_checker(value, TAG_STR);
    printf("%s\n", (char *)value->data);
}

void print_bool(VAL *value)
{
    tag_checker(value, TAG_BOOL);
    printf("%s\n", *((int8_t *)value->data) ? "true" : "false");
}
