#include "taggedValue.h"

void *allocate_memory(size_t size)
{
    void *ptr = malloc(size);
    if (!ptr)
    {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

VAL *allocate_tagged_val(int8_t tag)
{
    VAL *val = allocate_memory(sizeof(VAL));
    val->tag = tag;
    return val;
}

void set_tagged_val(VAL *val, void *data)
{
    val->data = data;
}

void tag_checker(VAL *val, int8_t tag)
{
    if (val == NULL)
    {
        fprintf(stderr, "Unexpected null value");
        exit(EXIT_FAILURE);
    }

    if (val->tag != tag)
    {
        fprintf(stderr, "Type mismatch: The provided tag is not a %d.\n", tag);
        exit(EXIT_FAILURE);
    }
}
