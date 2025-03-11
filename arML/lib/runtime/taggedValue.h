// taggedValue.h
#ifndef TAGGED_VALUE_H
#define TAGGED_VALUE_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define TAG_INT 0
#define TAG_BOOL 1
#define TAG_STR 2
#define TAG_CHAR 3
#define TAG_UNIT 4
#define TAG_CLOSURE 5

typedef struct VAL
{
    int8_t tag;
    void *data;
} VAL;

void *allocate_memory(size_t size);
VAL *allocate_tagged_val(int8_t tag);
void set_tagged_val(VAL *val, void *data);
void tag_checker(VAL *val, int8_t tag);

#endif
