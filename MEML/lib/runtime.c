#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int64_t print_int(int64_t n) {
    printf("%" PRId64 "\n", n);
    return 0; // unit
}