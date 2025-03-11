#include <stdint.h>
#include <stdio.h>

typedef int64_t value;

int64_t print_int(int64_t n) {
  printf("%ld\n", (long)n);
  fflush(stdout);
  return 0;
}