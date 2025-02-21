#include <stdint.h>

#include "ml_runtimelib.h"

int64_t plus_mlint(int64_t x, int64_t y);
int64_t minus_mlint(int64_t x, int64_t y);
int64_t mult_mlint(int64_t x, int64_t y);
int64_t div_mlint(int64_t x, int64_t y);

int64_t eq_ml(int64_t x, int64_t y);
int64_t neq_ml(int64_t x, int64_t y);
int64_t peq_ml(int64_t x, int64_t y);
int64_t pneq_ml(int64_t x, int64_t y);
int64_t g_ml(int64_t x, int64_t y);
int64_t ge_ml(int64_t x, int64_t y);
int64_t l_ml(int64_t x, int64_t y);
int64_t le_ml(int64_t x, int64_t y);

void print_int(int64_t a);

int64_t lor_ml(int64_t x, int64_t y);
int64_t land_ml(int64_t x, int64_t y);