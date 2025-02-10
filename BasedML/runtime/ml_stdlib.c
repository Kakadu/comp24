#include "ml_stdlib.h"

int64_t plus_mlint(int64_t x, int64_t y) { return x + y - 1; }
int64_t minus_mlint(int64_t x, int64_t y) { return x - y + 1; }
int64_t mult_mlint(int64_t x, int64_t y) { return (x >> 1) * (y - 1) + 1; }
int64_t div_mlint(int64_t x, int64_t y) {
    return CONVERT_INT_NATIVE_TO_ML(CONVERT_INT_ML_TO_NATIVE(x) / CONVERT_INT_ML_TO_NATIVE(y));
}

int64_t compare_ml(int64_t x, int64_t y);

int64_t compare_boxed(int64_t x, int64_t y) {
    box_t* xbox = get_box_t(x);
    box_t* ybox = get_box_t(y);

    DEBUG_RUN(if (xbox->header.tag != ybox->header.tag) EXCEPTION_FMT("Debug \"compare: args have different tag\""););
    if (xbox->header.tag == T_CLOSURE) {
        EXCEPTION_FMT("Invalid_argument \"compare: functional closure\"");
    } else {
        // Tuple or list, same algo for both
        DEBUG_RUN(if (xbox->header.size != ybox->header.size)
                      EXCEPTION_FMT("Debug \"compare: args have different size\""););
        int64_t res = 0;
        for (int i = 1; i < xbox->header.size; i++) {
            int val_num = i - 1;
            res = compare_ml(xbox->values[val_num], xbox->values[val_num]);
            if (res != 0)
                break;
        }
        return res;
    }
}

int64_t compare_ml(int64_t x, int64_t y) {
    uint8_t is_x_ptr = is_ml_ptr(x);
    uint8_t is_y_ptr = is_ml_ptr(y);
    if (is_x_ptr && is_y_ptr) {
        DEBUG_RUN(if (is_inside_heap(x) != is_inside_heap(y))
                      EXCEPTION_FMT("Debug \"compare: some arg inside heap, other is not\""););
        if (!is_inside_heap(x)) {
            // function case
            EXCEPTION_FMT("Invalid_argument \"compare: functional value\"");
        } else {
            return compare_boxed(x, y);
        }
    } else if (is_x_ptr || is_y_ptr) {
        // cons and nill case
        if (is_x_ptr)
            return 1;
        else
            return -1;
    } else {
        // unboxed values
        x = CONVERT_INT_ML_TO_NATIVE(x);
        y = CONVERT_INT_ML_TO_NATIVE(y);

        if (x > y) {
            return 1;
        } else if (x < y) {
            return -1;
        } else {
            return 0;
        }
    }
}

int64_t eq_ml(int64_t x, int64_t y) { return compare_ml(x, y) == 0; }
int64_t neq_ml(int64_t x, int64_t y) { return compare_ml(x, y) != 0; }
int64_t peq_ml(int64_t x, int64_t y) { return compare_ml(x, y) <= 0; }

int64_t g_ml(int64_t x, int64_t y) { return compare_ml(x, y) == 1; }
int64_t ge_ml(int64_t x, int64_t y) { return compare_ml(x, y) >= 0; }

int64_t l_ml(int64_t x, int64_t y) { return compare_ml(x, y) == -1; }
int64_t le_ml(int64_t x, int64_t y) { return compare_ml(x, y) <= 0; }

void print_int(int64_t a) {
    DEBUG_RUN(if (is_ml_ptr(a)) EXCEPTION_FMT("Debug \"print_int: get boxed arg\""););
    printf("%ld", CONVERT_INT_ML_TO_NATIVE(a));
    fflush(stdout);
}