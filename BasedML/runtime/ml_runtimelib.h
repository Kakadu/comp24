#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG
    #define DEBUG_RUN(code)                                                                                            \
        do {                                                                                                           \
            code                                                                                                       \
        } while (0)
#else
    #define DEBUG_RUN(code) // Empty macro - does nothing if DEBUG is not defined
#endif

#define EXIT_CODE_EXCEPTION -2

#define EXCEPTION_FMT(fmt, ...)                                                                                        \
    do {                                                                                                               \
        fprintf(stderr, "Exception: ");                                                                                \
        fprintf(stderr, fmt, ##__VA_ARGS__);                                                                           \
        fprintf(stderr, "\n");                                                                                         \
        exit(EXIT_CODE_EXCEPTION);                                                                                     \
    } while (0)

typedef enum {
    T_UNBOXED = -1,
    T_TUPLE = 0,
    T_CONS = 0,
    // T_FUNCTION = -1, // not used

    T_CLOSURE = 247,
    T_LAST_REAL_TAG = 255,
} tag_t;

// typedef enum { MLT_INT, MLT_BOOL, MLT_UNIT, MLT_FUNCTION, MLT_LAST_UNBOXED_TYPE, MLT_TUPLE, MLT_CONS } ml_type;
// not used

#pragma pack(push, 1)
typedef struct {
    int64_t size : 54; // in QWORDS, not as ocaml
    uint8_t color : 2;
    tag_t tag : 8;
} box_header_t;
typedef struct {
    box_header_t header;
    int64_t values[];
} box_t;
#pragma pack(pop)

#define CONVERT_INT_ML_TO_NATIVE(x) ((x) >> 1)
#define CONVERT_INT_NATIVE_TO_ML(x) (((x) << 1) + 1)
int8_t is_ml_ptr(int64_t arg);
tag_t get_tag(int64_t obj);

int64_t mlrt_create_cons(int64_t data, int64_t next);
int64_t mlrt_create_tuple(int64_t tuple_size, ...);
int64_t mlrt_create_empty_closure(int64_t fun, int64_t args_num);
int64_t mlrt_apply_args_to_closure(int64_t closure_box, int64_t new_args_num, ...);

int64_t mlrt_check_tag(int64_t target, int64_t tag);
int64_t mlrt_get_box_field(int64_t box, int64_t field_num);
void mltr_match_error();