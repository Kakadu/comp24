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

typedef struct {
    int64_t val;
    struct list_t* next;
} list_t;

#define CONVERT_INT_ML_TO_NATIVE(x) ((x) >> 1)
#define CONVERT_INT_NATIVE_TO_ML(x) (((x) << 1) + 1)
int8_t is_ml_ptr(int64_t arg);
int8_t is_inside_heap(int64_t ptr);
box_t* get_box_t(int64_t ptr);
