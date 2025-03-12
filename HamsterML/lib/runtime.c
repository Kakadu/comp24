#include <ffi.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/******************** Error Handling ********************/

#define EXIT_CODE_EXCEPTION -2

#define EXCEPTION_FMT(fmt, ...)                                                \
  do {                                                                         \
    fprintf(stderr, "Exception: ");                                            \
    fprintf(stderr, fmt, ##__VA_ARGS__);                                       \
    fprintf(stderr, "\n");                                                     \
    exit(EXIT_CODE_EXCEPTION);                                                 \
  } while (0)

/******************** ML Value Types & Structures ********************/

typedef enum {
  T_UNBOXED = -1,
  T_TUPLE = 0,
  T_CONS = 0,
  T_CLOSURE = 247,
  T_LAST_REAL_TAG = 255,
} tag_t;

#pragma pack(push, 1)
typedef struct {
  int64_t size : 54; // in QWORDS
  uint8_t color : 2; // Retained for structure compatibility, unused
  tag_t tag : 8;
} box_header_t;

typedef struct {
  box_header_t header;
  int64_t values[];
} box_t;
#pragma pack(pop)

// List node structure
typedef struct {
  int64_t val;
  int64_t next;
} cons_t;

// Function closure structure
typedef struct {
  int64_t fun;
  int64_t args_num;
  int64_t args_applied;
  int64_t applied_args[];
} closure_t;

/******************** ML Value Conversion Macros ********************/

#define CONVERT_INT_ML_TO_NATIVE(x) ((x) >> 1)
#define CONVERT_INT_NATIVE_TO_ML(x) (((x) << 1) + 1)

/******************** Forward Declarations ********************/

int8_t is_ml_ptr(int64_t arg);
tag_t get_tag(int64_t obj);
box_t *create_box_t(size_t size);
int8_t compare(int64_t x, int64_t y);
int8_t compare_ml(int64_t x, int64_t y);
int8_t compare_boxed(int64_t x, int64_t y);
int64_t create_closure_by_src(box_t *src_box, int64_t new_args_num,
                              va_list *new_args);
int64_t call_closure(box_t *closure_box, int64_t new_args_num,
                     va_list *new_args);
int64_t apply_args_to_closure(box_t *closure_box, int64_t new_args_num,
                              va_list *new_args);

/******************** ML Runtime Core Functions ********************/

// Check if value is a pointer (boxed) or immediate value (unboxed)
int8_t is_ml_ptr(int64_t arg) { return !(arg & 1); }

// Allocate memory for ML values
void *ml_malloc(size_t size) { return malloc(size); }

// Create a boxed value with proper alignment
box_t *create_box_t(size_t size) {
  // Ensure 8-byte alignment
  if (size % 8 != 0)
    size += 8 - (size % 8);

  box_t *res_box = ml_malloc(size);
  res_box->header.color = 0; // Unused but kept for structure compatibility
  res_box->header.size = size / 8;
  return res_box;
}

// Get tag of ML value
tag_t get_tag(int64_t obj) {
  if (is_ml_ptr(obj)) {
    return ((box_t *)obj)->header.tag;
  } else {
    return T_UNBOXED;
  }
}

/******************** Function Closure Support ********************/

// Create a new empty closure
int64_t mlrt_create_empty_closure(int64_t fun, int64_t args_num) {
  box_t *closure_box = create_box_t(sizeof(box_header_t) + 0x18);
  closure_box->header.tag = T_CLOSURE;

  closure_t *clos = (closure_t *)&closure_box->values;
  clos->fun = fun;
  clos->args_applied = 0;
  clos->args_num = args_num;

  return (int64_t)closure_box;
}

// Create a new closure based on existing one with additional arguments
int64_t create_closure_by_src(box_t *src_box, int64_t new_args_num,
                              va_list *new_args) {
  box_t *closure_box = create_box_t((src_box->header.size + new_args_num) * 8);
  closure_box->header.tag = T_CLOSURE;

  closure_t *src_clos = (closure_t *)&src_box->values;
  closure_t *clos = (closure_t *)&closure_box->values;

  clos->fun = src_clos->fun;
  clos->args_applied = src_clos->args_applied + new_args_num;
  clos->args_num = src_clos->args_num;

  // Copy existing args and add new ones
  for (int i = 0; i < src_clos->args_applied + new_args_num; i++) {
    if (i < src_clos->args_applied)
      clos->applied_args[i] = src_clos->applied_args[i];
    else
      clos->applied_args[i] = va_arg(*new_args, int64_t);
  }

  return (int64_t)closure_box;
}

// Call a closure with arguments via FFI
int64_t call_closure(box_t *closure_box, int64_t new_args_num,
                     va_list *new_args) {
  closure_t *closure = (closure_t *)&closure_box->values;
  size_t args_count = closure->args_num;

  ffi_cif cif;
  ffi_type *arg_types[args_count];
  int64_t *args[args_count];

  int64_t buffer_new_args[new_args_num];

  // Set up argument types (all are int64_t)
  for (int i = 0; i < args_count; ++i) {
    arg_types[i] = &ffi_type_sint64;
    if (i < closure->args_applied)
      args[i] = &(closure->applied_args[i]);
    else {
      int na_num = i - closure->args_applied;
      buffer_new_args[na_num] = va_arg(*new_args, int64_t);
      args[i] = &(buffer_new_args[na_num]);
    }
  }

  int64_t res = 0;

  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args_count, &ffi_type_sint64,
                   arg_types) == FFI_OK) {
    ffi_call(&cif, (void *)closure->fun, &res, (void *)args);
  } else {
    EXCEPTION_FMT("call_closure: Failed to prepare call interface\n");
  }

  return res;
}

// Apply arguments to a closure, handling partial application
int64_t apply_args_to_closure(box_t *closure_box, int64_t new_args_num,
                              va_list *new_args) {
  closure_t *closure = (closure_t *)&closure_box->values;
  int64_t args_num_until_apply = closure->args_num - closure->args_applied;

  if (args_num_until_apply <= new_args_num) {
    // We have enough arguments to call the function
    int64_t call_res =
        call_closure(closure_box, args_num_until_apply, new_args);
    new_args_num -= args_num_until_apply;

    if (new_args_num == 0)
      return call_res;
    else
      return apply_args_to_closure((box_t *)call_res, new_args_num, new_args);
  } else {
    // Partial application - create a new closure with the additional arguments
    return create_closure_by_src(closure_box, new_args_num, new_args);
  }
}

// Public API for applying arguments to a closure
int64_t mlrt_apply_args_to_closure(int64_t closure_box, int64_t new_args_num,
                                   ...) {
  va_list new_args;
  va_start(new_args, new_args_num);
  int64_t res =
      apply_args_to_closure((box_t *)closure_box, new_args_num, &new_args);
  va_end(new_args);
  return res;
}

/******************** ML Box Access Functions ********************/

// Get field from boxed value
int64_t mlrt_get_box_field(int64_t box, int64_t field_num) {
  field_num = CONVERT_INT_ML_TO_NATIVE(field_num);
  if (!is_ml_ptr(box))
    return CONVERT_INT_NATIVE_TO_ML(0);
  else
    return ((box_t *)box)->values[field_num];
}

// Check tag of ML value
int64_t mlrt_check_tag(int64_t target, int64_t tag) {
  tag = CONVERT_INT_ML_TO_NATIVE(tag);
  return CONVERT_INT_NATIVE_TO_ML(tag == get_tag(target));
}

// Report pattern matching failure
void mltr_match_error() { EXCEPTION_FMT("Exception: Match_failure"); }

/******************** ML Standard Library ********************/

// Arithmetic operations
int64_t plus_mlint(int64_t x, int64_t y) { return x + y - 1; }
int64_t minus_mlint(int64_t x, int64_t y) { return x - y + 1; }
int64_t mult_mlint(int64_t x, int64_t y) { return (x >> 1) * (y - 1) + 1; }
int64_t div_mlint(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(CONVERT_INT_ML_TO_NATIVE(x) /
                                  CONVERT_INT_ML_TO_NATIVE(y));
}

// Comparison functions
int8_t compare(int64_t x, int64_t y) {
  if (x > y)
    return 1;
  else if (x < y)
    return -1;
  else
    return 0;
}

// Compare boxed values
int8_t compare_boxed(int64_t x, int64_t y) {
  box_t *xbox = (box_t *)x;
  box_t *ybox = (box_t *)y;

  if (xbox->header.tag == T_CLOSURE) {
    EXCEPTION_FMT("Invalid_argument \"compare: functional closure\"");
  } else {
    // Tuple or list, same algo for both
    int64_t res = 0;
    for (int i = 1; i < xbox->header.size; i++) {
      int val_num = i - 1;
      res = compare_ml(xbox->values[val_num], ybox->values[val_num]);
      if (res != 0)
        break;
    }
    return res;
  }
}

// ML value comparison
int8_t compare_ml(int64_t x, int64_t y) {
  tag_t x_tag = get_tag(x);
  tag_t y_tag = get_tag(y);

  int8_t tag_comp = compare(x_tag, y_tag);
  if (tag_comp == 0) {
    if (x_tag == T_UNBOXED) {
      // Unboxed values
      x = CONVERT_INT_ML_TO_NATIVE(x);
      y = CONVERT_INT_ML_TO_NATIVE(y);
      return compare(x, y);
    } else {
      return compare_boxed(x, y);
    }
  } else {
    return tag_comp;
  }
}

// Comparison operators
int64_t eq_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(compare_ml(x, y) == 0);
}
int64_t neq_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(compare_ml(x, y) != 0);
}
int64_t peq_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML((x == y));
}
int64_t pneq_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML((x != y));
}
int64_t g_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(compare_ml(x, y) == 1);
}
int64_t ge_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(compare_ml(x, y) >= 0);
}
int64_t l_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(compare_ml(x, y) == -1);
}
int64_t le_ml(int64_t x, int64_t y) {
  return CONVERT_INT_NATIVE_TO_ML(compare_ml(x, y) <= 0);
}

// I/O functions
int64_t print_int(int64_t a) {
  printf("%ld\n", CONVERT_INT_ML_TO_NATIVE(a));
  fflush(stdout);
  return CONVERT_INT_NATIVE_TO_ML(0);
}

// Bitwise operators
int64_t lor_ml(int64_t x, int64_t y) { return x | y; }
int64_t land_ml(int64_t x, int64_t y) { return x & y; }