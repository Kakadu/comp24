#include "ml_stdlib.h"
#include <unistd.h>

/*
let rec fib_cps n cont =
  match n with
  | 0 -> cont 0
  | 1 -> cont 1
  | _ ->
      fib_cps (n - 1) (fun a ->
          fib_cps (n - 2) (fun b ->
              cont (a + b)))
*/

int64_t fib_cps_c;
int64_t lambda1_c;
int64_t lambda2_c;
int64_t print_int_c;

int64_t lambda1_f(int64_t cont, int64_t n, int64_t a) {
    int64_t c = minus_mlint(n, CONVERT_INT_NATIVE_TO_ML(2));
    int64_t lambda_cont = mlrt_apply_args_to_closure(lambda2_c, 2, cont, a);
    return mlrt_apply_args_to_closure(fib_cps_c, 2, c, lambda_cont);
}

int64_t lambda2_f(int64_t cont, int64_t a, int64_t b) {
    int64_t c = plus_mlint(a, b);
    return mlrt_apply_args_to_closure(cont, 1, c);
}

int64_t fib_cps_f(int64_t n, int64_t cont) {
    if (CONVERT_INT_ML_TO_NATIVE(eq_ml(n, CONVERT_INT_NATIVE_TO_ML(0))))
        return mlrt_apply_args_to_closure(cont, 1, CONVERT_INT_NATIVE_TO_ML(0));
    else if (CONVERT_INT_ML_TO_NATIVE(eq_ml(n, CONVERT_INT_NATIVE_TO_ML(1))))
        return mlrt_apply_args_to_closure(cont, 1, CONVERT_INT_NATIVE_TO_ML(1));
    else {
        int64_t c = minus_mlint(n, CONVERT_INT_NATIVE_TO_ML(1));
        int64_t lambda_cont = mlrt_apply_args_to_closure(lambda1_c, 2, cont, n);
        return mlrt_apply_args_to_closure(fib_cps_c, 2, c, lambda_cont);
    }
}

int main() {
    print_int_c = mlrt_create_empty_closure((int64_t)print_int, 1);
    fib_cps_c = mlrt_create_empty_closure((int64_t)fib_cps_f, 2);
    lambda1_c = mlrt_create_empty_closure((int64_t)lambda1_f, 3);
    lambda2_c = mlrt_create_empty_closure((int64_t)lambda2_f, 3);

    for (int i = 0; i < 10; i++) {
        mlrt_apply_args_to_closure(fib_cps_c, 2, CONVERT_INT_NATIVE_TO_ML(i), print_int_c);
    }
}