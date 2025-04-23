#define _GNU_SOURCE
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <ffi.h>

#define MAX_APPS 100
#define MAX_ARGS 16

struct Func {
    uint8_t argscnt;
    uint8_t cnt;
    void *ptr;
    int64_t *argsfun;
    ffi_cif *cif;
    ffi_type **arg_types;
    void **arg_values;
};

struct Func func_init(void *ptr, uint8_t cnt) {
    struct Func new;
    new.ptr = ptr;
    new.argscnt = cnt;
    new.cnt = 0;
    
    new.argsfun = malloc(sizeof(int64_t) * cnt);
    if (!new.argsfun) {
        fprintf(stderr, "Memory allocation failed for arguments!\n");
        exit(1);
    }
    
    new.cif = malloc(sizeof(ffi_cif));
    if (!new.cif) {
        fprintf(stderr, "Memory allocation failed for FFI CIF!\n");
        free(new.argsfun);
        exit(1);
    }
    
    new.arg_types = malloc(sizeof(ffi_type*) * (cnt + 1)); // +1 for return type
    if (!new.arg_types) {
        fprintf(stderr, "Memory allocation failed for argument types!\n");
        free(new.argsfun);
        free(new.cif);
        exit(1);
    }
    
    new.arg_values = malloc(sizeof(void*) * cnt);
    if (!new.arg_values) {
        fprintf(stderr, "Memory allocation failed for argument values!\n");
        free(new.argsfun);
        free(new.cif);
        free(new.arg_types);
        exit(1);
    }
    
    for (int i = 0; i < cnt; i++) {
        new.arg_types[i] = &ffi_type_sint64;
    }
    new.arg_types[cnt] = NULL;
    
    ffi_status status = ffi_prep_cif(new.cif, FFI_DEFAULT_ABI, cnt, &ffi_type_sint64, new.arg_types);
    if (status != FFI_OK) {
        fprintf(stderr, "Failed to prepare FFI call interface: %d\n", status);
        free(new.argsfun);
        free(new.cif);
        free(new.arg_types);
        free(new.arg_values);
        exit(1);
    }
    
    return new;
}

struct Func copy_func(const struct Func *original) {
    struct Func copy;

    copy.argscnt = original->argscnt;
    copy.cnt = original->cnt;

    copy.ptr = original->ptr;

    copy.argsfun = malloc(sizeof(int64_t) * original->argscnt);
    if (!copy.argsfun) {
        fprintf(stderr, "Memory allocation failed for argsfun!\n");
    }
    memcpy(copy.argsfun, original->argsfun, sizeof(int64_t) * original->argscnt);

    copy.cif = malloc(sizeof(ffi_cif));
    if (!copy.cif) {
        fprintf(stderr, "Memory allocation failed for cif!\n");
        free(copy.argsfun);
        exit(1);
    }
    memcpy(copy.cif, original->cif, sizeof(ffi_cif));

    copy.arg_types = malloc(sizeof(ffi_type*) * (original->argscnt + 1));
    if (!copy.arg_types) {
        fprintf(stderr, "Memory allocation failed for arg_types!\n");
        free(copy.argsfun);
        free(copy.cif);
        exit(1);
    }
    for (int i = 0; i < original->argscnt; i++) {
        copy.arg_types[i] = original->arg_types[i];
    }
    copy.arg_types[original->argscnt] = NULL;

    copy.arg_values = malloc(sizeof(void*) * original->argscnt);
    if (!copy.arg_values) {
        fprintf(stderr, "Memory allocation failed for arg_values!\n");
        free(copy.argsfun);
        free(copy.cif);
        free(copy.arg_types);
        exit(1);
    }
    memcpy(copy.arg_values, original->arg_values, sizeof(void*) * original->argscnt);

    return copy;
}

void func_free(struct Func *f) {
    if (f) {
        free(f->argsfun);
        free(f->cif);
        free(f->arg_types);
        free(f->arg_values);
    }
}

struct Func *part_apps;
uint8_t *used_apps;
uint16_t last_app = 0;

int64_t app_n(struct Func *f) {
    if (f == NULL || f->ptr == NULL) {
        fprintf(stderr, "Error: NULL pointer in app_n function\n");
        return -1;
    }
    
    for (int i = 0; i < f->argscnt; i++) {
        f->arg_values[i] = &f->argsfun[i];
    }
    
    int64_t result;
    ffi_call(f->cif, FFI_FN(f->ptr), &result, f->arg_values);
    
    return result;
}

int64_t app(struct Func *f, uint8_t cnt, int64_t *args) {
    // fprintf(stdout, "Warning: %p(%ld) [%d %d]", f->ptr, (int64_t)f, f->argscnt, f->cnt);
    // if (cnt > 0) {
    //     fprintf(stdout, " -> %ld\n", args[0]);
    // } else {
    //     fprintf(stdout, "\n");
    // }

    if (f == NULL || args == NULL) {
        fprintf(stderr, "Error: NULL pointer in app function\n");
        return -1;
    }
    
    uint8_t f_cnt = f->cnt;
    uint8_t new_cnt = f_cnt + cnt;
    
    for (int i = f_cnt; i < new_cnt && i < f->argscnt; i++) {
        f->argsfun[i] = args[i - f_cnt];
    }
    
    f->cnt = (new_cnt < f->argscnt) ? new_cnt : f->argscnt;
    
    if (f->cnt >= f->argscnt) {
        int64_t ret = app_n(f);
        
        if (new_cnt > f->argscnt) {
            // fprintf(stdout, "Warning: overflow args\n");
            int64_t new_args[MAX_ARGS];
            for (int i = 0; i < new_cnt - f->argscnt && i < MAX_ARGS; i++) {
                new_args[i] = args[i + (f->argscnt - f_cnt)];
            }
            
            struct Func *new_f = &part_apps[last_app];
            *new_f = func_init(f->ptr, f->argscnt);
            last_app = (last_app + 1) % MAX_APPS;
            
            return app(new_f, new_cnt - f->argscnt, new_args);
        }
        
        return ret;
    }
    
    return (int64_t)f;
}

int64_t part_app(void *f_ptr, int argcnt, int appcnt, ...) {
    int64_t args[MAX_ARGS];
    va_list argptr;
    va_start(argptr, appcnt);
    
    for (int i = 0; i < appcnt && i < MAX_ARGS; i++) {
        args[i] = va_arg(argptr, int64_t);
    }
    va_end(argptr);
    
    if (f_ptr == NULL) {
        fprintf(stderr, "Error: NULL function pointer\n");
        return -1;
    }
    int app_idx = 0;
    
    if ((int64_t)&part_apps[0] <= (int64_t)f_ptr && (int64_t)f_ptr <= (int64_t)&part_apps[MAX_APPS-1]) {
        // part_apps[last_app] = copy_func(f_ptr);
        // used_apps[last_app] = 1;
        // app_idx = last_app;
        app_idx = ((int64_t)f_ptr - (int64_t)&part_apps[0]) / sizeof(struct Func);
    } else {
        part_apps[last_app] = func_init(f_ptr, argcnt);
        used_apps[last_app] = 1;
        app_idx = last_app;
    }
    
    last_app = (last_app + 1) % MAX_APPS;
    
    int64_t ret = app(&part_apps[app_idx], appcnt, args);
    // fprintf(stdout, "Result: %ld\n", ret);
    return ret;
}

void init_part_apps() {
    part_apps = malloc(sizeof(struct Func) * MAX_APPS);
    if (!part_apps) {
        fprintf(stderr, "Failed to allocate memory for part_apps\n");
        exit(1);
    }
    used_apps = malloc(sizeof(uint8_t) * MAX_APPS);
    if (!used_apps) {
        fprintf(stderr, "Failed to allocate memory for used_apps\n");
        exit(1);
    }
    
    for (int i = 0; i < MAX_APPS; i++) {
        part_apps[i].ptr = NULL;
        part_apps[i].argsfun = NULL;
        part_apps[i].argscnt = 0;
        part_apps[i].cnt = 0;
        part_apps[i].cif = NULL;
        part_apps[i].arg_types = NULL;
        part_apps[i].arg_values = NULL;
        used_apps[i] = 0;
    }
}

void cleanup_part_apps() {
    if (part_apps) {
        for (int i = 0; i < MAX_APPS; i++) {
            if (used_apps[i]) {
                func_free(&part_apps[i]);
            }
        }
        free(part_apps);
        free(used_apps);
    }
}

#include <stdio.h>
void print_int2(int number) {
    fprintf(stdout, "%d", number);
}


// int many_arg(int n, int n1, int n2, int n3, int n4, int n5, int n6, int n7, int n8, int n9, int n10, int n11, int n12, int n13) {
//     int ret = n + n1 + n3 + (n4/n2) + n5 + n6 + n7 + n8 + n9 + n10 + n11 * n12 * n13;
//     return ret % 256;
// }


// int fun(int a, int b) {
//     return (10 * a + b);
// }

// int notmain() {
//     return many_arg(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13);
// }

// #include <stdio.h>

// int not_main() {
//     init_part_apps();
//     int64_t a = part_app(notmain, 0, 0);
//     int64_t m = part_app(many_arg, 14, 0);
//     int64_t m2 = part_app(m, 14, 1, 0);
//     int64_t m3 = part_app(m2, 14, 6, 1, 2, 3, 4, 5, 6);
//     int64_t m4 = part_app(m3, 0, 7, 7, 8, 9, 10, 11, 12, 13);
//     printf("%d %d %d %d %d\n", a, m, m2, m3, m4);
//     cleanup_part_apps();
//     return 0;
// }
