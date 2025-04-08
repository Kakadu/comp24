#define _GNU_SOURCE
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <ffi.h>

// Maximum number of partial applications to store
#define MAX_APPS 100
// Maximum number of arguments per function
#define MAX_ARGS 16

// Structure to store function information
struct Func {
    uint8_t argscnt;      // Total number of arguments the function expects
    uint8_t cnt;          // Number of arguments already applied
    void *ptr;            // Function pointer
    int64_t *argsfun;     // Array of arguments
    ffi_cif *cif;         // FFI call interface
    ffi_type **arg_types; // FFI argument types
    void **arg_values;    // FFI argument values
};

// Initialize a new function structure
struct Func func_init(void *ptr, uint8_t cnt) {
    struct Func new;
    new.ptr = ptr;
    new.argscnt = cnt;
    new.cnt = 0;
    
    // Allocate memory for arguments
    new.argsfun = malloc(sizeof(int64_t) * cnt);
    if (!new.argsfun) {
        fprintf(stderr, "Memory allocation failed for arguments!\n");
        exit(1);
    }
    
    // Allocate memory for FFI structures
    new.cif = malloc(sizeof(ffi_cif));
    if (!new.cif) {
        fprintf(stderr, "Memory allocation failed for FFI CIF!\n");
        free(new.argsfun);
        exit(1);
    }
    
    // Allocate memory for argument types
    new.arg_types = malloc(sizeof(ffi_type*) * (cnt + 1)); // +1 for return type
    if (!new.arg_types) {
        fprintf(stderr, "Memory allocation failed for argument types!\n");
        free(new.argsfun);
        free(new.cif);
        exit(1);
    }
    
    // Allocate memory for argument values
    new.arg_values = malloc(sizeof(void*) * cnt);
    if (!new.arg_values) {
        fprintf(stderr, "Memory allocation failed for argument values!\n");
        free(new.argsfun);
        free(new.cif);
        free(new.arg_types);
        exit(1);
    }
    
    // Set up argument types (all int64_t)
    for (int i = 0; i < cnt; i++) {
        new.arg_types[i] = &ffi_type_sint64;
    }
    new.arg_types[cnt] = NULL; // Terminate the array
    
    // Prepare the FFI call interface
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

// Free resources associated with a function
void func_free(struct Func *f) {
    if (f) {
        free(f->argsfun);
        free(f->cif);
        free(f->arg_types);
        free(f->arg_values);
    }
}

// Global array to store partial applications
struct Func *part_apps;
uint8_t *used_apps;
uint16_t last_app = 0;

// Apply a function with its stored arguments
int64_t app_n(struct Func *f) {
    if (f == NULL || f->ptr == NULL) {
        fprintf(stderr, "Error: NULL pointer in app_n function\n");
        return -1;
    }
    
    // Set up argument values
    for (int i = 0; i < f->argscnt; i++) {
        f->arg_values[i] = &f->argsfun[i];
    }
    
    // Call the function using FFI
    int64_t result;
    ffi_call(f->cif, FFI_FN(f->ptr), &result, f->arg_values);
    
    return result;
}

// Apply arguments to a function
int64_t app(struct Func *f, uint8_t cnt, int64_t *args) {
    if (f == NULL || args == NULL) {
        fprintf(stderr, "Error: NULL pointer in app function\n");
        return -1;
    }
    
    uint8_t f_cnt = f->cnt;
    uint8_t new_cnt = f_cnt + cnt;
    
    // Store the new arguments
    for (int i = f_cnt; i < new_cnt && i < f->argscnt; i++) {
        f->argsfun[i] = args[i - f_cnt];
    }
    
    // Update the count of applied arguments
    f->cnt = (new_cnt < f->argscnt) ? new_cnt : f->argscnt;
    
    // If we have all arguments, call the function
    if (f->cnt >= f->argscnt) {
        int64_t ret = app_n(f);
        
        // If there are more arguments than needed, create a new partial application
        if (new_cnt > f->argscnt) {
            int64_t new_args[MAX_ARGS];
            for (int i = 0; i < new_cnt - f->argscnt && i < MAX_ARGS; i++) {
                new_args[i] = args[i + (f->argscnt - f_cnt)];
            }
            
            // Create a new function with the remaining arguments
            struct Func *new_f = &part_apps[last_app];
            *new_f = func_init(f->ptr, f->argscnt);
            last_app = (last_app + 1) % MAX_APPS;
            
            return app(new_f, new_cnt - f->argscnt, new_args);
        }
        
        return ret;
    }
    
    // Return the function pointer as an integer
    return (int64_t)f;
}

// Main function for partial application
int64_t part_app(void *f_ptr, int argcnt, int appcnt, ...) {
    int64_t args[MAX_ARGS];
    va_list argptr;
    va_start(argptr, appcnt);
    
    // Get the arguments from the variable argument list
    for (int i = 0; i < appcnt && i < MAX_ARGS; i++) {
        args[i] = va_arg(argptr, int64_t);
    }
    va_end(argptr);
    
    // Check if f_ptr is a valid function pointer
    if (f_ptr == NULL) {
        fprintf(stderr, "Error: NULL function pointer\n");
        return -1;
    }
    
    // Check if f_ptr is within the part_apps array
    if ((int64_t)&part_apps[0] <= (int64_t)f_ptr && (int64_t)f_ptr <= (int64_t)&part_apps[MAX_APPS-1]) {
        // If it's already a partial application, copy it
        part_apps[last_app] = *(struct Func *)f_ptr;
    } else {
        // Initialize a new function
        part_apps[last_app] = func_init(f_ptr, argcnt);
        used_apps[last_app] = 1;
    }
    
    // Update last_app and ensure it's within bounds
    last_app = (last_app + 1) % MAX_APPS;
    
    // Call the function with the arguments
    return app(&part_apps[last_app-1], appcnt, args);
}

// Initialize the partial applications array
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
    
    // Initialize all function pointers to NULL
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

// Clean up resources
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

// // Example function with many arguments
// int many_arg(int n, int n1, int n2, int n3, int n4, int n5, int n6, int n7, int n8, int n9, int n10, int n11, int n12, int n13) {
//     int ret = n + n1 + n3 + (n4/n2) + n5 + n6 + n7 + n8 + n9 + n10 + n11 * n12 * n13;
//     return ret % 256;
// }

// // Example function with two arguments
// int fun(int a, int b) {
//     return (10 * a + b);
// }

// // Example function with no arguments
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
