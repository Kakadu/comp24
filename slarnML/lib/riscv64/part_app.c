#define _GNU_SOURCE
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>

// const uint16_t MAX_APPS = 100;
// const uint8_t MAX_ARGS = 4;
#define MAX_APPS 100
#define MAX_ARGS 4


int64_t min(int64_t a, int64_t b) {
    if (a < b) return a;
    else return b;
}

struct Func
{
    uint8_t argscnt;
    uint8_t cnt;
    void *ptr;
    int64_t *argsfun;
};
struct Func func_init(void *ptr, uint8_t cnt) {
    struct Func new;
    new.ptr = ptr;
    new.argscnt = cnt;
    new.argsfun = malloc(sizeof(int64_t)*min((int64_t)MAX_ARGS, cnt));
    if (!new.argsfun) {
        fprintf(stderr, "Memory allocation failed!");
        exit(1);
    }
    new.cnt = 0;
    return new;
}
struct Func *part_apps;
uint16_t last_app = 0;

int64_t app_n(struct Func *f) {
    switch ((*f).argscnt) {
    case 0:
        int64_t(*f_ptr0)();
        f_ptr0 = (*f).ptr;
        return f_ptr0();
    case 1:
        int64_t(*f_ptr1)(int64_t);
        f_ptr1 = (*f).ptr;
        return f_ptr1(f->argsfun[0]);
    case 2:
        int64_t(*f_ptr2)(int64_t, int64_t);
        f_ptr2 = (*f).ptr;
        return f_ptr2(f->argsfun[0], f->argsfun[1]);
    case 3:
        int64_t(*f_ptr3)(int64_t, int64_t, int64_t);
        f_ptr3 = (*f).ptr;
        return f_ptr3(f->argsfun[0], f->argsfun[1], f->argsfun[2]);
    case 4:
        int64_t(*f_ptr4)(int64_t, int64_t, int64_t, int64_t);
        f_ptr4 = (*f).ptr;
        return f_ptr4(f->argsfun[0], f->argsfun[1], f->argsfun[2], f->argsfun[3]);
    default:
        return -1;
    }
}

int64_t app(struct Func *f, uint8_t cnt, int64_t *args) {
    uint8_t f_cnt = f->cnt;
    for (; f->cnt < min(MAX_ARGS, min(f_cnt + cnt, f->argscnt)); f->cnt++) {
        f->argsfun[f->cnt] = args[f->cnt - f_cnt];
    }
    int64_t ret;
    if (f->cnt < f->argscnt) {
        return (int64_t)f;
    } else {
        ret = app_n(f);
    }
    if (f_cnt + cnt > f->argscnt) {
        int64_t new_args[MAX_ARGS];
        for (int i = f->argscnt - f_cnt; i < min(MAX_ARGS, cnt); i++) {
            new_args[i - (f->argscnt - f_cnt)] = args[i];
        }
        return app((void*)ret, f_cnt + cnt - f->argscnt, new_args);
    }
    else return ret;
}

int64_t part_app(void *f_ptr, int argcnt, int appcnt, ...) {
    int cnt = 0;
    int64_t args[MAX_ARGS];
    va_list argptr;
    va_start(argptr, appcnt);
    for (int i = 0; i < min(appcnt, MAX_ARGS); i++) {
        args[i] = va_arg(argptr, int64_t);
    }
    va_end(argptr);
    if ((int64_t)&part_apps[0] <= (int64_t)f_ptr && (int64_t)f_ptr <= (int64_t)&part_apps[MAX_APPS-1]) {
        part_apps[last_app] = *(struct Func *)f_ptr;
    } else {
        part_apps[last_app] = func_init(f_ptr, argcnt);
    }
    last_app = (last_app + 1) % MAX_APPS;
    return app(&part_apps[last_app-1], appcnt, args);
}

void init_part_apps() {
    part_apps = malloc(sizeof(struct Func) * MAX_APPS);
}

int many_arg(int n,int n1,int n2,int n3,int n4,int n5,int n6,int n7,int n8,int n9,int n10,int n11,int n12,int n13) {
    int ret = n+n1+n3+(n4/n2)+n5+n6+n7+n8+n9+n10+n11*n12*n13;
    return ret % 256;
}

int fun ( int a, int b)
{
    return(10*a+b);
}

int notmain ()
{
    return many_arg(0,1,2,3,4,5,6,7,8,9,10,11,12,13);
}

