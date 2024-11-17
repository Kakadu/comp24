#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
 
 
typedef struct PartAppl {
    int64_t *fn;
    int tcount;
    int acount;
    int64_t *args;
} *PAppli;
 
static int64_t invoke0(PAppli p) {
    return ((int64_t(*)()) p->fn)();
}
 
static int64_t invoke1(PAppli p) {
    return ((int64_t(*)(int64_t)) p->fn)(p->args[0]);
}
 
static int64_t invoke2(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t)) p->fn)(p->args[0], p->args[1]);
}
 
static int64_t invoke3(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t)) p->fn)(p->args[0], p->args[1], p->args[2]);
}
 
static int64_t invoke4(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t)) p->fn)(p->args[0], p->args[1], p->args[2], p->args[3]);
}
 
static int64_t invoke5(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t)) p->fn)(
        p->args[0], p->args[1], p->args[2], p->args[3], p->args[4]);
}
 
static int64_t invoke6(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) p->fn)(
        p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5]);
}
 
static int64_t invoke7(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) p->fn)(
        p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6]);
}
 
static int64_t invoke8(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) p->fn)(
        p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7]);
}
 
static int64_t invoke9(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) p->fn)(
        p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8]);
}
 
static int64_t invoke10(PAppli p) {
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) p->
        fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8],
            p->args[9]);
}
 
static int64_t (*invokers[11])(PAppli) = {
    invoke0,
    invoke1,
    invoke2,
    invoke3,
    invoke4,
    invoke5,
    invoke6,
    invoke7,
    invoke8,
    invoke9,
    invoke10
};
 
PAppli newPAppli(int64_t *fn, int tcount) {
    PAppli p = malloc(sizeof(struct PartAppl));
    p->fn = fn;
    p->tcount = tcount;
    p->acount = 0;
    p->args = (int64_t *) malloc(sizeof(int64_t) * tcount);
    return p;
}
 
PAppli fromPAppli(PAppli p) {
    PAppli p2 = malloc(sizeof(struct PartAppl));
    p2->fn = p->fn;
    p2->tcount = p->tcount;
    p2->acount = p->acount;
    p2->args = (int64_t *) malloc(sizeof(int64_t) * p->tcount);
    for (int i = 0; i < p->acount; i++) {
        p2->args[i] = p->args[i];
    }
    return p2;
}
 
int64_t applyPAppli(int64_t pointer, int64_t arg) {
    PAppli p = (PAppli) pointer;
    PAppli new = fromPAppli(p);
 
    new->args[p->acount] = arg;
    new->acount++;
    if (new->acount == new->tcount) {
        int64_t (*fn)(PAppli) = invokers[new->tcount];
        int64_t res = fn(new);
        free(new->args);
        free(new);
        return res;
    }
    return (int64_t) new;
}
 
int64_t addNewPAppliClosure(int64_t func, int64_t tcount) {
    int64_t *fn = (int64_t *) func;
    PAppli new = NULL;
    if (tcount > 10) {
        printf("Error: сannot create a function with more than 10 arguments");
        exit(1);
    }
    new = newPAppli(fn, tcount);
    if (tcount == 0) {
        int64_t (*fn)(PAppli) = invokers[new->tcount];
        int64_t res = fn(new);
        free(new->args);
        free(new);
        return res;
    }
    return (int64_t) new;
}
 
int64_t print_int(int64_t x) {
    printf("%ld\n", x);
    return 0;
}
 
int64_t print_bool(int64_t x) {
    if (x == 0) {
        printf("false\n");
    } else {
        printf("true\n");
    }
    return 0;
}