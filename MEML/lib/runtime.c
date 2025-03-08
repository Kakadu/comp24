#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct PartApp
{
    int64_t *fn;
    int tcount;
    int acount;
    int64_t *args;
} *Closure;

static int64_t args0(Closure p)
{
    return ((int64_t(*)())p->fn)();
}

static int64_t args1(Closure p)
{
    return ((int64_t(*)(int64_t))p->fn)(p->args[0]);
}

static int64_t args2(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t))p->fn)(p->args[0], p->args[1]);
}

static int64_t args3(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2]);
}

static int64_t args4(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3]);
}

static int64_t args5(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4]);
}

static int64_t args6(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5]);
}

static int64_t args7(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6]);
}

static int64_t args8(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7]);
}

static int64_t args9(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8]);
}

static int64_t args10(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9]);
}

static int64_t args11(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10]);
}

static int64_t args12(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11]);
}

static int64_t args13(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12]);
}

static int64_t args14(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13]);
}

static int64_t args15(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14]);
}

static int64_t args16(Closure p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15]);
}

static int64_t (*argsrs[17])(Closure) = {
    args0,
    args1,
    args2,
    args3,
    args4,
    args5,
    args6,
    args7,
    args8,
    args9,
    args10,
    args11,
    args12,
    args13,
    args14,
    args15,
    args16,
};


Closure newClosure(int64_t *fn, int tcount)
{
    Closure p = (Closure)malloc(sizeof(struct PartApp));
    p->fn = fn;
    p->tcount = tcount;
    p->acount = 0;
    p->args = (int64_t *)malloc(sizeof(int64_t) * tcount);
    return p;
}

Closure fromClosure(Closure p)
{
    Closure p2 = (Closure)malloc(sizeof(struct PartApp));
    p2->fn = p->fn;
    p2->tcount = p->tcount;
    p2->acount = p->acount;
    p2->args = (int64_t *)malloc(sizeof(int64_t) * p->tcount);
    for (int i = 0; i < p->acount; i++)
    {
        p2->args[i] = p->args[i];
    }
    return p2;
}

int64_t appClosure(int64_t pointer, int64_t arg)
{
    Closure p = (Closure)pointer;
    // Create new partitial application structure for every application
    Closure new = fromClosure(p);

    new->args[p->acount] = arg;
    new->acount++;
    if (new->acount == new->tcount)
    {
        int64_t (*fn)(Closure) = argsrs[new->tcount];
        int64_t res = fn(new);
        free(new->args);
        free(new);
        return res;
    }
    else
    {
        if (new->acount > new->tcount)
        {
            printf("Error: too many arguments applied\n");
            exit(1);
        }
        
    }
    return (int64_t)new;
}

int64_t addInClosure(int64_t func, int64_t tcount)
{
    int64_t* fn = (int64_t*)func;
    Closure new = NULL;
    if (tcount > 16)
    {
        printf("Error: сannot create a function with more than 16 arguments");
        exit(1);
    }
    new = newClosure(fn, tcount);
    if (tcount == 0){
        int64_t (*fn)(Closure) = argsrs[new->tcount];
        int64_t res = fn(new);
        free(new->args);
        free(new);
        return res;
    }
    return (int64_t) new;
}

int64_t print_int(int64_t x)
{
    printf("%ld\n", x);
    return 0;
}
