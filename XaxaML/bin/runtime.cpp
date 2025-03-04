#include <ffi.h>
#include <vector>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <utility>
#include <cstdarg> 

enum ValueTag
{
    VAL_INT,
    VAL_BOOL,
    VAL_UNIT,
    VAL_CLOSURE,
    VAL_LIST,
    VAL_TUPLE
};

struct Closure;
typedef void *FunctionPtr;

struct Value
{
    ValueTag tag;
    union
    {
        int64_t as_int;
        bool as_bool;
        Closure *as_closure;
        std::vector<Value *> *as_list;
        std::vector<Value *> *as_tuple;
        void *as_pointer;
    } data;

    static Value *Int(int64_t i)
    {
        return new Value{VAL_INT, {.as_int = i}};
    }

    static Value *Bool(bool b)
    {
        return new Value{VAL_BOOL, {.as_bool = b}};
    }

    static Value *Unit()
    {
        return new Value{VAL_UNIT, {.as_pointer = nullptr}};
    }

    static Value *Tuple(std::vector<Value *> vals)
    {
        return new Value{VAL_TUPLE, {.as_tuple = new std::vector<Value *>(vals)}};
    }

    static Value *List(std::vector<Value *> vals)
    {
        return new Value{VAL_LIST, {.as_list = new std::vector<Value *>(vals)}};
    }

    int64_t Int() const
    {
        if (tag != VAL_INT)
            throw std::runtime_error("Expected int");
        return data.as_int;
    }

    bool Bool() const
    {
        if (tag != VAL_BOOL)
            throw std::runtime_error("Expected bool");
        return data.as_bool;
    }

    std::vector<Value *> &List() const
    {
        if (tag != VAL_LIST)
            throw std::runtime_error("Expected list");
        return *(data.as_list);
    }

    std::vector<Value *> &Tuple() const
    {
        if (tag != VAL_TUPLE)
            throw std::runtime_error("Expected tuple");
        return *(data.as_tuple);
    }
};

struct Closure
{
    FunctionPtr func_ptr;
    size_t total_args;
    std::vector<Value *> args;

    Closure *copy() const { return new Closure{func_ptr, total_args, args}; }
};

extern "C" Value *create_closure(FunctionPtr f, size_t arity)
{
    Closure *c = new Closure{f, arity, {}};
    return new Value{VAL_CLOSURE, {.as_closure = c}};
}


extern "C" Value *apply(Value *closure_val, Value *arg);


extern "C" Value *print_int(Value *v)
{
    std::cout << v->Int() << std::endl;
    return Value::Unit();
}

extern "C" Value *print_bool(Value *v)
{
    std::cout << (v->Bool() ? "true" : "false") << std::endl;
    return Value::Unit();
}

extern "C" Value *list_hd(Value *lst)
{
    auto &l = lst->List();
    if (l.empty())
        throw std::runtime_error("hd of empty list");
    return l.front();
}

extern "C" Value *list_tl(Value *lst)
{
    auto &l = lst->List();
    if (l.empty())
        throw std::runtime_error("tl of empty list");
    return Value::List({l.begin() + 1, l.end()});
}

extern "C" Value *list_length(Value *lst)
{
    return Value::Int(static_cast<int64_t>(lst->List().size()));
}

extern "C" Value *unpack_tuple(Value *tpl, Value *idx) {
    if (tpl->tag != VAL_TUPLE) {
        throw std::runtime_error("Expected tuple but got something else");
    }
    auto &t = tpl->Tuple();
    int64_t i = idx->Int();
    if (i < 0 || i >= static_cast<int64_t>(t.size())) {
        throw std::runtime_error("Tuple index out of bounds");
    }
    return t[i];
  }


extern "C" Value *match_failure(Value *, Value *)
{
    throw std::runtime_error("Match failure");
}

extern "C" Value *uplus(Value *a) { return Value::Int(a->Int()); }
extern "C" Value *uminus(Value *a) { return Value::Int(-a->Int()); }

#define BIN_OP_INT(name, op) \
    extern "C" Value *name(Value *a, Value *b) { return Value::Int(a->Int() op b->Int()); }

BIN_OP_INT(add, +)
BIN_OP_INT(sub, -)
BIN_OP_INT(mul, *)
BIN_OP_INT(div_op, /)

#define BIN_OP_BOOL(name, op) \
    extern "C" Value *name(Value *a, Value *b) { return Value::Bool(a->Bool() op b->Bool()); }

BIN_OP_BOOL(and_op, &&)
BIN_OP_BOOL(or_op, ||)

#define CMP_OP_INT(name, op) \
    extern "C" Value *name(Value *a, Value *b) { return Value::Bool(a->Int() op b->Int()); }

CMP_OP_INT(gt, >)
CMP_OP_INT(lt, <)
CMP_OP_INT(ge, >=)
CMP_OP_INT(le, <=)

extern "C" Value *eq(Value *a, Value *b)
{
    if (a->tag != b->tag)
        return Value::Bool(false);
    
    switch (a->tag) {
        case VAL_INT:
            return Value::Bool(a->Int() == b->Int());
        
        case VAL_BOOL:
            return Value::Bool(a->Bool() == b->Bool());
        
        case VAL_UNIT:
            return Value::Bool(true);
        
        case VAL_LIST: {
            auto &a_list = a->List();
            auto &b_list = b->List();
            
            if (a_list.size() != b_list.size())
                return Value::Bool(false);

            for (size_t i = 0; i < a_list.size(); i++) {
                if (!eq(a_list[i], b_list[i])->Bool())
                    return Value::Bool(false);
            }
            return Value::Bool(true);
        }
        default:
            return Value::Bool(a == b);
    }
}

extern "C" Value *ne(Value *a, Value *b) { return Value::Bool(!eq(a, b)->Bool()); }

extern "C" Value *phys_eq(Value *a, Value *b) { 
    switch (a->tag) {
        case VAL_INT:
        case VAL_BOOL:
        case VAL_UNIT:
            return Value::Bool(eq(a, b)->Bool());
        default:
            return Value::Bool(a == b);
    }
}

extern "C" Value *phys_ne(Value *a, Value *b) { return Value::Bool(!phys_eq(a, b)->Bool()); }

// Функции для создания базовых типов
extern "C" Value *create_int(int64_t i) {
    return Value::Int(i);
}

extern "C" Value *create_bool(bool b) {
    return Value::Bool(b);
}

extern "C" Value *create_unit() {
    return Value::Unit();
}

extern "C" int64_t get_int(Value *v) {
    return v->Int();
}

extern "C" bool get_bool(Value *v) {
    return v->Bool();
}

extern "C" Value *list_cons(Value *head, Value *tail) {
    std::vector<Value*> list = {head};
    if (tail->tag == VAL_LIST) {
        auto &tail_list = tail->List();
        list.insert(list.end(), tail_list.begin(), tail_list.end());
    }
    return Value::List(list);
}

extern "C" Value *create_empty_list() {
    return Value::List({});
}

extern "C" Value *create_tuple(int n, ...) {
    va_list args;
    va_start(args, n);
    std::vector<Value*> values;
    for (int i = 0; i < n; i++) {
        values.push_back(va_arg(args, Value*));
    }
    va_end(args);
    return Value::Tuple(values);
}

extern "C" Value *apply(Value *closure_val, Value *arg)
{
    if (closure_val->tag != VAL_CLOSURE)
        throw std::runtime_error("Expected closure");

    Closure *c = closure_val->data.as_closure->copy();
    c->args.push_back(arg);

    if (c->args.size() == c->total_args)
    {
        ffi_cif cif;
        std::vector<ffi_type *> arg_types(c->total_args, &ffi_type_pointer);
        std::vector<void *> arg_values(c->total_args);
        for (unsigned i = 0; i < c->total_args; ++i)
            arg_values[i] = &c->args[i];
        if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, c->total_args, &ffi_type_pointer, arg_types.data()) != FFI_OK)
            throw std::runtime_error("ffi_prep_cif failed");
        Value *result = nullptr;
        ffi_call(&cif, FFI_FN(c->func_ptr), &result, arg_values.data());
        delete c;
        return result;
    }
    else
    {
        return new Value{VAL_CLOSURE, {.as_closure = c}};
    }
}
