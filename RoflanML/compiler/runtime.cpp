#include <ffi.h>
#include <stdio.h>

#include <cstdarg>
#include <cstdint>
#include <stdexcept>
#include <variant>
#include <vector>

using Ptr = void*;

using Int = int64_t;
using Bool = bool;
using Unit = Ptr;
struct Closure;

using RoflanMLObject = std::variant<Int, bool, Unit, Closure*>;

// Stdlib print functions

extern "C" RoflanMLObject* print_int(RoflanMLObject* obj) {
    printf("%ld\n", std::get<Int>(*obj));
    fflush(stdout);
    return new RoflanMLObject(Unit{nullptr});
}

extern "C" RoflanMLObject* print_bool(RoflanMLObject* obj) {
    printf(std::get<Bool>(*obj) ? "true\n" : "false\n");
    fflush(stdout);
    return new RoflanMLObject(Unit{nullptr});
}

// Boxed creation

extern "C" RoflanMLObject* Create_int(Int v) { return new RoflanMLObject(v); }
extern "C" RoflanMLObject* Create_bool(Bool v) { return new RoflanMLObject(v); }
extern "C" RoflanMLObject* Create_unit() {
    return new RoflanMLObject(Unit{nullptr});
}

// Boxed gettes

extern "C" Int Get_int(RoflanMLObject* obj) { return std::get<Int>(*obj); }
extern "C" Bool Get_bool(RoflanMLObject* obj) { return std::get<Bool>(*obj); }

// Comparison operators

extern "C" RoflanMLObject* RoflanML_eq(RoflanMLObject* left,
                                       RoflanMLObject* right) {
    return Create_bool(*left == *right);
}

extern "C" RoflanMLObject* RoflanML_neq(RoflanMLObject* left,
                                        RoflanMLObject* right) {
    return Create_bool(*left != *right);
}

extern "C" RoflanMLObject* RoflanML_gt(RoflanMLObject* left,
                                       RoflanMLObject* right) {
    if (std::holds_alternative<Int>(*left) &&
        std::holds_alternative<Int>(*right)) {
        return Create_bool(std::get<Int>(*left) > std::get<Int>(*right));
    }
    if (std::holds_alternative<Bool>(*left) &&
        std::holds_alternative<Bool>(*right)) {
        return Create_bool(std::get<Bool>(*left) > std::get<Int>(*right));
    }

    throw std::runtime_error(
        "RoflanML_gt works only with bool or integer types");
}

extern "C" RoflanMLObject* RoflanML_ge(RoflanMLObject* left,
                                       RoflanMLObject* right) {
    if (std::holds_alternative<Int>(*left) &&
        std::holds_alternative<Int>(*right)) {
        return Create_bool(std::get<Int>(*left) >= std::get<Int>(*right));
    }
    if (std::holds_alternative<Bool>(*left) &&
        std::holds_alternative<Bool>(*right)) {
        return Create_bool(std::get<Bool>(*left) >= std::get<Int>(*right));
    }

    throw std::runtime_error(
        "RoflanML_ge works only with bool or integer types");
}

extern "C" RoflanMLObject* RoflanML_lt(RoflanMLObject* left,
                                       RoflanMLObject* right) {
    if (std::holds_alternative<Int>(*left) &&
        std::holds_alternative<Int>(*right)) {
        return Create_bool(std::get<Int>(*left) < std::get<Int>(*right));
    }
    if (std::holds_alternative<Bool>(*left) &&
        std::holds_alternative<Bool>(*right)) {
        return Create_bool(std::get<Bool>(*left) < std::get<Int>(*right));
    }

    throw std::runtime_error(
        "RoflanML_lt works only with bool or integer types");
}

extern "C" RoflanMLObject* RoflanML_le(RoflanMLObject* left,
                                       RoflanMLObject* right) {
    if (std::holds_alternative<Int>(*left) &&
        std::holds_alternative<Int>(*right)) {
        return Create_bool(std::get<Int>(*left) <= std::get<Int>(*right));
    }
    if (std::holds_alternative<Bool>(*left) &&
        std::holds_alternative<Bool>(*right)) {
        return Create_bool(std::get<Bool>(*left) <= std::get<Int>(*right));
    }

    throw std::runtime_error(
        "RoflanML_le works only with bool or integer types");
}

// Boolean binary operators

extern "C" RoflanMLObject* RoflanML_or(RoflanMLObject* left,
                                       RoflanMLObject* right) {
    return Create_bool(std::get<Bool>(*left) || std::get<Bool>(*right));
}

extern "C" RoflanMLObject* RoflanML_and(RoflanMLObject* left,
                                        RoflanMLObject* right) {
    return Create_bool(std::get<Bool>(*left) && std::get<Bool>(*right));
}

// Integer binary operators

extern "C" RoflanMLObject* RoflanML_add(RoflanMLObject* left,
                                        RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) + std::get<Int>(*right));
}

extern "C" RoflanMLObject* RoflanML_sub(RoflanMLObject* left,
                                        RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) - std::get<Int>(*right));
}

extern "C" RoflanMLObject* RoflanML_mul(RoflanMLObject* left,
                                        RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) * std::get<Int>(*right));
}

extern "C" RoflanMLObject* RoflanML_div(RoflanMLObject* left,
                                        RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) / std::get<Int>(*right));
}

struct Closure {
    Ptr const function;
    uint32_t arity;
    std::vector<RoflanMLObject*> args;
};

extern "C" RoflanMLObject* Create_closure(Ptr function, uint32_t arity) {
    return new RoflanMLObject(new Closure{function, arity, {}});
}

extern "C" RoflanMLObject* Apply(RoflanMLObject* closure, ...) {
    va_list args;
    va_start(args, closure);
    int64_t number_of_args = va_arg(args, int64_t);

    auto as_closure_type = *std::get<Closure*>(*closure);

    auto new_closure = new Closure{as_closure_type.function,
                                   as_closure_type.arity, as_closure_type.args};

    RoflanMLObject* apply_result = nullptr;
    // As we already typechecked, we can be sure that only last result will be
    // not closure
    for (int i = 0; i < number_of_args; ++i) {
        new_closure->args.push_back(va_arg(args, RoflanMLObject*));

        if (new_closure->args.size() < new_closure->arity) {
            continue;
        }

        ffi_cif cif;
        auto arg_types = new ffi_type*[new_closure->arity];
        for (int i = 0; i < new_closure->arity; ++i) {
            arg_types[i] = &ffi_type_pointer;
        }
        auto args_ptrs = new void*[new_closure->arity];
        for (int i = 0; i < new_closure->arity; ++i) {
            args_ptrs[i] = &new_closure->args[i];
        }

        if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, new_closure->arity,
                         &ffi_type_pointer, arg_types) != FFI_OK) {
            throw std::runtime_error("Failed ffi_prep_cif");
        }
        ffi_call(&cif, FFI_FN(new_closure->function), &apply_result, args_ptrs);

        delete[] arg_types;
        delete[] args_ptrs;
        delete new_closure;

        if (i == number_of_args - 1) {
            return apply_result;
        }

        new_closure = std::get<Closure*>(*apply_result);
    }

    if (apply_result == nullptr) {
        apply_result = new RoflanMLObject(new_closure);
    }

    return apply_result;
}