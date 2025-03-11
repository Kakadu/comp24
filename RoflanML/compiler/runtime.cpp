#include <ffi.h>
#include <stdio.h>

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

RoflanMLObject* print_int(RoflanMLObject* obj) {
    printf("%ld", std::get<Int>(*obj));
    fflush(stdout);
    return new RoflanMLObject(Unit());
}

RoflanMLObject* print_bool(RoflanMLObject* obj) {
    printf(std::get<Bool>(*obj) ? "true" : "false");
    fflush(stdout);
    return new RoflanMLObject(Unit());
}

// Boxed creation

RoflanMLObject* Create_int(Int v) { return new RoflanMLObject(v); }
RoflanMLObject* Create_bool(Bool v) { return new RoflanMLObject(v); }
RoflanMLObject* Create_unit() { return new RoflanMLObject(Unit{nullptr}); }

// Boxed gettes

Int Get_int(RoflanMLObject* obj) { return std::get<Int>(*obj); }
Bool Get_bool(RoflanMLObject* obj) { return std::get<Bool>(*obj); }

// Comparison operators

RoflanMLObject* RoflanML_eq(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_bool(*left == *right);
}

RoflanMLObject* RoflanML_neq(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_bool(*left != *right);
}

RoflanMLObject* RoflanML_gt(RoflanMLObject* left, RoflanMLObject* right) {
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

RoflanMLObject* RoflanML_ge(RoflanMLObject* left, RoflanMLObject* right) {
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

RoflanMLObject* RoflanML_lt(RoflanMLObject* left, RoflanMLObject* right) {
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

RoflanMLObject* RoflanML_le(RoflanMLObject* left, RoflanMLObject* right) {
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

RoflanMLObject* RoflanML_or(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_bool(std::get<Bool>(*left) || std::get<Bool>(*right));
}

RoflanMLObject* RoflanML_and(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_bool(std::get<Bool>(*left) && std::get<Bool>(*right));
}

// Integer binary operators

RoflanMLObject* RoflanML_add(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) + std::get<Int>(*right));
}

RoflanMLObject* RoflanML_sub(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) - std::get<Int>(*right));
}

RoflanMLObject* RoflanML_mul(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) * std::get<Int>(*right));
}

RoflanMLObject* RoflanML_div(RoflanMLObject* left, RoflanMLObject* right) {
    return Create_int(std::get<Int>(*left) / std::get<Int>(*right));
}

struct Closure {
    Ptr const function;
    uint32_t arity;
    std::vector<Ptr> args;
};

RoflanMLObject* Create_closure(Ptr function, uint32_t arity) {
    return new RoflanMLObject(new Closure{function, arity, {}});
}

RoflanMLObject* Apply(RoflanMLObject* closure, RoflanMLObject* arg) {
    auto as_closure_type = *std::get<Closure*>(*closure);
    auto new_closure = new Closure{as_closure_type.function,
                                   as_closure_type.arity, as_closure_type.args};
    new_closure->args.push_back(static_cast<Ptr>(arg));

    if (new_closure->args.size() != new_closure->arity) {
        return new RoflanMLObject(new_closure);
    }

    ffi_cif cif;
    auto arg_types = new ffi_type* [new_closure->arity] { &ffi_type_pointer };
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, new_closure->arity,
                     &ffi_type_pointer, arg_types) != FFI_OK) {
        throw std::runtime_error("Failed to apply argument");
    }
    RoflanMLObject* apply_result = nullptr;
    ffi_call(&cif, FFI_FN(new_closure->function), &apply_result,
             new_closure->args.data());

    delete arg_types;
    delete new_closure;
    return apply_result;
}