#include <algorithm>
#include <cinttypes>
#include <cstdarg>
#include <ffi.h>
#include <format>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <ranges>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

void add_to_tuple();
class Value;
extern "C" Value *create_tuple(int count, ...);
extern "C" Value *create_bool(bool v);

class Value {
public:
  class ValuePtr : public std::unique_ptr<Value> {
  public:
    explicit ValuePtr(Value *val) : std::unique_ptr<Value>(val) {}

    ValuePtr(const ValuePtr &val)
        : std::unique_ptr<Value>(val ? std::make_unique<Value>(*val)
                                     : std::unique_ptr<Value>()) {}
    bool operator==(const ValuePtr &b) const {
      if (!(this->get() && b.get()))
        return false;
      return (*this)->operator==(*b);
    }
  };

  using TupleType = std::vector<ValuePtr>;

  struct FuncObject {
    void *func;
    size_t args_count;
    std::vector<std::shared_ptr<Value>> args;
    FuncObject(void *func, size_t args_count)
        : func(func), args_count(args_count) {
      args.reserve(args_count);
    }
    [[nodiscard]] bool can_call() const { return args_count == args.size(); }

    bool operator==(const FuncObject &) const { return false; }
  };

  class ConstructorT {
    using option = std::optional<ValuePtr>;
    using list = std::pair<ValuePtr, ValuePtr>;
    std::variant<option, list> args;

  public:
    enum ConsType {
      None = 0,
      Some = 1,
      ListAdd = 2,
      EmptyList = 3,
    };

    ConstructorT() = default;

    ConstructorT(ConsType name, Value *param) {
      switch (name) {
      case ConsType::None:
        args.emplace<option>();
        return;
      case ConsType::Some:
        args.emplace<option>(ValuePtr(param));
        return;
      case ConsType::ListAdd: {
        auto &params = param->get_tuple();
        args.emplace<list>(params[0], params[1]);
        return;
      }
      case ConsType::EmptyList:
        args.emplace<list>(ValuePtr(nullptr), ValuePtr(nullptr));
        return;
      default:
        throw std::invalid_argument(
            std::format("Unknown constructor: {}", static_cast<int>(name)));
      }
    }

    Value *disassemble() {
      return std::visit(overloaded{[&](list &list) {
                                     if (!list.first) {
                                       throw std::runtime_error(
                                           "Try to disassemble empty list");
                                     }
                                     return create_tuple(
                                         2, ValuePtr(list.first).release(),
                                         ValuePtr(list.second).release());
                                   },
                                   [&](option &option) {
                                     return ValuePtr(option.value()).release();
                                   }},
                        args);
    }

    Value *same_cons(ConsType name) {
      return std::visit(overloaded{[&](list &arg) {
                                     switch (name) {
                                     case EmptyList:
                                       return create_bool(!arg.second.get());
                                     case ListAdd:
                                       return create_bool(arg.second.get());
                                     default:
                                       throw std::invalid_argument(std::format(
                                           "Unknown constructor: {}",
                                           static_cast<int>(name)));
                                     }
                                   },
                                   [&](option &arg) {
                                     switch (name) {
                                     case None:
                                       return create_bool(!arg);
                                     case Some:
                                       return create_bool(arg.has_value());
                                     default:
                                       throw std::invalid_argument(std::format(
                                           "Unknown constructor: {}",
                                           static_cast<int>(name)));
                                     }
                                   }},
                        args);
    }

    bool operator==(const ConstructorT &b) const { return args == b.args; }
  };

private:
  std::variant<int64_t, char, bool, std::string, TupleType, FuncObject,
               ConstructorT>
      value;

  template <typename Type> Type &get() {
    try {
      return std::get<Type>(value);
    } catch (const std::bad_variant_access &e) {
      throw std::runtime_error(
          std::format("Invalid value, expected {}, got index {}, error \"{}\"",
                      typeid(Type).name(), value.index(), e.what()));
    }
  }

public:
  Value() = default;
  Value(const Value &) = default;
  template <typename T> explicit Value(T v) : value(std::move(v)){};
  static auto Int(int64_t v) { return new Value(v); }
  static auto Bool(bool v) { return new Value(v); }
  static auto Char(char v) { return new Value(v); }
  static auto String(std::string str) { return new Value(std::move(str)); }
  static Value *Tuple(TupleType elems) { return new Value(std::move(elems)); }
  static auto Unit() { return new Value(); }
  static auto Constructor(int64_t name, Value *param) {
    return new Value(ConstructorT(ConstructorT::ConsType(name), param));
  }

  static auto Function(void *func, size_t args_count) {
    return new Value(FuncObject(func, args_count));
  }

  [[nodiscard]] auto &get_func() { return get<FuncObject>(); }

  [[nodiscard]] auto get_int() { return get<int64_t>(); }

  [[nodiscard]] auto get_bool() { return get<bool>(); }

  [[nodiscard]] auto get_char() { return get<char>(); }
  [[nodiscard]] auto &get_string() { return get<std::string>(); }

  [[nodiscard]] TupleType &get_tuple() { return get<TupleType>(); }
  [[nodiscard]] Value *disassemble() {
    return get<ConstructorT>().disassemble();
  }
  [[nodiscard]] Value *same_cons(int64_t name) {
    return get<ConstructorT>().same_cons(ConstructorT::ConsType(name));
  }

  bool operator==(const Value &b) const { return value == b.value; }
};

extern "C" Value *copy(Value *v) { return new Value(*v); }

#define PRINT(type)                                                            \
  extern "C" void print_##type(Value *v) {                                     \
    auto res = std::format("{}", v->get_##type());                             \
    std::cout << res << std::endl;                                             \
  }

using string = std::string;

PRINT(bool)
PRINT(int)
PRINT(char)
PRINT(string)

Value *apply_cpp(Value *v, std::vector<Value *> &args) {
  auto copy = new Value(v->get_func());

  auto &func = copy->get_func();
  std::vector<Value *> next;
  for (auto &arg : args) {
    if (func.can_call()) {
      next.emplace_back(arg);
    } else {
      func.args.emplace_back(arg);
    }
  }
  if (func.can_call()) {
    ffi_cif cif;
    auto arg_types = std::vector(func.args_count, &ffi_type_pointer);
    auto ret_type = &ffi_type_pointer;

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, func.args_count, ret_type,
                     arg_types.data()) != FFI_OK) {
      throw std::runtime_error("ffi_prep_cif failed");
    }

    auto args = std::vector<void *>(func.args_count);
    std::ranges::transform(func.args, args.begin(),
                           [](auto &x) -> void * { return &x; });

    Value *result;
    ffi_call(&cif, FFI_FN(func.func), &result, args.data());
    if (!next.empty()) {
      result = apply_cpp(result, next);
    }
    return result;
  }
  return copy;
}

extern "C" Value *apply(Value *v, int count, ...) {
  std::vector<Value *> args;
  va_list varargs;
  va_start(varargs, count);
  for (auto _ : std::views::iota(0, count)) {
    auto c = va_arg(varargs, Value *);
    args.emplace_back(c);
  }
  va_end(varargs);

  return apply_cpp(v, args);
}

extern "C" bool get_bool(Value *v) { return v->get_bool(); }
extern "C" Value *create_unit() { return Value::Unit(); }
extern "C" Value *create_int(const int64_t v) { return Value::Int(v); }
extern "C" Value *create_bool(const bool v) { return Value::Bool(v); }
extern "C" Value *create_char(const char v) { return Value::Char(v); }
extern "C" Value *create_string(int count, ...) {
  std::string string;
  string.reserve(count);
  va_list args;
  va_start(args, count);
  for (auto _ : std::views::iota(0, count)) {
    int c = va_arg(args, int);
    string.push_back(static_cast<char>(c));
  }
  va_end(args);
  return Value::String(std::move(string));
}
extern "C" Value *create_tuple(int count, ...) {
  Value::TupleType vector;
  vector.reserve(count);
  va_list args;
  va_start(args, count);
  for (auto _ : std::views::iota(0, count)) {
    auto c = va_arg(args, Value *);
    vector.emplace_back(copy(c));
  }
  va_end(args);
  return Value::Tuple(std::move(vector));
}
extern "C" Value *get_from_tuple(Value *tuple, Value *index) {
  return Value::ValuePtr(tuple->get_tuple()[index->get_int()]).release();
}
extern "C" Value *create_function(void *func, size_t args_count) {
  return Value::Function(func, args_count);
}
extern "C" Value *create_constructor(int64_t name, Value *arg) {
  return Value::Constructor(name, arg);
}
extern "C" Value *disassemble(Value *name) { return name->disassemble(); }
extern "C" Value *same_cons(Value *cons, int64_t name) {
  return cons->same_cons(name);
}

#define BIN_OP_INT(name, op)                                                   \
  extern "C" Value *name(Value *a, Value *b) {                                 \
    return Value::Int(a->get_int() op b->get_int());                           \
  }

BIN_OP_INT(op_plus, +);
BIN_OP_INT(op_minus, -);
BIN_OP_INT(op_mult, *);
BIN_OP_INT(op_div, /);

extern "C" Value *op_neg(Value *a) { return Value::Int(-a->get_int()); }
extern "C" Value *op_pos(Value *a) { return Value::Int(a->get_int()); }

#define BIN_OP_BOOL(name, op)                                                  \
  extern "C" Value *name(Value *a, Value *b) {                                 \
    return Value::Bool(a->get_bool() op b->get_bool());                        \
  }

BIN_OP_BOOL(op_or, ||);
BIN_OP_BOOL(op_and, &&);

extern "C" Value *op_not(Value *a) { return Value::Bool(!a->get_bool()); }

#define BIN_OP_INT_BOOL(name, op)                                              \
  extern "C" Value *name(Value *a, Value *b) {                                 \
    return Value::Bool(a->get_int() op b->get_int());                          \
  }

BIN_OP_INT_BOOL(op_gt, >)
BIN_OP_INT_BOOL(op_ge, >=)
BIN_OP_INT_BOOL(op_lt, <)
BIN_OP_INT_BOOL(op_le, <=)

extern "C" Value *op_eq(Value *a, Value *b) {
  return Value::Bool(a->operator==(*b));
}
extern "C" Value *op_neq(Value *a, Value *b) { return op_not(op_eq(a, b)); }
extern "C" Value *op_phys_eq(Value *a, Value *b) { return Value::Bool(a == b); }

extern "C" void exception(Value *s) {
  throw std::runtime_error(s->get_string());
}

extern "C" void free_value(const Value *v) { delete v; };
