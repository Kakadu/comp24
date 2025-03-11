#include <cinttypes>
#include <iostream>
#include <memory>
#include <string>
#include <variant>

template <class... Ts>
struct overloaded : Ts...
{
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

class Value
{
    class MyString : public std::unique_ptr<std::string>
    {
    public:
        MyString()
            : std::unique_ptr<std::string>(std::make_unique<std::string>()) {};

        bool operator==(const MyString &b) const { return operator*() == *b; }
    };

    std::variant<int64_t, char, bool, MyString> value;

    explicit Value(int64_t v) : value(v) {};

    explicit Value(bool v) : value(v) {};

    explicit Value(char v) : value(v) {};

    explicit Value(MyString v) : value(std::move(v)) {};

public:
    static auto Int(const int64_t v) { return new Value(v); }
    static auto Bool(const bool v) { return new Value(v); }
    static auto Char(const char v) { return new Value(v); }
    static auto String() { return new Value(MyString()); }

    void add_char(char n) const { std::get<MyString>(value)->push_back(n); }

    [[nodiscard]] auto get_int() const { return std::get<int64_t>(value); }

    [[nodiscard]] auto get_bool() const { return std::get<bool>(value); }

    [[nodiscard]] auto get_char() const { return std::get<char>(value); }

    [[nodiscard]] auto get_string() const { return *std::get<MyString>(value); }

    bool operator==(const Value &b) const { return value == b.value; }
};

auto create_int(auto v) { return Value::Int(v); }
auto create_bool(auto v) { return Value::Bool(v); }
auto create_char(auto v) { return Value::Char(v); }
auto create_string() { return Value::String(); }
void add_char(Value *val, char n) { val->add_char(n); }

#define BIN_OP_INT(name, op)                             \
    Value *name(Value *a, Value *b)                      \
    {                                                    \
        return Value::Int(a->get_int() op b->get_int()); \
    }

BIN_OP_INT(op_plus, +);
BIN_OP_INT(op_minus, -);
BIN_OP_INT(op_mult, *);
BIN_OP_INT(op_div, /);

Value *op_neg(const Value *a) { return Value::Int(-a->get_int()); }
Value *op_pos(const Value *a) { return Value::Int(a->get_int()); }

#define BIN_OP_BOOL(name, op)                               \
    Value *name(Value *a, Value *b)                         \
    {                                                       \
        return Value::Bool(a->get_bool() op b->get_bool()); \
    }

BIN_OP_BOOL(op_or, ||);
BIN_OP_BOOL(op_and, &&);

Value *op_not(const Value *a) { return Value::Bool(!a->get_bool()); }

#define BIN_OP_INT_BOOL(name, op)                         \
    Value *name(Value *a, Value *b)                       \
    {                                                     \
        return Value::Bool(a->get_int() op b->get_int()); \
    }

BIN_OP_INT_BOOL(op_gt, >)
BIN_OP_INT_BOOL(op_ge, >=)
BIN_OP_INT_BOOL(op_lt, <)
BIN_OP_INT_BOOL(op_le, <=)

Value *op_eq(Value *a, Value *b) { return Value::Bool(a->operator==(*b)); }
Value *op_neq(Value *a, Value *b) { return op_not(op_eq(a, b)); }
Value *op_phys_eq(Value *a, Value *b) { return Value::Bool(a == b); }

#define PRINT(type)                               \
    void print_##type(const Value &v)             \
    {                                             \
        std::cout << v.get_##type() << std::endl; \
    }

PRINT(bool)
PRINT(int)
PRINT(char)

void exception(Value *s) { throw std::runtime_error(s->get_string()); }

void free_value(const Value *v) { delete v; };

int main()
{
    auto f = Value::String();
    add_char(f, 'b');
    auto b = Value::String();
    add_char(b, 'b');
    auto c = Value::Int(0);
    auto d = Value::Char('c');
    auto g = Value::Char('c');
    auto res = op_eq(f, b);
    std::cout << std::boolalpha << res->get_bool() << std::endl;
    free_value(res);
    free_value(f);
    free_value(b);
    free_value(c);
    free_value(d);
    free_value(g);
    return 0;
}
