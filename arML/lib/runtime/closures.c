#include "taggedValue.h"
#include <stdarg.h>
#include <ffi.h>

typedef struct CLOSURE
{
    void *fun_ptr;
    u_int32_t args_num;
    u_int32_t applied_args_num;
    VAL **applied_args;
} CLOSURE;

void validate_closure(VAL *closure_value);
void validate_argument_count(CLOSURE *closure);
void *allocate_safe(size_t size, const char *error_message);

VAL *ct_closure(void *function_pointer, int32_t total_args)
{
    CLOSURE *new_closure = allocate_memory(sizeof(CLOSURE));

    new_closure->fun_ptr = function_pointer;
    new_closure->args_num = total_args;
    new_closure->applied_args_num = 0;
    new_closure->applied_args = NULL;

    VAL *closure_value = allocate_memory(sizeof(VAL));
    closure_value->tag = TAG_CLOSURE;
    closure_value->data = new_closure;

    return closure_value;
}

VAL *invoke_closure(VAL *closure_value)
{
    validate_closure(closure_value);

    CLOSURE *closure = closure_value->data;
    validate_argument_count(closure);

    ffi_cif ffi_interface;
    ffi_type **arg_types = allocate_safe(closure->args_num * sizeof(ffi_type *), "Failed to allocate memory for argument types");
    void **arg_values = allocate_safe(closure->args_num * sizeof(void *), "Failed to allocate memory for argument values");

    for (uint32_t i = 0; i < closure->args_num; i++)
    {
        arg_types[i] = &ffi_type_pointer;
        arg_values[i] = &closure->applied_args[i];
    }

    if (ffi_prep_cif(&ffi_interface, FFI_DEFAULT_ABI, closure->args_num, &ffi_type_pointer, arg_types) != FFI_OK)
    {
        fprintf(stderr, "Error [invoke_closure]: Failed to prepare FFI call\n");
        free(arg_types);
        free(arg_values);
        exit(EXIT_FAILURE);
    }

    VAL *result = allocate_memory(sizeof(VAL));
    ffi_call(&ffi_interface, closure->fun_ptr, &result, arg_values);

    free(arg_types);
    free(arg_values);

    if (!result)
    {
        fprintf(stderr, "Error [invoke_closure]: Closure invocation returned NULL\n");
        exit(EXIT_FAILURE);
    }

    return result;
}

VAL *execute_closure(VAL *closure_obj, int32_t num_args, VAL **arg_list)
{
    validate_closure(closure_obj);

    VAL *new_closure_obj = allocate_tagged_val(TAG_CLOSURE);
    CLOSURE *new_closure = allocate_memory(sizeof(CLOSURE));

    CLOSURE *existing_closure = closure_obj->data;
    if (existing_closure == NULL)
    {
        fprintf(stderr, "Error: $execute_closure$ - existing_closure is NULL\n");
        exit(1);
    }

    new_closure->fun_ptr = existing_closure->fun_ptr;
    new_closure->args_num = existing_closure->args_num;

    if (existing_closure->applied_args_num + num_args > new_closure->args_num)
    {
        new_closure->applied_args_num = new_closure->args_num;
    }
    else
    {
        new_closure->applied_args_num = existing_closure->applied_args_num + num_args;
    }

    VAL **new_applied_args = allocate_memory(new_closure->applied_args_num * sizeof(VAL *));

    for (u_int32_t i = 0; i < existing_closure->applied_args_num; i++)
    {
        new_applied_args[i] = existing_closure->applied_args[i];
    }

    for (u_int32_t i = 0; i < new_closure->applied_args_num - existing_closure->applied_args_num; i++)
    {
        new_applied_args[existing_closure->applied_args_num + i] = arg_list[i];
    }

    new_closure->applied_args = new_applied_args;

    new_closure_obj->data = new_closure;

    if (new_closure->applied_args_num == new_closure->args_num)
    {
        VAL *result = invoke_closure(new_closure_obj);
        if (existing_closure->applied_args_num + num_args > new_closure->applied_args_num)
        {
            int32_t adjusted_num_args = existing_closure->applied_args_num + num_args - new_closure->applied_args_num;

            return execute_closure(result, adjusted_num_args, &arg_list[new_closure->applied_args_num - existing_closure->applied_args_num]);
        }

        return result;
    }

    return new_closure_obj;
}

VAL *invoke_closure_with_args(VAL *closure_obj, int32_t num_args, ...)
{
    va_list argptr;
    va_start(argptr, num_args);

    VAL **args = allocate_memory(num_args * sizeof(struct TAGGED_VAL *));
    for (int i = 0; i < num_args; i++)
    {
        VAL *arg = va_arg(argptr, VAL *);
        if (arg == NULL)
        {
            fprintf(stderr, "Error argument is NULL\n");
            exit(1);
        }
        args[i] = arg;
    }

    va_end(argptr);

    return execute_closure(closure_obj, num_args, args);
}

void validate_closure(VAL *closure_value)
{
    if (!closure_value)
    {
        fprintf(stderr, "Error [validate_closure]: closure_value is NULL\n");
        exit(EXIT_FAILURE);
    }
    if (closure_value->tag != TAG_CLOSURE)
    {
        fprintf(stderr, "Error [validate_closure]: Expected a closure, but got a different type\n");
        exit(EXIT_FAILURE);
    }
    if (!closure_value->data)
    {
        fprintf(stderr, "Error [validate_closure]: Closure data is NULL\n");
        exit(EXIT_FAILURE);
    }
}

void validate_argument_count(CLOSURE *closure)
{
    if (closure->applied_args_num != closure->args_num)
    {
        fprintf(stderr, "Error [validate_argument_count]: Incorrect number of arguments (expected %d, got %d)\n",
                closure->args_num, closure->applied_args_num);
        exit(EXIT_FAILURE);
    }
}

void *allocate_safe(size_t size, const char *error_message)
{
    void *ptr = malloc(size);
    if (!ptr)
    {
        fprintf(stderr, "Error [allocate_safe]: %s\n", error_message);
        exit(EXIT_FAILURE);
    }
    return ptr;
}
