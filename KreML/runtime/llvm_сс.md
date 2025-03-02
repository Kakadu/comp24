# Calling convention
Functions take up to 8 arguments.
- If function does not have an environment and its arity is less or equal than 8, arguments are passed directly
- If function does not have an environment and its arity is greater than 8, first seven args are passed on positions 2..8, the rest args are passed as a heap pointer on position 1
- If function has an environment, it is passed as a first argument
    - if function arity is less than 8, all arguments are passed directly from the second position
    - If function arity is greater or equal than 8, first seven args are passed on positions 2..8, the rest args are passed as a part of an environment, args come before free variables