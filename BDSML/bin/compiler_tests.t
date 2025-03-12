  $ clang-16 -std=c++20 -c runtime/runtime.cpp -o runtime.o
  $ for f in manytests/typed/*.ml; do
  > ./compiler.exe < $f
  > clang-16 -lstdc++ -std=c++20 -lffi out.ll runtime.o -o a.out
  > echo "\n----------------------------------- $f"
  > cat $f
  > echo "\n"
  > ./a.out
  > rm a.out
  > done
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-cf6ae5.o: in function `main':
  BDSML:(.text+0x25a): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/001fac.ml
  let rec fac n = if n<=1 then 1 else n * fac (n-1)
  
  let main =
    let () = print_int (fac 4) in
    0
  
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Undefined function: __var_k0")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs in file "lib/llvm/codegen.ml", line 135, characters 31-45
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-8fb482.o: in function `main':
  BDSML:(.text+0x25a): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/002fac.ml
  let rec fac_cps n k =
    if n=1 then k 1 else
    fac_cps (n-1) (fun p -> k (p*n))
  
  let main =
    let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
    0
  
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-a0673c.o: in function `main':
  BDSML:(.text+0x546): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x63c): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/003fib.ml
  let rec fib_acc a b n =
    if n=1 then b
    else
      let n1 = n-1 in
      let ab = a+b in
      fib_acc b ab n1
  
  let rec fib n =
    if n<2
    then n
    else fib (n - 1) + fib (n - 2) 
  
  let main =
    let () = print_int (fib_acc 0 1 4) in
    let () = print_int (fib 4) in
    0
  
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Couldn't find value for key__var_test10")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_aexpr in file "lib/llvm/codegen.ml", line 51, characters 16-44
  Called from Stdlib__List.map in file "list.ml", line 86, characters 15-19
  Called from My_llvm__Codegen.compile_cexpr in file "lib/llvm/codegen.ml", line 92, characters 32-60
  Called from My_llvm__Codegen.compile_lexpr in file "lib/llvm/codegen.ml", line 104, characters 16-31
  Called from My_llvm__Codegen.compile_values in file "lib/llvm/codegen.ml", line 145, characters 23-38
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_main in file "lib/llvm/codegen.ml", line 162, characters 11-33
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 169, characters 11-31
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-160406.o: in function `main':
  BDSML:(.text+0x546): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x63c): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/004manyargs.ml
  let wrap f = if 1 = 1 then f else f
  
  let test3 a b c =
    let a = print_int a in
    let b = print_int b in
    let c = print_int c in
    0
  
  let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  
  let main =
    let rez =
        (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
           1000000000)
    in
    let () = print_int rez in
    let temp2 = wrap test3 1 10 100 in
    0
  
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Undefined function: __var_f")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs.(fun) in file "lib/llvm/codegen.ml", line 136, characters 57-71
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-d56ca9.o: in function `main':
  BDSML:(.text+0x546): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x63c): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/005fix.ml
  let rec fix f x = f (fix f) x
  
  let fac self n = if n<=1 then 1 else n * self (n-1)
  
  let main =
    let () = print_int (fix fac 6) in
    0
  
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Couldn't find value for keylifted_0")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_aexpr in file "lib/llvm/codegen.ml", line 51, characters 16-44
  Called from My_llvm__Codegen.compile_cexpr in file "lib/llvm/codegen.ml", line 77, characters 19-35
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs in file "lib/llvm/codegen.ml", line 135, characters 31-45
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-e9eeba.o: in function `main':
  BDSML:(.text+0x546): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x63c): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/006partial.ml
  let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)
  
  let foo x = foo true (foo false (foo true (foo false x)))
  let main =
    let () = print_int (foo 11) in
    0
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Undefined function: __var_foo2")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_lexpr in file "lib/llvm/codegen.ml", line 104, characters 16-31
  Called from My_llvm__Codegen.compile_values in file "lib/llvm/codegen.ml", line 145, characters 23-38
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_main in file "lib/llvm/codegen.ml", line 162, characters 11-33
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 169, characters 11-31
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-a87285.o: in function `main':
  BDSML:(.text+0x546): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x63c): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/006partial2.ml
  let foo a b c =
    let () = print_int a in
    let () = print_int b in
    let () = print_int c in
    a + b * c
  
  let main =
    let foo = foo 1 in
    let foo = foo 2 in
    let foo = foo 3 in
    let () = print_int foo in
    0
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Couldn't find value for keylifted_1")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_aexpr in file "lib/llvm/codegen.ml", line 51, characters 16-44
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs in file "lib/llvm/codegen.ml", line 135, characters 31-45
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-6be5e5.o: in function `main':
  BDSML:(.text+0x546): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x63c): undefined reference to `create_unit'
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/006partial3.ml
  let foo a =
    let () = print_int a in fun b ->
    let () = print_int b in fun c ->
    print_int c
  
  let main =
    let () = foo 4 8 9 in
    0
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-0a6a3a.o: in function `__var__start':
  BDSML:(.text+0x74): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0xc4): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x124): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x19d): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x2fc): undefined reference to `create_unit'
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-0a6a3a.o:BDSML:(.text+0x3bf): more undefined references to `create_unit' follow
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/007order.ml
  let _start () () a () b _c () d __ =
    let () = print_int (a+b) in
    let () = print_int __ in
    a*b / _c + d
  
  
  let main =
    print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Undefined function: __var_g")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_lexpr in file "lib/llvm/codegen.ml", line 104, characters 16-31
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs in file "lib/llvm/codegen.ml", line 135, characters 31-45
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-cb8ff9.o: in function `__var__start':
  BDSML:(.text+0x74): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0xc4): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x124): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x19d): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x2fc): undefined reference to `create_unit'
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-cb8ff9.o:BDSML:(.text+0x3bf): more undefined references to `create_unit' follow
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/008ascription.ml
  let addi = fun f g x -> (f x (g x: bool) : int)
  
  let main =
    let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
    0
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Couldn't find value for keylifted_0")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_aexpr in file "lib/llvm/codegen.ml", line 51, characters 16-44
  Called from My_llvm__Codegen.compile_lexpr in file "lib/llvm/codegen.ml", line 104, characters 16-31
  Called from My_llvm__Codegen.compile_values in file "lib/llvm/codegen.ml", line 145, characters 23-38
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_main in file "lib/llvm/codegen.ml", line 162, characters 11-33
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 169, characters 11-31
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-251886.o: in function `__var__start':
  BDSML:(.text+0x74): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0xc4): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x124): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x19d): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x2fc): undefined reference to `create_unit'
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-251886.o:BDSML:(.text+0x3bf): more undefined references to `create_unit' follow
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/009let_poly.ml
  let temp =
    let f = fun x -> x in
    (f 1, f true)
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Undefined function: __var_f")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs.(fun) in file "lib/llvm/codegen.ml", line 136, characters 57-71
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-92f63b.o: in function `__var__start':
  BDSML:(.text+0x74): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0xc4): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x124): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x19d): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x2fc): undefined reference to `create_unit'
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-92f63b.o:BDSML:(.text+0x3bf): more undefined references to `create_unit' follow
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/015tuples.ml
  let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l
  let feven p n =
    let (e, o) = p in
    if n == 0 then 1 else o (n - 1)
  let fodd p n =
    let (e, o) = p in
    if n == 0 then 0 else e (n - 1)
  let tie = fixpoly (feven, fodd)
  
  let rec meven n = if n = 0 then 1 else modd (n - 1)
  and modd n = if n = 0 then 1 else meven (n - 1)
  let main =
    let () = print_int (modd 1) in
    let () = print_int (meven 2) in
    let (even,odd) = tie in
    let () = print_int (odd 3) in
    let () = print_int (even 4) in
    0
  
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  Fatal error: exception Failure("Undefined function: __var_f")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from My_llvm__Codegen.compile_lexpr in file "lib/llvm/codegen.ml", line 104, characters 16-31
  Called from My_llvm__Codegen.compile_cexpr in file "lib/llvm/codegen.ml", line 77, characters 19-35
  Called from My_llvm__Codegen.compile_cexpr in file "lib/llvm/codegen.ml", line 81, characters 19-35
  Called from My_llvm__Codegen.compile_func in file "lib/llvm/codegen.ml", line 129, characters 19-37
  Called from My_llvm__Codegen.compile_funcs.(fun) in file "lib/llvm/codegen.ml", line 136, characters 57-71
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from My_llvm__Codegen.compile_program in file "lib/llvm/codegen.ml", line 168, characters 11-37
  Called from Dune__exe__Compiler in file "bin/compiler.ml", line 28, characters 30-57
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-75fe40.o: in function `__var__start':
  BDSML:(.text+0x74): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0xc4): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x124): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x19d): undefined reference to `create_unit'
  /usr/bin/ld: BDSML:(.text+0x2fc): undefined reference to `create_unit'
  /usr/bin/ld: /tmp/build_9a60d5_dune/out-75fe40.o:BDSML:(.text+0x3bf): more undefined references to `create_unit' follow
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  
  ----------------------------------- manytests/typed/016lists.ml
  let rec length xs =
    match xs with
    | [] -> 0
    | h::tl -> 1 + length tl
  
  let length_tail =
    let rec helper acc xs =
    match xs with
    | [] -> acc
    | h::tl -> helper (acc + 1) tl
    in
    helper 0
  
  let rec map f xs =
    match xs with
    | [] -> []
    | a::[] -> [f a]
    | a::b::[] -> [f a; f b]
    | a::b::c::[] -> [f a; f b; f c]
    | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl
  
  let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)
  
  let concat =
    let rec helper xs =
      match xs with
      | [] -> []
      | h::tl -> append h (helper tl)
    in helper
  
  let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl
  
  let rec cartesian xs ys =
    match xs with
    | [] -> []
    | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)
  
  let main =
    let () = iter print_int [1;2;3] in
    let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
    0
  
  
  ./a.out: not found
  rm: cannot remove 'a.out': No such file or directory
  [1]
