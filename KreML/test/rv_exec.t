  $ dune exec rv_codegen <<- EOF > test.S
  > let main =
  >   let () = print_int 0 in
  >   0
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  0

  $ dune exec rv_codegen <<- EOF > test.S
  > let main =
  >   let map f (x, y) = f x, f y in
  >   let a, b = map (fun x -> x + 1) (1, 2) in
  >   let () = print_int a in
  >   let () = print_int b in
  >   0
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  2
  3

  $ dune exec rv_codegen <<- EOF > test.S
  > let main =
  >   let a = 5 in
  >   let f x = a + x in
  >   let () = print_int (f 5) in
  >   0
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ riscv64-unknown-elf-objdump -d test.out > /home/rnpozharskiy/disassembly.s
  $ qemu-riscv64-static test.out
  10

  $ dune exec rv_codegen < manytests/typed/001fac.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  24

  $ dune exec rv_codegen < manytests/typed/002fac.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  24

  $ dune exec rv_codegen < manytests/typed/003fib.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  3
  3

  $ dune exec rv_codegen < manytests/typed/004manyargs.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  1111111111
  1
  10
  100

  $ dune exec rv_codegen < manytests/typed/005fix.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  720


  $ dune exec rv_codegen < manytests/typed/006partial.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  1122

  $ dune exec rv_codegen < manytests/typed/006partial2.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  1
  2
  3
  7

  $ dune exec rv_codegen < manytests/typed/006partial3.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  4
  8
  9

  $ dune exec rv_codegen < manytests/typed/007order.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  1
  2
  4
  -1
  103
  -555555
  10000

  $ dune exec rv_codegen < manytests/typed/008ascription.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  8

no main
$ dune exec rv_codegen < manytests/typed/009let_poly.ml > test.S
$ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
$ qemu-riscv64-static test.out

debug it lol)
  $ dune exec rv_codegen < manytests/typed/015tuples.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out

  $ dune exec rv_codegen < manytests/typed/016lists.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  1
  2
  3
  8
