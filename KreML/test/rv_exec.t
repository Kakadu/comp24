  $ dune exec rv_codegen <<- EOF > test.S
  > let main =
  >   let () = print_int 0 in
  >   0
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  0

  $ dune exec rv_codegen < manytests/typed/001fac.ml > test.S
  $ riscv64-linux-gnu-gcc -static -o test.out test.S -L../runtime/ -l:rv64_runtime.a
  $ qemu-riscv64-static test.out
  24
