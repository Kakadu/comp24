  $ if [ -z "$latest" ]; then
  >   alias riscv64-linux-gnu-gcc='riscv64-unknown-linux-gnu-gcc'
  >   alias qemu-riscv64-static='qemu-riscv64'
  > fi

  $ dune exec riscv -- -o /tmp/many.s < ./typed/001fac.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  24 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/002fac.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  24 

  $ dune exec riscv -- -o /tmp/many.s < ./typed/003fib.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  3 3 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/004manyargs.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  1111111111 1 10 100 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/005fix.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  720 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/006partial.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  1122 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/006partial2.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  1 2 3 7 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/006partial3.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  4 8 9 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/007order.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  1 2 4 -1 103 -555555 10000 


type annotations on expressions
;   $ dune exec riscv -- -o /tmp/many.s < ./typed/008ascription.ml
; $ cat /tmp/many.s
;   $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
;   $ qemu-riscv64-static /tmp/many


no main
$ dune exec riscv -- -o /tmp/many.s < ./typed/009let_poly.ml
  $ dune exec riscv -- -o /tmp/many.s  <<- EOF
  > let temp =
  >   let f = fun x -> x in
  >   (f 1, f true)
  > let main = 
  >   let () = print_tuple temp in
  >   ()
  > EOF
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  (1, 1)


ADT
;   $ dune exec riscv -- -o /tmp/many.s < ./typed/010sukharev.ml
; $ cat /tmp/many.s
;   $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
;   $ qemu-riscv64-static /tmp/many


  $ dune exec riscv -- -o /tmp/many.s < ./typed/011mapcps.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  2 3 4 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/012fibcps.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  8 


  $ dune exec riscv -- -o /tmp/many.s < ./typed/013foldfoldr.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  6 

mutual recursion
;  $ dune exec riscv -- -o /tmp/many.s < ./typed/015tuples.ml
;  Undefined variable 'modd'
;$ cat /tmp/many.s
;  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
;  $ qemu-riscv64-static /tmp/many


  $ dune exec riscv -- -o /tmp/many.s < ./typed/016lists.ml
$ cat /tmp/many.s
  $ riscv64-linux-gnu-gcc -static /tmp/many.s -o /tmp/many -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/many
  1 2 3 8 

