$ dune exec riscv -- -o /tmp/rt.s < rt.ml
$ riscv64-unknown-linux-gnu-gcc -static /tmp/rt.s -o /tmp/rt -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
$ /tmp/rt > /tmp/rt.ppm
