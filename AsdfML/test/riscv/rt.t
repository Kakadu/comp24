$ dune exec riscv -- -o /tmp/out.s < rt.ml
$ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
$ /tmp/out > /tmp/rt.ppm
$ magick /tmp/rt.ppm /tmp/rt.png
