$ dune exec riscv -- -o /tmp/rt.s < rt.ml
$ riscv64-unknown-linux-gnu-gcc /tmp/rt.s -o /tmp/rt -L../../runtime/ -l:libruntime.a
$ /tmp/rt > /tmp/rt.ppm
$ magick /tmp/rt.ppm /tmp/rt.png
