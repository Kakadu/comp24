  $ LD_LIBRARY_PATH=$LD_LIBRARY_PATH:../../runtime/ ./fib_cps.elf
  0
  1
  1
  2
  3
  5
  8
  13
  21
  34

  $ ln -s ../../runtime/riscv/libffi.so ../../runtime/riscv/libffi.so.8
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -E LD_LIBRARY_PATH=../../runtime/riscv ./fib_cps_riscv.elf
  0
  1
  1
  2
  3
  5
  8
  13
  21
  34
