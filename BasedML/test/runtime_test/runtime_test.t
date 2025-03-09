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
  Warning: GC not implemented function mlrt_print_gc_info() don't have effect
  Warning: GC not implemented function mlrt_compact() don't have effect
  Warning: GC not implemented function mlrt_print_gc_info() don't have effect

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
  Memory used:   0x17c0/  0x4000
  Memory used:     0x80/  0x4000
