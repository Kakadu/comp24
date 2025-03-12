  $ clang-16 -std=c++17 -c runtime.cpp -o runtime.o
  $ for prog in ../manytests/typed/*; do
  > echo "\n--------------- $prog\n"
  > dune exec ./compiler.exe < $prog > out.ll
  > clang-16 -Wno-override-module -lstdc++ -std=c++17 -lffi -v out.ll runtime.o -o a.out
  > cat $prog
  > echo "\n\n"
  > echo "Output:\n"
  > ./a.out
  > rm out.ll
  > rm a.out
  > done
  
  --------------- ../manytests/typed/000.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-a457b4.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-a457b4.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
  let f x = x + 1
  
  let g f = f - 1
  
  let () = print_int (f 52)
  
  
  Output:
  
  53
  
  --------------- ../manytests/typed/001fac.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-75ec6e.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-75ec6e.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
  let rec fac n = if n<=1 then 1 else n * fac (n-1)
  
  let main =
    let () = print_int (fac 4) in
    0
  
  
  
  
  Output:
  
  24
  
  --------------- ../manytests/typed/002fac.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-b776fd.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-b776fd.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
  let rec fac_cps n k =
    if n=1 then k 1 else
    fac_cps (n-1) (fun p -> k (p*n))
  
  let main =
    let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
    0
  
  
  
  
  Output:
  
  24
  
  --------------- ../manytests/typed/003fib.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-b58f97.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-b58f97.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
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
  
  
  
  
  Output:
  
  3
  3
  
  --------------- ../manytests/typed/004manyargs.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-411616.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-411616.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
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
  
  
  
  
  Output:
  
  1111111111
  1
  10
  100
  
  --------------- ../manytests/typed/005fix.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-618458.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-618458.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
  let rec fix f x = f (fix f) x
  
  let fac self n = if n<=1 then 1 else n * self (n-1)
  
  let main =
    let () = print_int (fix fac 6) in
    0
  
  
  
  
  Output:
  
  720
  
  --------------- ../manytests/typed/006partial.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-325479.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-325479.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
  let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)
  
  let foo x = foo true (foo false (foo true (foo false x)))
  let main =
    let () = print_int (foo 11) in
    0
  
  
  Output:
  
  1122
  
  --------------- ../manytests/typed/006partial2.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-50f851.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-50f851.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
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
  
  
  Output:
  
  1
  2
  3
  7
  
  --------------- ../manytests/typed/006partial3.ml
  
  Ubuntu clang version 16.0.6 (23ubuntu4)
  Target: x86_64-pc-linux-gnu
  Thread model: posix
  InstalledDir: /usr/bin
  Found candidate GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Selected GCC installation: /usr/bin/../lib/gcc/x86_64-linux-gnu/13
  Candidate multilib: .;@m64
  Selected multilib: .;@m64
   "/usr/lib/llvm-16/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name out.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -mllvm -treat-scalable-fixed-error-as-warning -debugger-tuning=gdb -v -fcoverage-compilation-dir=$TESTCASE_ROOT -resource-dir /usr/lib/llvm-16/lib/clang/16 -Wno-override-module -std=c++17 -fdebug-compilation-dir=$TESTCASE_ROOT -ferror-limit 19 -fgnuc-version=4.2.1 -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/build_3c3229_dune/out-cd19a8.o -x ir out.ll
  clang -cc1 version 16.0.6 based upon LLVM 16.0.6 default target x86_64-pc-linux-gnu
   "/usr/bin/ld" -pie -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /lib/x86_64-linux-gnu/Scrt1.o /lib/x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtbeginS.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/lib -L/usr/lib -lstdc++ -lffi /tmp/build_3c3229_dune/out-cd19a8.o runtime.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/13/crtendS.o /lib/x86_64-linux-gnu/crtn.o
  let foo a =
    let () = print_int a in fun b ->
    let () = print_int b in fun c ->
    print_int c
  
  let main =
    let () = foo 4 8 9 in
    0
  
  
  Output:
  
  4
  8
  9

