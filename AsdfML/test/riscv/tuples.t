  $ if [ -z "$latest" ]; then
  >   alias riscv64-linux-gnu-gcc='riscv64-unknown-linux-gnu-gcc'
  >   alias qemu-riscv64-static='qemu-riscv64'
  > fi

$ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
> let main = 
>   
> EOF
$ cat /tmp/tuples.s
$ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
$ qemu-riscv64-static /tmp/tuples

  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let main = 
  >   let tuple = (42, true) in
  >   print_tuple tuple
  > EOF
  ANF:
  let main = let tuple = (42, true) in
         print_tuple tuple
  
  $ cat /tmp/tuples.s
  .section .data
  
  .section .text
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      call runtime_init
      li a0,2
      call ml_create_tuple
      sd a0,0(s0)  # tuple
      li a2,42
      ld a0,0(s0)
      li a1,0
      call ml_set_tuple_field
      li a2,1
      ld a0,0(s0)
      li a1,1
      call ml_set_tuple_field
      ld a0,0(s0)
      sd a0,-8(s0)  # tuple
      # Creating closure for ml_print_tuple
      la a0,ml_print_tuple
      li a1,1
      call create_closure
      ld a1,-8(s0)  # tuple
      call apply_closure_1
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  $ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/tuples
  (42, 1)


  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let cross (x1, y1, z1) (x2, y2, z2) =
  >   let x = (y1 * z2) - (z1 * y2) in
  >   let y = (z1 * x2) - (x1 * z2) in
  >   let z = (x1 * y2) - (y1 * x2) in
  >   (x, y, z)
  > 
  > let main = 
  >   let a = (1, 3, 5) in
  >   let b = (7, 11, 13) in
  >   let c = cross a b in
  >   print_tuple c
  > EOF
  ANF:
  let cross arg_0 arg_1 =
         let x2 = ml_get_tuple_field arg_1 0 in
         let y2 = ml_get_tuple_field arg_1 1 in
         let z2 = ml_get_tuple_field arg_1 2 in
         let x1 = ml_get_tuple_field arg_0 0 in
         let y1 = ml_get_tuple_field arg_0 1 in
         let z1 = ml_get_tuple_field arg_0 2 in
         let anf4 = ( * ) y1 z2 in
         let anf5 = ( * ) z1 y2 in
         let x = ( - ) anf4 anf5 in
         let anf2 = ( * ) z1 x2 in
         let anf3 = ( * ) x1 z2 in
         let y = ( - ) anf2 anf3 in
         let anf0 = ( * ) x1 y2 in
         let anf1 = ( * ) y1 x2 in
         let z = ( - ) anf0 anf1 in
         (x, y, z)
  let main =
    let a = (1, 3, 5) in
    let b = (7, 11, 13) in
    let c = cross a b in
    print_tuple c
  
$ cat /tmp/tuples.s
  $ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/tuples
  (-16, 22, -10)


  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let main = 
  >   let (a, b, c) = (1, 2, true) in
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = println_bool c in
  >   0
  > EOF
  ANF:
  let main =
         let __tuple_0 = (1, 2, true) in
         let a = ml_get_tuple_field __tuple_0 0 in
         let b = ml_get_tuple_field __tuple_0 1 in
         let c = ml_get_tuple_field __tuple_0 2 in
         let anf0 = println_int a in
         let anf1 = println_int b in
         let anf2 = println_bool c in
         0
  
$ cat /tmp/tuples.s
  $ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/tuples
  1
  2
  true


  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let div x = match x with
  >   | (a, 0) -> 0
  >   | (a, b) -> a / b
  > 
  > let main = 
  >   let _ = println_int (div (10, 2)) in
  >   let _ = println_int (div (10, 0)) in
  >   0
  > EOF
  ANF:
  let div x =
         let anf3 = ml_get_tuple_field x 1 in
         let anf1 = ( = ) anf3 0 in
         if anf1 
         then let a = ml_get_tuple_field x 0 in
           0 
         else
           let a_0 = ml_get_tuple_field x 0 in
           let b = ml_get_tuple_field x 1 in
           ( / ) a_0 b
  let main =
    let anf7 = div (10, 2) in
    let anf4 = println_int anf7 in
    let anf6 = div (10, 0) in
    let anf5 = println_int anf6 in
    0
  
$ cat /tmp/tuples.s
  $ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/tuples
  5
  0


  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let (a, b, c) = (1, 2, 3)
  > let main = 
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = println_int c in
  >   0
  > EOF
  ANF:
  let __temp_match_0 = (1, 2, 3)
  let __tuple_0 = __temp_match_0
  let a = ml_get_tuple_field __tuple_0 0
  let b = ml_get_tuple_field __tuple_0 1
  let c = ml_get_tuple_field __tuple_0 2
  let main =
    let anf3 = println_int a in
    let anf4 = println_int b in
    let anf5 = println_int c in
    0
  
  init___temp_match_0 ANF:
  let init___temp_match_0 _ = (1, 2, 3)
  
  init___tuple_0 ANF:
  let init___tuple_0 _ = __temp_match_0
  
  init_a ANF:
  let init_a _ = ml_get_tuple_field __tuple_0 0
  
  init_b ANF:
  let init_b _ = ml_get_tuple_field __tuple_0 1
  
  init_c ANF:
  let init_c _ = ml_get_tuple_field __tuple_0 2
  
$ cat /tmp/tuples.s
  $ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/tuples
  1
  2
  3

  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let tuple = (1, (2, 3))
  > let (a, (b, c)) = tuple
  > let main = 
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = println_int c in
  >   0
  > EOF
  ANF:
  let tuple = (1, (2, 3))
  let __temp_match_0 = tuple
  let __tuple_0 = __temp_match_0
  let a = ml_get_tuple_field __tuple_0 0
  let __tuple_1 = ml_get_tuple_field __tuple_0 1
  let b = ml_get_tuple_field __tuple_1 0
  let c = ml_get_tuple_field __tuple_1 1
  let main =
    let anf4 = println_int a in
    let anf5 = println_int b in
    let anf6 = println_int c in
    0
  
  init_tuple ANF:
  let init_tuple _ = (1, (2, 3))
  
  init___temp_match_0 ANF:
  let init___temp_match_0 _ = tuple
  
  init___tuple_0 ANF:
  let init___tuple_0 _ = __temp_match_0
  
  init_a ANF:
  let init_a _ = ml_get_tuple_field __tuple_0 0
  
  init___tuple_1 ANF:
  let init___tuple_1 _ = ml_get_tuple_field __tuple_0 1
  
  init_b ANF:
  let init_b _ = ml_get_tuple_field __tuple_1 0
  
  init_c ANF:
  let init_c _ = ml_get_tuple_field __tuple_1 1
  
$ cat /tmp/tuples.s
  $ riscv64-linux-gnu-gcc -static /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
  $ qemu-riscv64-static /tmp/tuples
  1
  2
  3
