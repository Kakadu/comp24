$ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
> let main = 
>   
> EOF
$ cat /tmp/tuples.s
$ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
$ /tmp/tuples

  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let main = 
  >   let tuple = (42, true, fun x -> x) in
  >   print_tuple tuple
  > EOF
  ANF:
  let ll_2 x = x
  let main = let tuple = (42, true, ll_2) in
    print_tuple tuple
  
  $ cat /tmp/tuples.s
  .section .data
  
  .section .text
  
      .globl ll_2
      .type ll_2, @function
  ll_2:
      # args: x
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      sd a0,0(s0)  # x
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      call runtime_init
      li a0,3
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
      # Creating closure for ll_2
      la a0,ll_2
      li a1,1
      call create_closure
      mv a2,a0  # ll_2
      ld a0,0(s0)
      li a1,2
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
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  (42, 1, 2360320)


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
         let x2 = `get_tuple_field arg_1 0 in
         let y2 = `get_tuple_field arg_1 1 in
         let z2 = `get_tuple_field arg_1 2 in
         let x1 = `get_tuple_field arg_0 0 in
         let y1 = `get_tuple_field arg_0 1 in
         let z1 = `get_tuple_field arg_0 2 in
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
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
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
         let a = `get_tuple_field __tuple_0 0 in
         let b = `get_tuple_field __tuple_0 1 in
         let c = `get_tuple_field __tuple_0 2 in
         let anf0 = println_int a in
         let anf1 = println_int b in
         let anf2 = println_bool c in
         0
  
$ cat /tmp/tuples.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
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
         let anf3 = `get_tuple_field x 1 in
         let anf1 = ( = ) anf3 0 in
         if anf1 
         then let a = `get_tuple_field x 0 in
           0 
         else
           let a_0 = `get_tuple_field x 0 in
           let b = `get_tuple_field x 1 in
           ( / ) a_0 b
  let main =
    let anf7 = div (10, 2) in
    let anf4 = println_int anf7 in
    let anf6 = div (10, 0) in
    let anf5 = println_int anf6 in
    0
  
$ cat /tmp/tuples.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
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
  let a = `get_tuple_field __tuple_0 0
  let b = `get_tuple_field __tuple_0 1
  let c = `get_tuple_field __tuple_0 2
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
  let init_a _ = `get_tuple_field __tuple_0 0
  
  init_b ANF:
  let init_b _ = `get_tuple_field __tuple_0 1
  
  init_c ANF:
  let init_c _ = `get_tuple_field __tuple_0 2
  
$ cat /tmp/tuples.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  1
  2
  3

