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
  let `ll_2 x = x
  let main = let a0 = (42, true, `ll_2) in
    print_tuple a0
  
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
      sd a0,-8(s0)  # a0
      # Creating closure for ml_print_tuple
      la a0,ml_print_tuple
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a0
      call apply_closure_1
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  (42, 1, 2122752)

  $ dune exec riscv -- -anf -o /tmp/tuples.s <<- EOF
  > let cross (x1, y1, z1) (x2, y2, z2) =
  >   let x = (y1 * z2) - (z1 * y2) in
  >   let y = (z1 * x2) - (x1 * z2) in
  >   let z = (x1 * y2) - (y1 * x2) in
  >   (x, y, z)
  > 
  > let main = 
  >   let a = (1, 2, 3) in
  >   let b = (4, 5, 6) in
  >   let c = cross a b in
  >   print_tuple c
  > EOF
  ANF:
  let cross `arg_8 `arg_9 =
         let a2 = `get_tuple_field `arg_9 2 in
         let a4 = `get_tuple_field `arg_9 1 in
         let a6 = `get_tuple_field `arg_9 0 in
         let a9 = `get_tuple_field `arg_8 2 in
         let a11 = `get_tuple_field `arg_8 1 in
         let a13 = `get_tuple_field `arg_8 0 in
         let a24 = ( * ) a11 a2 in
         let a25 = ( * ) a9 a4 in
         let a15 = ( - ) a24 a25 in
         let a22 = ( * ) a9 a6 in
         let a23 = ( * ) a13 a2 in
         let a17 = ( - ) a22 a23 in
         let a20 = ( * ) a13 a4 in
         let a21 = ( * ) a11 a6 in
         let a19 = ( - ) a20 a21 in
         (a15, a17, a19)
  let main =
    let a26 = (1, 2, 3) in
    let a27 = (4, 5, 6) in
    let a29 = cross a26 a27 in
    print_tuple a29
  
$ cat /tmp/tuples.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  (-3, 6, -3)

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
         let a0 = (1, 2, true) in
         let a2 = `get_tuple_field a0 2 in
         let a4 = `get_tuple_field a0 1 in
         let a6 = `get_tuple_field a0 0 in
         let a7 = println_int a6 in
         let a8 = println_int a4 in
         let a9 = println_bool a2 in
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
         let a11 = ( && ) true true in
         let a13 = `get_tuple_field x 1 in
         let a12 = ( = ) a13 0 in
         let a1 = ( && ) a11 a12 in
         if a1 
         then let a4 = `get_tuple_field x 0 in
           0 
         else
           let a7 = `get_tuple_field x 1 in
           let a9 = `get_tuple_field x 0 in
           ( / ) a9 a7
  let main =
    let a17 = div (10, 2) in
    let a14 = println_int a17 in
    let a16 = div (10, 0) in
    let a15 = println_int a16 in
    0
  
$ cat /tmp/tuples.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  5
  0
