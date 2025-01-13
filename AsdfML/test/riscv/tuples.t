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
  
      .globl ll_2
      .type ll_2, @function
  ll_2:
      # args: x
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # x
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
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
      sd a0,0(s0)
      li a2,1
      ld a0,0(s0)
      li a1,1
      call ml_set_tuple_field
      sd a0,0(s0)
      # Creating closure for ll_2
      la a0,ll_2
      li a1,1
      call create_closure
      mv a2,a0  # ll_2
      ld a0,0(s0)
      li a1,2
      call ml_set_tuple_field
      sd a0,0(s0)
      sd a0,-8(s0)  # a0
      # Creating closure for ml_print_tuple
      la a0,ml_print_tuple
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a0
      call apply_closure
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  (42, 1, 2114560)

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
         let a0 = `arg_9 in
         let a40 = `get_tuple_field a0 in
         let a2 = a40 2 in
         let a39 = `get_tuple_field a0 in
         let a4 = a39 1 in
         let a38 = `get_tuple_field a0 in
         let a6 = a38 0 in
         let a7 = `arg_8 in
         let a37 = `get_tuple_field a7 in
         let a9 = a37 2 in
         let a36 = `get_tuple_field a7 in
         let a11 = a36 1 in
         let a35 = `get_tuple_field a7 in
         let a13 = a35 0 in
         let a34 = ( * ) a11 in
         let a33 = a34 a2 in
         let a30 = ( - ) a33 in
         let a32 = ( * ) a9 in
         let a31 = a32 a4 in
         let a15 = a30 a31 in
         let a29 = ( * ) a9 in
         let a28 = a29 a6 in
         let a25 = ( - ) a28 in
         let a27 = ( * ) a13 in
         let a26 = a27 a2 in
         let a17 = a25 a26 in
         let a24 = ( * ) a13 in
         let a23 = a24 a4 in
         let a20 = ( - ) a23 in
         let a22 = ( * ) a11 in
         let a21 = a22 a6 in
         let a19 = a20 a21 in
         (a15, a17, a19)
  let main =
    let a41 = (1, 2, 3) in
    let a42 = (4, 5, 6) in
    let a46 = cross a41 in
    let a44 = a46 a42 in
    print_tuple a44
  
$ cat /tmp/tuples.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/tuples.s -o /tmp/tuples -L../../runtime/ -l:libruntime.a
  $ /tmp/tuples
  (-3, 6, -3)
