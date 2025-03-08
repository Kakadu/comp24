  $ if [ -z "$latest" ]; then
  >   alias riscv64-linux-gnu-gcc='riscv64-unknown-linux-gnu-gcc'
  > fi

$ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
> 
> EOF
$ cat /tmp/out.s
$ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
$ /tmp/out

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 42
  > EOF
  ANF:
  let main = 42
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      call runtime_init
      li a0,42
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  [42]

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = println_int 42
  > EOF
  ANF:
  let main = println_int 42
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      call runtime_init
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      li a1,42
      call apply_closure_1
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  42

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let a = 1 in
  >   let b = 2 in
  >   let c = a + b in
  >   println_int c
  > EOF
  ANF:
  let main = let a = 1 in
         let b = 2 in
         let c = ( + ) a b in
         println_int c
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      call runtime_init
      li a0,1
      sd a0,0(s0)  # a
      li a0,2
      sd a0,-8(s0)  # b
      ld t0,0(s0)  # a
      ld t1,-8(s0)  # b
      add a0,t0,t1  # a ( + ) b
      sd a0,-16(s0)  # c
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,-16(s0)  # c
      call apply_closure_1
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  3

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let sum x y = x + y in
  >   println_int (sum 1 2)
  > EOF
  ANF:
  let ll_sum_1 x y = ( + ) x y
  let main = let anf2 = ll_sum_1 1 2 in
    println_int anf2
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl ll_sum_1
      .type ll_sum_1, @function
  ll_sum_1:
      # args: x, y
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # x
      sd a1,-8(s0)  # y
      ld t0,0(s0)  # x
      ld t1,-8(s0)  # y
      add a0,t0,t1  # x ( + ) y
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      call runtime_init
      # Creating closure for ll_sum_1
      la a0,ll_sum_1
      li a1,2
      call create_closure
      li a1,1
      li a2,2
      call apply_closure_2
      sd a0,0(s0)  # anf2
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,0(s0)  # anf2
      call apply_closure_1
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  3

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let add x y = x + y in
  >   let mul x y = x * y in
  >   let muladd x y z = add (mul x y) z in
  >   println_int (muladd 2 3 4)
  > EOF
  ANF:
  let ll_add_1 x y = ( + ) x y
  let ll_mul_2 x_0 y_0 = ( * ) x_0 y_0
  let ll_muladd_3 add mul x_1 y_1 z = let anf3 = mul x_1 y_1 in
    add anf3 z
  let main = let anf5 = ll_muladd_3 ll_add_1 ll_mul_2 2 3 4 in
    println_int anf5
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl ll_add_1
      .type ll_add_1, @function
  ll_add_1:
      # args: x, y
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # x
      sd a1,-8(s0)  # y
      ld t0,0(s0)  # x
      ld t1,-8(s0)  # y
      add a0,t0,t1  # x ( + ) y
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  
      .globl ll_mul_2
      .type ll_mul_2, @function
  ll_mul_2:
      # args: x_0, y_0
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # x_0
      sd a1,-8(s0)  # y_0
      ld t0,0(s0)  # x_0
      ld t1,-8(s0)  # y_0
      mul a0,t0,t1  # x_0 ( * ) y_0
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  
      .globl ll_muladd_3
      .type ll_muladd_3, @function
  ll_muladd_3:
      # args: add, mul, x_1, y_1, z
      addi sp,sp,-64
      sd ra,64(sp)
      sd s0,56(sp)
      addi s0,sp,48  # Prologue ends
      sd a0,0(s0)  # add
      sd a1,-8(s0)  # mul
      sd a2,-16(s0)  # x_1
      sd a3,-24(s0)  # y_1
      sd a4,-32(s0)  # z
      ld a0,-8(s0)  # mul
      ld a1,-16(s0)  # x_1
      ld a2,-24(s0)  # y_1
      call apply_closure_2
      sd a0,-40(s0)  # anf3
      ld a0,0(s0)  # add
      ld a1,-40(s0)  # anf3
      ld a2,-32(s0)  # z
      call apply_closure_2
      ld s0,56(sp)  # Epilogue starts
      ld ra,64(sp)
      addi sp,sp,64
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      call runtime_init
      # Creating closure for ll_add_1
      la a0,ll_add_1
      li a1,2
      call create_closure
      sd a0,0(s0)
      # Creating closure for ll_mul_2
      la a0,ll_mul_2
      li a1,2
      call create_closure
      sd a0,-8(s0)
      # Creating closure for ll_muladd_3
      la a0,ll_muladd_3
      li a1,5
      call create_closure
      ld a1,0(s0)
      ld a2,-8(s0)
      li a3,2
      li a4,3
      li a5,4
      call apply_closure_5
      sd a0,-16(s0)  # anf5
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,-16(s0)  # anf5
      call apply_closure_1
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  10

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let rec is_even x = match x with 
  >   | 0 -> true 
  >   | 1 -> false
  >   | _ -> is_even (x - 2)
  > let main = 
  >   let x = println_bool (is_even 42) in
  >   println_bool (is_even 43)
  > EOF
  ANF:
  let is_even x =
         let anf1 = ( = ) x 0 in
         if anf1 
         then true 
         else
           let anf3 = ( = ) x 1 in
           if anf3 
           then false 
           else let anf5 = ( - ) x 2 in
             is_even anf5
  let main =
    let anf8 = is_even 42 in
    let x_0 = println_bool anf8 in
    let anf7 = is_even 43 in
    println_bool anf7
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl is_even
      .type is_even, @function
  is_even:
      # args: x
      addi sp,sp,-48
      sd ra,48(sp)
      sd s0,40(sp)
      addi s0,sp,32  # Prologue ends
      sd a0,0(s0)  # x
      ld t0,0(s0)  # x
      li t1,0
      xor a0,t0,t1
      seqz a0,a0
      sd a0,-8(s0)  # anf1
      ld t0,-8(s0)  # anf1
      beq t0,zero,.else_0
      li a0,1
      j .end_0
  .else_0:
      ld t0,0(s0)  # x
      li t1,1
      xor a0,t0,t1
      seqz a0,a0
      sd a0,-16(s0)  # anf3
      ld t0,-16(s0)  # anf3
      beq t0,zero,.else_1
      li a0,0
      j .end_1
  .else_1:
      ld t0,0(s0)  # x
      li t1,2
      sub a0,t0,t1  # x ( - ) 2
      sd a0,-24(s0)  # anf5
      # Creating closure for is_even
      la a0,is_even
      li a1,1
      call create_closure
      ld a1,-24(s0)  # anf5
      call apply_closure_1
  .end_1:
  .end_0:
      ld s0,40(sp)  # Epilogue starts
      ld ra,48(sp)
      addi sp,sp,48
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      call runtime_init
      # Creating closure for is_even
      la a0,is_even
      li a1,1
      call create_closure
      li a1,42
      call apply_closure_1
      sd a0,0(s0)  # anf8
      # Creating closure for ml_println_bool
      la a0,ml_println_bool
      li a1,1
      call create_closure
      ld a1,0(s0)  # anf8
      call apply_closure_1
      sd a0,-8(s0)  # x_0
      # Creating closure for is_even
      la a0,is_even
      li a1,1
      call create_closure
      li a1,43
      call apply_closure_1
      sd a0,-16(s0)  # anf7
      # Creating closure for ml_println_bool
      la a0,ml_println_bool
      li a1,1
      call create_closure
      ld a1,-16(s0)  # anf7
      call apply_closure_1
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  true
  false

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let apply f x = f x
  > let compose f g = fun x -> f (g x)
  > let main = 
  >   let f = fun x -> x + 1 in
  >   let g = fun x -> x * 2 in
  >   println_int (apply (compose f g) 3)
  > EOF
  ANF:
  let apply f x = f x
  let compose f_0 g x_0 = let anf2 = g x_0 in
    f_0 anf2
  let ll_f_1_3 x_1 = ( + ) x_1 1
  let ll_g_0_4 x_2 = ( * ) x_2 2
  let main =
    let anf7 = compose ll_f_1_3 ll_g_0_4 in
    let anf6 = apply anf7 3 in
    println_int anf6
  
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  7

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let res = (1 + 2) * 3 + 4 * (32 / (3 + 5)) in
  >   println_int res
  > EOF
  ANF:
  let main =
         let anf5 = ( + ) 1 2 in
         let anf1 = ( * ) anf5 3 in
         let anf4 = ( + ) 3 5 in
         let anf3 = ( / ) 32 anf4 in
         let anf2 = ( * ) 4 anf3 in
         let res = ( + ) anf1 anf2 in
         println_int res
  
  $ cat /tmp/out.s
  .section .data
  
  .section .text
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-64
      sd ra,64(sp)
      sd s0,56(sp)
      addi s0,sp,48  # Prologue ends
      call runtime_init
      li t0,1
      li t1,2
      add a0,t0,t1  # 1 ( + ) 2
      sd a0,0(s0)  # anf5
      ld t0,0(s0)  # anf5
      li t1,3
      mul a0,t0,t1  # anf5 ( * ) 3
      sd a0,-8(s0)  # anf1
      li t0,3
      li t1,5
      add a0,t0,t1  # 3 ( + ) 5
      sd a0,-16(s0)  # anf4
      li t0,32
      ld t1,-16(s0)  # anf4
      div a0,t0,t1  # 32 ( / ) anf4
      sd a0,-24(s0)  # anf3
      li t0,4
      ld t1,-24(s0)  # anf3
      mul a0,t0,t1  # 4 ( * ) anf3
      sd a0,-32(s0)  # anf2
      ld t0,-8(s0)  # anf1
      ld t1,-32(s0)  # anf2
      add a0,t0,t1  # anf1 ( + ) anf2
      sd a0,-40(s0)  # res
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,-40(s0)  # res
      call apply_closure_1
      ld s0,56(sp)  # Epilogue starts
      ld ra,64(sp)
      addi sp,sp,64
      ret
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  25

  $ dune exec riscv -- -o /tmp/out.s <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > 
  > let main =
  >   pythagoras_cps 3 4 (fun res -> println_int res)
  > EOF
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  25

  $ dune exec riscv -- -o /tmp/out.s <<- EOF
  > let fn x y = x + y
  > let const = 42
  > let const_tuple = (const, 1 + const, fn 2 const)
  > let main = 
  >   let _ = print_tuple const_tuple in
  >   ()
  > EOF
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  (42, 43, 44)

  $ dune exec riscv -- -o /tmp/out.s <<- EOF
  > let fn r1 r2 r3 r4 r5 r6 r7 h0 = 
  >   r1 + r2 + r3 + r4 + r5 + r6 + r7 + 
  >   h0 
  > let main = 
  >   let res = fn 1 1 1 1 1 1 1 1 in
  >   let _ = println_int res in
  >   ()
  > EOF
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  8

  $ dune exec riscv -- -o /tmp/out.s <<- EOF
  > let fn r1 r2 r3 r4 r5 r6 r7 h0 h1 h2 h3 h4 h5 h6 h7 = 
  >   r1 + r2 + r3 + r4 + r5 + r6 + r7 + 
  >   h0 + h1 + h2 + h3 + h4 + h5 + h6 + h7
  > let main = 
  >   let res = fn 1 1 1 1 1 1 1   1 1 1 1 1 1 1 1 in
  >   let _ = println_int res in
  >   ()
  > EOF
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  15

  $ dune exec riscv -- -o /tmp/out.s <<- EOF
  > let fn x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99 = 
  >   x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 + x43 + x44 + x45 + x46 + x47 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + x55 + x56 + x57 + x58 + x59 + x60 + x61 + x62 + x63 + x64 + x65 + x66 + x67 + x68 + x69 + x70 + x71 + x72 + x73 + x74 + x75 + x76 + x77 + x78 + x79 + x80 + x81 + x82 + x83 + x84 + x85 + x86 + x87 + x88 + x89 + x90 + x91 + x92 + x93 + x94 + x95 + x96 + x97 + x98 + x99
  > 
  > let main = 
  >   let res = fn  1 1 1 1 1  1 1 1 1 1  1 1 1 1 1  1 1 1 1 1 in
  >   let res = res 1 1 1 1 1  1 1 1 1 1  1 1 1 1 1  1 1 1 1 1 in
  >   let res = res 1 1 1 1 1  1 1 1 1 1  1 1 1 1 1  1 1 1 1 1 in
  >   let res = res 1 1 1 1 1  1 1 1 1 1  1 1 1 1 1  1 1 1 1 1 in
  >   let res = res 1 1 1 1 1  1 1 1 1 1  1 1 1 1 1  1 1 1 1 1 in
  >   let _ = println_int res in
  >   ()
  > EOF
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  100


  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > let main = 
  >   let _ = (pythagoras_cps 3 4 (fun res -> println_int res)) in ()
  > EOF
  ANF:
  let add_cps x y k = let anf1 = ( + ) x y in
         k anf1
  let square_cps x_0 k_0 = let anf3 = ( * ) x_0 x_0 in
    k_0 anf3
  let ll_4 k_1 x_squared y_squared = add_cps x_squared y_squared k_1
  let ll_3 k_1 y_0 x_squared =
    let anf6 = ll_4 k_1 x_squared in
    square_cps y_0 anf6
  let pythagoras_cps x_1 y_0 k_1 =
    let anf8 = ll_3 k_1 y_0 in
    square_cps x_1 anf8
  let ll_7 res = println_int res
  let main = let anf10 = pythagoras_cps 3 4 ll_7 in
    ()
  
$ cat /tmp/out.s
  $ riscv64-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  25
