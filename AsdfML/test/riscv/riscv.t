$ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
> 
> EOF
$ cat /tmp/out.s
$ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
$ /tmp/out

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 42
  > EOF
  ANF:
  let main = 42
  
  $ cat /tmp/out.s
  
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
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  [42]

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = print_int 42
  > EOF
  ANF:
  let main = print_int 42
  
  $ cat /tmp/out.s
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      call runtime_init
      # Creating closure for ml_print_int
      la a0,ml_print_int
      li a1,1
      call create_closure
      li a1,42
      call apply_closure_1
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  42

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let a = 1 in
  >   let b = 2 in
  >   let c = a + b in
  >   print_int c
  > EOF
  ANF:
  let main = let a0 = 1 in
         let a1 = 2 in
         let a3 = ( + ) a0 a1 in
         print_int a3
  
  $ cat /tmp/out.s
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-48
      sd ra,48(sp)
      sd s0,40(sp)
      addi s0,sp,32  # Prologue ends
      call runtime_init
      li a0,1
      sd a0,0(s0)  # a0
      li a0,2
      sd a0,-8(s0)  # a1
      # Creating closure for ml_add
      la a0,ml_add
      li a1,2
      call create_closure
      ld a1,0(s0)  # a0
      ld a2,-8(s0)  # a1
      call apply_closure_2
      sd a0,-16(s0)  # a3
      # Creating closure for ml_print_int
      la a0,ml_print_int
      li a1,1
      call create_closure
      ld a1,-16(s0)  # a3
      call apply_closure_1
      ld s0,40(sp)  # Epilogue starts
      ld ra,48(sp)
      addi sp,sp,48
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  3

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let sum x y = x + y in
  >   print_int (sum 1 2)
  > EOF
  ANF:
  let `sum_1 x y = ( + ) x y
  let main = let a1 = `sum_1 in
    let a3 = a1 1 2 in
    print_int a3
  
  $ cat /tmp/out.s
  
      .globl sum_1
      .type sum_1, @function
  sum_1:
      # args: x, y
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      sd a0,0(s0)  # x
      sd a1,-8(s0)  # y
      # Creating closure for ml_add
      la a0,ml_add
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      ld a2,-8(s0)  # y
      call apply_closure_2
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      call runtime_init
      # Creating closure for sum_1
      la a0,sum_1
      li a1,2
      call create_closure
      sd a0,0(s0)  # a1
      li a1,1
      li a2,2
      call apply_closure_2
      sd a0,-8(s0)  # a3
      # Creating closure for ml_print_int
      la a0,ml_print_int
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a3
      call apply_closure_1
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  3

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let main = 
  >   let add x y = x + y in
  >   let mul x y = x * y in
  >   let muladd x y z = add (mul x y) z in
  >   print_int (muladd 2 3 4)
  > EOF
  ANF:
  let `add_1 x y = ( + ) x y
  let `mul_2 x y = ( * ) x y
  let `muladd_3 add mul x y z = let a3 = mul x y in
    add a3 z
  let main =
    let a4 = `add_1 in
    let a5 = `mul_2 in
    let a6 = `muladd_3 in
    let a8 = a6 a4 a5 2 3 4 in
    print_int a8
  
  $ cat /tmp/out.s
  
      .globl add_1
      .type add_1, @function
  add_1:
      # args: x, y
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      sd a0,0(s0)  # x
      sd a1,-8(s0)  # y
      # Creating closure for ml_add
      la a0,ml_add
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      ld a2,-8(s0)  # y
      call apply_closure_2
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  
      .globl mul_2
      .type mul_2, @function
  mul_2:
      # args: x, y
      addi sp,sp,-40
      sd ra,40(sp)
      sd s0,32(sp)
      addi s0,sp,24  # Prologue ends
      sd a0,0(s0)  # x
      sd a1,-8(s0)  # y
      # Creating closure for ml_mul
      la a0,ml_mul
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      ld a2,-8(s0)  # y
      call apply_closure_2
      ld s0,32(sp)  # Epilogue starts
      ld ra,40(sp)
      addi sp,sp,40
      ret
  
      .globl muladd_3
      .type muladd_3, @function
  muladd_3:
      # args: add, mul, x, y, z
      addi sp,sp,-72
      sd ra,72(sp)
      sd s0,64(sp)
      addi s0,sp,56  # Prologue ends
      sd a0,0(s0)  # add
      sd a1,-8(s0)  # mul
      sd a2,-16(s0)  # x
      sd a3,-24(s0)  # y
      sd a4,-32(s0)  # z
      ld a0,-8(s0)  # mul
      ld a1,-16(s0)  # x
      ld a2,-24(s0)  # y
      call apply_closure_2
      sd a0,-40(s0)  # a3
      ld a0,0(s0)  # add
      ld a1,-40(s0)  # a3
      ld a2,-32(s0)  # z
      call apply_closure_2
      ld s0,64(sp)  # Epilogue starts
      ld ra,72(sp)
      addi sp,sp,72
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-56
      sd ra,56(sp)
      sd s0,48(sp)
      addi s0,sp,40  # Prologue ends
      call runtime_init
      # Creating closure for add_1
      la a0,add_1
      li a1,2
      call create_closure
      sd a0,0(s0)  # a4
      # Creating closure for mul_2
      la a0,mul_2
      li a1,2
      call create_closure
      sd a0,-8(s0)  # a5
      # Creating closure for muladd_3
      la a0,muladd_3
      li a1,5
      call create_closure
      sd a0,-16(s0)  # a6
      ld a1,0(s0)  # a4
      ld a2,-8(s0)  # a5
      li a3,2
      li a4,3
      li a5,4
      call apply_closure_5
      sd a0,-24(s0)  # a8
      # Creating closure for ml_print_int
      la a0,ml_print_int
      li a1,1
      call create_closure
      ld a1,-24(s0)  # a8
      call apply_closure_1
      ld s0,48(sp)  # Epilogue starts
      ld ra,56(sp)
      addi sp,sp,56
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  10

  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let rec is_even x = match x with 
  >   | 0 -> true 
  >   | 1 -> false
  >   | _ -> is_even (x - 2)
  > let main = 
  >   let x = print_bool (is_even 42) in
  >   print_bool (is_even 43)
  > EOF
  ANF:
  let is_even x =
         let a1 = ( = ) x 0 in
         if a1 
         then true 
         else
           let a3 = ( = ) x 1 in
           if a3 
           then false 
           else let a5 = ( - ) x 2 in
             is_even a5
  let main =
    let a10 = is_even 42 in
    let a7 = print_bool a10 in
    let a9 = is_even 43 in
    print_bool a9
  
  $ cat /tmp/out.s
  
      .globl is_even
      .type is_even, @function
  is_even:
      # args: x
      addi sp,sp,-56
      sd ra,56(sp)
      sd s0,48(sp)
      addi s0,sp,40  # Prologue ends
      sd a0,0(s0)  # x
      # Creating closure for ml_eq
      la a0,ml_eq
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      li a2,0
      call apply_closure_2
      sd a0,-8(s0)  # a1
      ld t0,-8(s0)  # a1
      beq t0,zero,.else_0
      li a0,1
      j .end_0
  .else_0:
      # Creating closure for ml_eq
      la a0,ml_eq
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      li a2,1
      call apply_closure_2
      sd a0,-16(s0)  # a3
      ld t0,-16(s0)  # a3
      beq t0,zero,.else_1
      li a0,0
      j .end_1
  .else_1:
      # Creating closure for ml_sub
      la a0,ml_sub
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      li a2,2
      call apply_closure_2
      sd a0,-24(s0)  # a5
      # Creating closure for is_even
      la a0,is_even
      li a1,1
      call create_closure
      ld a1,-24(s0)  # a5
      call apply_closure_1
  .end_1:
  .end_0:
      ld s0,48(sp)  # Epilogue starts
      ld ra,56(sp)
      addi sp,sp,56
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-48
      sd ra,48(sp)
      sd s0,40(sp)
      addi s0,sp,32  # Prologue ends
      call runtime_init
      # Creating closure for is_even
      la a0,is_even
      li a1,1
      call create_closure
      li a1,42
      call apply_closure_1
      sd a0,0(s0)  # a10
      # Creating closure for ml_print_bool
      la a0,ml_print_bool
      li a1,1
      call create_closure
      ld a1,0(s0)  # a10
      call apply_closure_1
      sd a0,-8(s0)  # a7
      # Creating closure for is_even
      la a0,is_even
      li a1,1
      call create_closure
      li a1,43
      call apply_closure_1
      sd a0,-16(s0)  # a9
      # Creating closure for ml_print_bool
      la a0,ml_print_bool
      li a1,1
      call create_closure
      ld a1,-16(s0)  # a9
      call apply_closure_1
      ld s0,40(sp)  # Epilogue starts
      ld ra,48(sp)
      addi sp,sp,48
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  true
  false


  $ dune exec riscv -- -anf -o /tmp/out.s <<- EOF
  > let apply f x = f x
  > let compose f g = fun x -> f (g x)
  > let main = 
  >   let f = fun x -> x + 1 in
  >   let g = fun x -> x * 2 in
  >   print_int (apply (compose f g) 3)
  > EOF
  ANF:
  let apply f x = f x
  let compose f g x = let a2 = g x in
    f a2
  let `f_3 x = ( + ) x 1
  let `g_4 x = ( * ) x 2
  let main =
    let a5 = `f_3 in
    let a6 = `g_4 in
    let a9 = compose a5 a6 in
    let a8 = apply a9 3 in
    print_int a8
  
$ cat /tmp/out.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/out.s -o /tmp/out -L../../runtime/ -l:libruntime.a
  $ /tmp/out
  7
