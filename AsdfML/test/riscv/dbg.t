;  $ if [ -z "$latest" ]; then
;  >   alias riscv64-linux-gnu-gcc='riscv64-unknown-linux-gnu-gcc'
;  >   alias qemu-riscv64-static='qemu-riscv64'
;  > fi
;
;  $ dune exec riscv -- -anf -o /tmp/dbg.s <<- EOF
;  > let pow x n =
;  >   let rec helper acc n =
;  >     match n with
;  >     | 0 -> acc
;  >     | n -> helper (acc * x) (n - 1)
;  >   in
;  >   helper 1 n
;  > 
;  > let main = 
;  >   let res = pow 2 10 in
;  >   let _ = println_int res in
;  >   ()
;  > EOF
;  ANF:
;  let ll_helper_1 x acc n_0 =
;         let anf1 = ( = ) n_0 0 in
;         if anf1 
;         then acc 
;         else
;           let anf3 = ( * ) acc x in
;           let anf4 = ( - ) n_0 1 in
;           ll_helper_1 x anf3 anf4
;  let pow x n = ll_helper_1 x 1 n
;  let main = let res = pow 2 10 in
;    let anf6 = println_int res in
;    ()
;  
;$ cat /tmp/dbg.s
;  $ riscv64-linux-gnu-gcc -static /tmp/dbg.s -o /tmp/dbg -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
;  $ RUST_LOG=debug qemu-riscv64-static /tmp/dbg
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16786, arity: 2, args: dec[] / hex[] } at 0x2a8920
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16786, arity: 2, args: dec[2, 10] / hex[2, a] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8af0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 1, 10] / hex[2, 1, a] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8ba0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 2, 9] / hex[2, 2, 9] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8c50
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 4, 8] / hex[2, 4, 8] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8d00
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 8, 7] / hex[2, 8, 7] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8db0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 16, 6] / hex[2, 10, 6] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8e60
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 32, 5] / hex[2, 20, 5] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8f10
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 64, 4] / hex[2, 40, 4] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a8fc0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 128, 3] / hex[2, 80, 3] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a9070
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 256, 2] / hex[2, 100, 2] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a9120
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 512, 1] / hex[2, 200, 1] }
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 3, args: dec[] / hex[] } at 0x2a91d0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 3, args: dec[2, 1024, 0] / hex[2, 400, 0] }
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Closure result: 1024 / 0x400
;  [runtime::closure]: Creating Closure { fn_ptr: 0x19e1c, arity: 1, args: dec[] / hex[] } at 0x2a90c0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x19e1c, arity: 1, args: dec[1024] / hex[400] }
;  1024
;  [runtime::closure]: Closure result: 0 / 0x0
;
;  $ if [ -z "$latest" ]; then
;  >   alias riscv64-linux-gnu-gcc='riscv64-unknown-linux-gnu-gcc'
;  >   alias qemu-riscv64-static='qemu-riscv64'
;  > fi
;
;  $ dune exec riscv -- -anf -o /tmp/dbg.s <<- EOF
;  > let wrap f = if 1 = 1 then f else f
;  > let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
;  > let main =
;  >   let a = 42 = 24 in 
;  >   let b = 42 != 24 in
;  >   let rez =
;  >     ((wrap test10) 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
;  >   in
;  >   let () = println_int rez in
;  >   0
;  > EOF
;  ANF:
;  let wrap f = let anf1 = ( = ) 1 1 in
;         if anf1 
;         then f 
;         else f
;  let test10 a b c d e f_0 g h i j =
;    let anf10 = ( + ) a b in
;    let anf9 = ( + ) anf10 c in
;    let anf8 = ( + ) anf9 d in
;    let anf7 = ( + ) anf8 e in
;    let anf6 = ( + ) anf7 f_0 in
;    let anf5 = ( + ) anf6 g in
;    let anf4 = ( + ) anf5 h in
;    let anf3 = ( + ) anf4 i in
;    ( + ) anf3 j
;  let main =
;    let a_0 = ( = ) 42 24 in
;    let b_0 = ( <> ) 42 24 in
;    let rez =
;      wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
;    let () = println_int rez in
;    0
;  
;  $ cat /tmp/dbg.s
;  .section .data
;  
;  .section .text
;  
;      .globl wrap
;      .type wrap, @function
;  wrap:
;      # args: f
;      addi sp,sp,-32
;      sd ra,32(sp)
;      sd s0,24(sp)
;      addi s0,sp,16  # Prologue ends
;      sd a0,0(s0)  # f
;      li t0,1
;      li t1,1
;      xor a0,t0,t1
;      seqz a0,a0
;      sd a0,-8(s0)  # anf1
;      ld t0,-8(s0)  # anf1
;      beq t0,zero,.else_0
;      ld a0,0(s0)  # f
;      j .end_0
;  .else_0:
;      ld a0,0(s0)  # f
;  .end_0:
;      ld s0,24(sp)  # Epilogue starts
;      ld ra,32(sp)
;      addi sp,sp,32
;      ret
;  
;      .globl test10
;      .type test10, @function
;  test10:
;      # args: a, b, c, d, e, f_0, g, h, i, j
;      addi sp,sp,-144
;      sd ra,144(sp)
;      sd s0,136(sp)
;      addi s0,sp,128  # Prologue ends
;      sd a0,0(s0)  # a
;      sd a1,-8(s0)  # b
;      sd a2,-16(s0)  # c
;      sd a3,-24(s0)  # d
;      sd a4,-32(s0)  # e
;      sd a5,-40(s0)  # f_0
;      sd a6,-48(s0)  # g
;      sd a7,-56(s0)  # Tuple for arguments
;      ld t0,0(s0)  # a
;      ld t1,-8(s0)  # b
;      add a0,t0,t1  # a ( + ) b
;      sd a0,-64(s0)  # anf10
;      ld t0,-64(s0)  # anf10
;      ld t1,-16(s0)  # c
;      add a0,t0,t1  # anf10 ( + ) c
;      sd a0,-72(s0)  # anf9
;      ld t0,-72(s0)  # anf9
;      ld t1,-24(s0)  # d
;      add a0,t0,t1  # anf9 ( + ) d
;      sd a0,-80(s0)  # anf8
;      ld t0,-80(s0)  # anf8
;      ld t1,-32(s0)  # e
;      add a0,t0,t1  # anf8 ( + ) e
;      sd a0,-88(s0)  # anf7
;      ld t0,-88(s0)  # anf7
;      ld t1,-40(s0)  # f_0
;      add a0,t0,t1  # anf7 ( + ) f_0
;      sd a0,-96(s0)  # anf6
;      ld t0,-96(s0)  # anf6
;      ld t1,-48(s0)  # g
;      add a0,t0,t1  # anf6 ( + ) g
;      sd a0,-104(s0)  # anf5
;      ld t0,-104(s0)  # anf5
;      ld t1,-56(s0)
;      ld t1,0(t1)
;      ld t1,0(t1)  # h
;      add a0,t0,t1  # anf5 ( + ) h
;      sd a0,-112(s0)  # anf4
;      ld t0,-112(s0)  # anf4
;      ld t1,-56(s0)
;      ld t1,0(t1)
;      ld t1,8(t1)  # i
;      add a0,t0,t1  # anf4 ( + ) i
;      sd a0,-120(s0)  # anf3
;      ld t0,-120(s0)  # anf3
;      ld t1,-56(s0)
;      ld t1,0(t1)
;      ld t1,16(t1)  # j
;      add a0,t0,t1  # anf3 ( + ) j
;      ld s0,136(sp)  # Epilogue starts
;      ld ra,144(sp)
;      addi sp,sp,144
;      ret
;  
;      .globl main
;      .type main, @function
;  main:
;      addi sp,sp,-64
;      sd ra,64(sp)
;      sd s0,56(sp)
;      addi s0,sp,48  # Prologue ends
;      call runtime_init
;      li t0,42
;      li t1,24
;      xor a0,t0,t1
;      seqz a0,a0
;      sd a0,0(s0)  # a_0
;      li t0,42
;      li t1,24
;      xor a0,t0,t1
;      snez a0,a0
;      sd a0,-8(s0)  # b_0
;      li a0,5
;      call ml_create_tuple
;      sd a0,-16(s0)  # tuple
;      li a2,100000
;      ld a0,-16(s0)
;      li a1,0
;      call ml_set_tuple_field
;      li a2,1000000
;      ld a0,-16(s0)
;      li a1,1
;      call ml_set_tuple_field
;      li a2,10000000
;      ld a0,-16(s0)
;      li a1,2
;      call ml_set_tuple_field
;      li a2,100000000
;      ld a0,-16(s0)
;      li a1,3
;      call ml_set_tuple_field
;      li a2,1000000000
;      ld a0,-16(s0)
;      li a1,4
;      call ml_set_tuple_field
;      # Creating closure for test10
;      la a0,test10
;      li a1,10
;      call create_closure
;      sd a0,-24(s0)
;      # Creating closure for wrap
;      la a0,wrap
;      li a1,1
;      call create_closure
;      ld a1,-24(s0)
;      li a2,1
;      li a3,10
;      li a4,100
;      li a5,1000
;      li a6,10000
;      ld a7,-16(s0)
;      call apply_closure_9plus
;      sd a0,-32(s0)  # rez
;      # Creating closure for ml_println_int
;      la a0,ml_println_int
;      li a1,1
;      call create_closure
;      ld a1,-32(s0)  # rez
;      call apply_closure_1
;      sd a0,-40(s0)  # ()
;      li a0,0
;      ld s0,56(sp)  # Epilogue starts
;      ld ra,64(sp)
;      addi sp,sp,64
;      ret
;  $ riscv64-linux-gnu-gcc -static /tmp/dbg.s -o /tmp/dbg -L../../runtime/ -l:libruntime.a -Wl,--no-warnings
;  $ RUST_LOG=debug qemu-riscv64-static /tmp/dbg
;  [runtime::tuple]: Created tuple of size 5 at 0x2a8800
;  [runtime::tuple]: Set [0] = 100000 in [100000, 0, 0, 0, 0] at 0x2a8800
;  [runtime::tuple]: Set [1] = 1000000 in [100000, 1000000, 0, 0, 0] at 0x2a8800
;  [runtime::tuple]: Set [2] = 10000000 in [100000, 1000000, 10000000, 0, 0] at 0x2a8800
;  [runtime::tuple]: Set [3] = 100000000 in [100000, 1000000, 10000000, 100000000, 0] at 0x2a8800
;  [runtime::tuple]: Set [4] = 1000000000 in [100000, 1000000, 10000000, 100000000, 1000000000] at 0x2a8800
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16742, arity: 10, args: dec[] / hex[] } at 0x2a8ad0
;  [runtime::closure]: Creating Closure { fn_ptr: 0x16712, arity: 1, args: dec[] / hex[] } at 0x2a8b20
;  [runtime::closure]: Tuple with arguments [100000, 1000000, 10000000, 100000000, 1000000000] at 0x2a8800
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 1, args: dec[2788048, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000] / hex[2a8ad0, 1, a, 64, 3e8, 2710, 186a0, f4240, 989680, 5f5e100, 3b9aca00] }
;  [runtime::closure]: Too many args, [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000] goes to next closure
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16712, arity: 1, args: dec[2788048] / hex[2a8ad0] }
;  [runtime::closure]: Closure result: 2788048 / 0x2a8ad0
;  [runtime::closure]: Applying Closure { fn_ptr: 0x16742, arity: 10, args: dec[1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000] / hex[1, a, 64, 3e8, 2710, 186a0, f4240, 989680, 5f5e100, 3b9aca00] }
;  [runtime::closure]: Tuple with arguments: [10000000, 100000000, 1000000000] at 0x2a9010
;  [runtime::closure]: Closure result: 1111111111 / 0x423a35c7
;  [runtime::closure]: Creating Closure { fn_ptr: 0x1a87a, arity: 1, args: dec[] / hex[] } at 0x2a8b70
;  [runtime::closure]: Applying Closure { fn_ptr: 0x1a87a, arity: 1, args: dec[1111111111] / hex[423a35c7] }
;  1111111111
;  [runtime::closure]: Closure result: 0 / 0x0
