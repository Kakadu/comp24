  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let a = [1;2;3] in
  >   print_list a
  > EOF
  ANF:
  let main = let a0 = [1; 2; 3] in
         print_list a0
  
  $ cat /tmp/lists.s
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
      call ml_create_list
      sd a0,0(s0)  # list
      li a0,3
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a0,2
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a0,1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      sd a0,-8(s0)  # a0
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a0
      call apply_closure_1
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [1, 2, 3]


  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let list = [1;2;3;4] in
  >   let a :: b :: c = list in
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = print_list c in
  >   0
  > EOF
  ANF:
  let main =
         let a0 = [1; 2; 3; 4] in
         let a19 = `list_is_empty a0 in
         let a15 = not a19 in
         let a18 = `list_tl a0 in
         let a17 = `list_is_empty a18 in
         let a16 = not a17 in
         let a2 = ( && ) a15 a16 in
         if a2 
         then
           let a4 = `list_hd a0 in
           let a13 = `list_tl a0 in
           let a6 = `list_hd a13 in
           let a12 = `list_tl a0 in
           let a8 = `list_tl a12 in
           let a9 = println_int a4 in
           let a10 = println_int a6 in
           let a11 = print_list a8 in
           0 
         else panic ()
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  1
  2
  [3, 4]

;  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
;  > let rec map f list = match list with
;  >   | hd::tl -> (f hd) :: (map f tl) 
;  >   | [] -> []
;  > 
;  > let sq x = x * x 
;  > 
;  > let main = 
;  >   let full = [2;3;4] in
;  >   let empty = [] in
;  >   let _ = print_list (map sq full) in
;  >   let _ = print_list (map sq empty) in
;  >   0
;  > EOF
;  ANF:
;  let map f list =
;         let a12 = `list_is_empty list in
;         let a1 = not a12 in
;         if a1 
;         then
;           let a3 = `list_hd list in
;           let a5 = `list_tl list in
;           let a7 = f a3 in
;           let a8 = map f a5 in
;           ( :: ) a7 a8 
;         else let a10 = `list_is_empty list in
;           if a10 
;           then [] 
;           else panic ()
;  let sq x = ( * ) x x
;  let main =
;    let a14 = [2; 3; 4] in
;    let a15 = [] in
;    let a19 = map sq a14 in
;    let a16 = print_list a19 in
;    let a18 = map sq a15 in
;    let a17 = print_list a18 in
;    0
;  
;$ cat /tmp/lists.s
;  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
;  $ /tmp/lists
;  [4, 9, 16]
;  []

;  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
;  > let [a; b; c] = [1; 2; 3]
;  > let main = 
;  >   let _ = println_int a in
;  >   let _ = println_int b in
;  >   let _ = println_int c in
;  >   0
;  > EOF
;  ANF:
;  let temp_list = [1; 2; 3]
;  let a = `list_field temp_list 0
;  let b = `list_field temp_list 1
;  let c = `list_field temp_list 2
;  let main =
;    let a3 = println_int a in
;    let a4 = println_int b in
;    let a5 = println_int c in
;    0
;  
;$ cat /tmp/lists.s
;  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
;  $ /tmp/lists
;  1
;  2
;  3

