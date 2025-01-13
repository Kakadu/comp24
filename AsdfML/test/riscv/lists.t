  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let a = [1;2;3] in
  >   print_list a
  > EOF
  ANF:
  let main = let a0 = [1; 2; 3] in
         print_list a0
  
  $ cat /tmp/lists.s
  
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
      li a1,3
      ld a0,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a1,2
      ld a0,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a1,1
      ld a0,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      sd a0,-8(s0)  # a0
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a0
      call apply_closure
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [1, 2, 3]

  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let rec map f list = match list with
  > | [] -> []
  > | hd::tl -> (f hd) :: (map f tl)
  > let main = 
  >   let sq = (fun x -> x * x) in
  >   let x = [1;2;3] in
  >   let res = map sq x in
  >   print_list res
  > EOF
  ANF:
  let map f list =
         let a1 = `list_is_empty list in
         if a1 
         then [] 
         else
           let a3 = `list_hd list in
           let a5 = `list_tl list in
           let a10 = f a3 in
           let a7 = ( :: ) a10 in
           let a9 = map f in
           let a8 = a9 a5 in
           a7 a8
  let `sq_4 x = let a12 = ( * ) x in
    a12 x
  let main =
    let a13 = `sq_4 in
    let a14 = [1; 2; 3] in
    let a18 = map a13 in
    let a16 = a18 a14 in
    print_list a16
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [1, 4, 9]

$ dune exec riscv -- -o /tmp/lists.s <<- EOF
> let rec map = fun f -> fun list -> match list with
> | [] -> []
> | hd::tl -> (f hd) :: (map f tl)
> 
> let rec fold =
>   fun init -> fun f -> fun list -> 
>   match list with
>   | [] -> init
>   | hd :: tl -> fold (f init hd) f tl
> 
> let rec filter = fun f -> fun list ->
>   match list with
>   | [] -> []
>   | hd :: tl -> if f hd then hd :: filter f tl else filter f tl 
> 
> let main = 
>   let gt0 = filter (fun x -> x > 0) in
>   let sq = map (fun x -> x * x) in
>   let sum = fold 0 (fun acc -> fun x -> acc + x) in
>   let x = [1;2;3] in
>   let res = sum (sq (gt0 x)) in
>   print_int res
> EOF
$ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
$ /tmp/lists
