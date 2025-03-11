  $ ./ll_runner.exe < manytests/typed/012fibcps.ml
  Bindings before transformations:
  val fib: int -> (int -> 'a) -> 'a
  val main: unit
  
  let ll_1 cc2_k cc3_a b =
    cc2_k ((( + ) cc3_a) b)
  ;;
  
  let ll_0 cc0_n cc1_k a =
    (fib ((( - ) cc0_n) 2)) ((ll_1 cc1_k) a)
  ;;
  
  let rec fib n k =
    (if (( < ) n) 2
    then
      k n
    else
      (fib ((( - ) n) 1)) ((ll_0 n) k))
  ;;
  
  let ll_2 x =
    x
  ;;
  
  let cc_ac0_main = print_int ((fib 6) ll_2)
  ;;
  
