let rec fib n k = if n<2 then n else fib (n-1) (fun a -> fib (n-2) (fun b -> k(a+b)))

let main = print_int(fib 6 (fun x -> x))