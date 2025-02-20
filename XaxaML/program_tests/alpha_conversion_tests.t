$ ./run_alpha_conversion.exe << EOF
> let a = 1
> let a = 1
> let f y = 
>   let x k = k in 
>   a + x 1 + y
> let x = 1