  $ ./inferencerTests.exe <<-EOF
  > let rec factorial n = if n <= 1  then 1 else factorial (n - 1) * n
  > let x5 = factorial 5 
  > EOF
  factorial : int -> int
  x5 : int
  $ ./inferencerTests.exe <<-EOF
  > let rec fibonacci n = if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)
  > let x5 = fibonacci 5 
  > EOF
  fibonacci : int -> int
  x5 : int

