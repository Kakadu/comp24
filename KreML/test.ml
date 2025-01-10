let rec f a =
  if (a < 0) then 1 else a * f (a - 1)