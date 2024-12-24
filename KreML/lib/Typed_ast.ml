

let rec is_even a =
  is_odd (a - 1)
and is_odd a =
  is_even (a-1)

and x = 6