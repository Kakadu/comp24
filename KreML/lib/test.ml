let rec is_even a =
  if a = 0 then true else is_odd (a - 1)
and is_odd a =
  if a = 1 then true else is_even ( a - 1)

and c = (5, 6)