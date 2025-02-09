val pp_el
  :  ('a -> bool)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit

val pp_list
  :  ('a -> bool)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> (unit, Format.formatter, unit) format
  -> 'a list
  -> unit

val pp_tuple
  :  ('a -> bool)
  -> (Format.formatter -> 'a -> unit)
  -> (unit, Format.formatter, unit) format
  -> Format.formatter
  -> 'a list
  -> unit
