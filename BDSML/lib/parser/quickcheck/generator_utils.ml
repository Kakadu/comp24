open QCheck.Gen

let gen_rest_of_ident =
  string_size
    ~gen:
      (oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; char_range '0' '9'; return '_' ])
    (int_range 0 10)
;;

let gen_ident =
  let* first_char = char_range 'a' 'z' in
  let* rest = gen_rest_of_ident in
  return (String.make 1 first_char ^ rest)
;;

let gen_capitalized_ident =
  let* first_char = char_range 'A' 'Z' in
  let* rest = gen_rest_of_ident in
  return (String.make 1 first_char ^ rest)
;;

let gen_list_helper main_gen_fun depth =
  let* len = int_range 2 5 in
  list_repeat len (main_gen_fun (depth / len))
;;

let gen_construct gen construct depth tuple =
  let* constr = oneof [ return "::"; return "[]"; gen_capitalized_ident ] in
  match constr with
  | "::" ->
    let* g = gen (depth / 2) in
    return (construct (constr, Some g))
  | "[]" -> return (construct (constr, None))
  | _ ->
    let* gt = gen_list_helper gen depth in
    return (construct (constr, Some (tuple gt)))
;;
