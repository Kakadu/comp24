type ('st, 'a) t = 'st -> 'st * ('a, string) Result.t

let return x : _ = fun st -> st, Result.ok x
let fail err st = st, Result.error err

let ( >>= ) : 's 'a 'b. ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
  fun l r : _ ->
  fun st ->
  let st, x = l st in
  match x with
  | Result.Ok x -> r x st
  | Result.Error s -> st, Result.error s
;;

let read : ('st, 'st) t = fun st -> st, Result.ok st
let write : 'st -> ('st, unit) t = fun s _oldstate -> s, Result.ok ()
let run (f : ('st, 'a) t) : 'st -> 'st * ('a, string) Result.t = f
let ( let* ) = ( >>= )
let ( *> ) l r = l >>= fun _ -> r
let ( >>| ) l r = l >>= fun x -> return (r x)
let ( <* ) l r = l >>= fun h -> r >>= fun _ -> return h

let map_list : 'a 'b 's. ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
  fun custom_f mlst ->
  let f lst res =
    let* tl = lst in
    let* x = custom_f res in
    return (x :: tl)
  in
  List.fold_left f (return []) mlst >>| List.rev
;;

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type local_map = Llvm.llvalue MapString.t
type glob_funs = (Llvm.lltype * Llvm.llvalue) MapString.t

(** current fucntion name, fuction table (type + llvm value), local variables*)
type state = string * glob_funs * local_map

let read_curr_fun : (state, string) t =
  let* fn, _, _ = read in
  return fn
;;

let write_curr_fun : string -> (state, unit) t =
  fun fn ->
  let* _, gf, lm = read in
  write (fn, gf, lm)
;;

let read_glob_funs : (state, glob_funs) t =
  let* _, gf, _ = read in
  return gf
;;

let write_glob_funs : glob_funs -> (state, unit) t =
  fun gf ->
  let* fn, _, lm = read in
  write (fn, gf, lm)
;;

let findopt_glob_fun : string -> (state, (Llvm.lltype * Llvm.llvalue) option) t =
  fun fun_name -> read_glob_funs >>| MapString.find_opt fun_name
;;

let add_glob_fun : string -> Llvm.lltype -> Llvm.llvalue -> (state, unit) t =
  fun fun_name fun_tp fun_val ->
  read_glob_funs
  >>= fun gf ->
  let new_gf = MapString.add fun_name (fun_tp, fun_val) gf in
  write_glob_funs new_gf
;;

let read_local_vars : (state, local_map) t =
  let* _, _, lm = read in
  return lm
;;

let write_local_vars : local_map -> (state, unit) t =
  fun lm ->
  let* fn, gf, _ = read in
  write (fn, gf, lm)
;;

let findopt_loc_var : string -> (state, Llvm.llvalue option) t =
  fun var_name -> read_local_vars >>| MapString.find_opt var_name
;;

let add_loc_var : string -> Llvm.llvalue -> (state, unit) t =
  fun var_name var_val ->
  read_local_vars
  >>= fun lm ->
  let new_lm = MapString.add var_name var_val lm in
  write_local_vars new_lm
;;

let new_fun_scope new_fun code =
  let* prev_fun = read_curr_fun <* write_curr_fun new_fun in
  let* prev_lm = read_local_vars <* write_local_vars MapString.empty in
  code <* write_curr_fun prev_fun <* write_local_vars prev_lm
;;
