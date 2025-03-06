open Llvm

let map_ident_to_runtime = function
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "/" -> "div"
  | "=" -> "eq"
  | "!=" -> "neq"
  | "<" -> "less"
  | "<=" -> "leq"
  | ">" -> "gre"
  | ">=" -> "geq"
  | "&&" -> "and"
  | "||" -> "or"
  | "base +" -> "uplus"
  | "base -" -> "uminus"
  | other -> other
;;

module R = struct
  module Env = struct
    let empty = Base.Map.empty (module Base.String)
    let update map key value = Base.Map.update map key ~f:(fun _ -> value)
    let lookup_env key map = Base.Map.find map key
  end

  open Base.Result

  type env_var = (string, llvalue, Base.String.comparator_witness) Base.Map.t
  type env = env_var
  type 'a t = env -> env * ('a, string) Result.t

  let fail error env = env, fail error
  let return value env = env, return value

  let ( >>= ) (monad : 'a t) f env =
    let env, result = monad env in
    match result with
    | Error e -> env, Error e
    | Ok value -> f value env
  ;;

  let ( let* ) = ( >>= )
  let ( >>| ) m f = m >>= fun x -> return @@ f x
  let run m = snd @@ m Env.empty

  let map f xs =
    let* res =
      List.fold_left
        (fun acc x ->
          let* acc = acc in
          let* res = f x in
          return (res :: acc))
        (return [])
        xs
    in
    return (List.rev res)
  ;;

  let lookup_env_var name (env : env) =
    let env_var = env in
    let n = Env.lookup_env (map_ident_to_runtime name) env_var in
    env, Ok n
  ;;

  let update_var key value (env_var : env) = Env.update env_var key value, Ok ()
  let get_env env = env, Ok env
  let clean_env (_ : env) = Env.empty, Ok ()
end

let context = global_context ()
let the_module = create_module context "HTML"
let target_triple = Llvm_target.Target.default_triple ()
let () = Llvm.set_target_triple target_triple the_module

let () =
  let () = assert (Llvm_executionengine.initialize ()) in
  let target = Llvm_target.Target.by_triple target_triple in
  let machine = Llvm_target.TargetMachine.create ~triple:target_triple target in
  let data_layout = Llvm_target.TargetMachine.data_layout machine in
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string data_layout) the_module
;;

let builder = builder context
let i64 = i64_type context
let void = void_type context
