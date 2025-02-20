module Format = Format

(* Variable identifier, represented as an integer *)
type var_id = int

(* Pretty-print a variable identifier *)
val pp_var_id : Format.formatter -> var_id -> unit

(* Convert a variable identifier to a string *)
val show_var_id : var_id -> string

(* Inferred type *)
type inf_type =
  | TInt (* Integer type *)
  | TBool (* Boolean type *)
  | TString (* String type *)
  | TUnit (* Unit type *)
  | TList of inf_type (* List type *)
  | TTuple of inf_type list (* Tuple type *)
  | TArrow of inf_type * inf_type (* Function type (T1 -> T2) *)
  | TPVar of var_id (* Type variable *)

(* Pretty-print an inferred type *)
val pp_inf_type : Format.formatter -> inf_type -> unit

(* Type inference and unification errors *)
type error =
  | Variable_not_found of string (* Unbound variable *)
  | Unification_failed of inf_type * inf_type (* Type mismatch *)
  | Incorrect_left_let_side (* let rec (a,b) = 1,2 *)
  | Incorrect_starting_point of Ast.expr (* Invalid program start *)
  | Empty_program (* Program contains no expressions *)

(* Pretty-print an error message *)
val pp_error : Format.formatter -> error -> unit

(* Convert an error to a string *)
val show_error : error -> string

(* Set of type variables *)
module VarSet : sig
  type t = Set.Make(Base.Int).t

  (* Pretty-print a set of type variables *)
  val pp : Format.formatter -> t -> unit
end

(* Main monad *)
module R : sig
  type 'a t = var_id -> var_id * ('a, error) result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a -> 'b -> 'b * ('c, 'a) result
  val return : 'a -> 'b -> 'b * ('a, 'c) result
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fold : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (* Run the computation with an initial variable ID *)
  val run : (var_id -> 'a * 'b) -> 'b
end

(* Module for basic work with types *)
module Type : sig
  type t = inf_type
end

(* Substitutions for type inference *)
module Subst : sig
  type t = (var_id, inf_type, Base.Int.comparator_witness) Base.Map.t

  (* Apply a substitution to an inferred type *)
  val apply : inf_type -> (var_id, inf_type, 'a) Base.Map.t -> inf_type

  (* Pretty-print a substitution *)
  val pp : Format.formatter -> t -> unit
end

(* A type scheme: quantified variables and a type *)
type scheme = Scheme of VarSet.t * inf_type

module Scheme : sig
  type t = scheme

  (* Pretty-print a type scheme *)
  val pp : Format.formatter -> scheme -> unit
end

(* Type environment: mapping variable names to type schemes *)
module TypeEnv : sig
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  (* Empty type environment *)
  val empty : t

  (* Default type environment *)
  val default : t

  (* Pretty-print a type environment *)
  val pp : Format.formatter -> t -> unit
end

(* Type inference functions *)
module Infer : sig
  val infer_pattern : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * inf_type) R.t
  val infer_expr : TypeEnv.t -> Ast.expr -> (Subst.t * inf_type) R.t
  val infer_prog : TypeEnv.t -> Ast.prog -> TypeEnv.t R.t
end
