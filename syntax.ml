open Batteries
open Sexplib.Std

type pattern = {ppat_loc:Location.t; ppat_desc:pattern_desc}
and pattern_desc =
  | Ppat_any
  | Ppat_unit
  | Ppat_var of Longident.t
  | Ppat_tuple of pattern list
  | Ppat_record of (Longident.t * pattern) list
with sexp

type rec_flag = bool with sexp

type expression = {pexp_loc:Location.t; pexp_desc:expression_desc}
and expression_desc =
  | Bool of bool
  | Int of int
  | Float of float
  | Seq of expression * expression
  | Var of Longident.t
  | Let of rec_flag * (pattern * expression) list * expression
with sexp

type type_decl = unit with sexp

type declaration =
  | Exp of expression
  | Type of type_decl with sexp

let r = ref (Sexplib.Std.sexp_of_int 1)
