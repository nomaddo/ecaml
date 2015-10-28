open Sexplib.Std

type t = {pident_loc:Location.t; pident_desc:longident_desc}
and longident_desc =
  | Pident of string
  | Pdot of string * t
with sexp

let rec print fmt {pident_desc} =
  match pident_desc with
  | Pident str -> Format.fprintf fmt "%s" str
  | Pdot (str, t) -> Format.fprintf fmt "%s.%a" str print t
