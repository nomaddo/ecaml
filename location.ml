open Sexplib.Std

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
} with sexp

type t = {
  pos_start: position;
  pos_end: position;
} with sexp
