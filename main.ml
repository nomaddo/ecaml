open Batteries
open Format
open Syntax

let rec printer fmt {pexp_desc}=
  let open Syntax in
  match pexp_desc with
  | Bool b -> Format.fprintf fmt "%b" b
  | Int i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%f" f
  | Seq (e1, e2) -> Format.fprintf fmt "%a; %a" printer e1 printer e2
  | Var lident -> Format.fprintf fmt "%a" Longident.print lident
  | Let (r, patexps, e) ->
    match patexps with
    | [] -> assert false
    | (p, e) :: [] ->
      Format.fprintf fmt "let %s%a=%a"
        (if r then "rec " else "")
        print_pattern p printer e
    | (p, e) :: xs ->
      Format.fprintf fmt "let %s%a=%a %a"
        (if r then "rec " else "")
        print_pattern p printer e
        (fun fmt -> List.iter (fun (p, e) -> Format.fprintf fmt "and %a = %a" print_pattern p printer e)) xs

and print_decl fmt = function
| Exp e -> Format.fprintf fmt "%a;; " printer e
| Type _ -> assert false

and print_pattern fmt {ppat_desc} =
  match ppat_desc with
| Ppat_any -> fprintf fmt "_"
| Ppat_unit -> fprintf fmt "()"
| Ppat_var lident -> Longident.print fmt lident
| Ppat_tuple ps -> fprintf fmt "(%a)" (Misc.print_list ~sep:", " print_pattern) ps
| Ppat_record lps -> fprintf fmt "{%a}" (Misc.print_list ~sep:"; " print_label) lps

and print_label fmt (ident, pat) =
  fprintf fmt "%a=%a" Longident.print ident print_pattern pat

let process file =
  let lexbuf = Lexing.from_channel (open_in file) in
  let decls = Parser.toplevel Lexer.token lexbuf in
  List.iter (print_decl Format.std_formatter) decls

let rec loop () =
  let input = Scanf.scanf "%s" (fun x -> x) in
  let lexbuf = Lexing.from_string input in
  let decls = Parser.toplevel Lexer.token lexbuf in
  List.iter (print_decl Format.std_formatter) decls;
  loop ()

let () =
  try
    match Array.length Sys.argv with
    | 1 -> loop ()
    | 2 -> process Sys.argv.(1)
    | _ -> assert false
  with _ -> begin
    !Syntax.r
    |> Sexplib.Sexp.to_string |> Printf.printf "error: %s"
    end
