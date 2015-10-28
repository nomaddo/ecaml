open Batteries
open Sexplib

let rec print_list ~sep f fmt list =
  match list with
  | [] -> ()
  | [x] -> f fmt x
  | x::xs ->
    Format.fprintf fmt "%a%s" f x sep;
    print_list ~sep f fmt xs
