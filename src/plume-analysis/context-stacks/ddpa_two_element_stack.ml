open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
open Ddpa_context_stack;;
open Pp_utils;;

module Stack : Context_stack =
struct
  type t = S of abstract_clause list;;
  let equal x y = compare x y == 0;;
  let compare = compare;;
  let empty = S([]);;
  let push c (S(lst)) =
    match lst with
      | [] -> S([c])
      | h::_ -> S([c;h])
  ;;
  let pop (S(lst)) =
    match lst with
      | [] -> S([])
      | _::t -> S(t)
  ;;
  let is_top c (S(c_option)) =
    match c_option with
    | h::_ -> c = h
    | [] -> true
  ;;
  let pp formatter (S(lst)) =
    List.iter ((pp_suffix pp_var_of_abstract_clause "|") formatter) lst;
    Format.pp_print_string formatter "?"
  ;;
  let show = pp_to_string pp;;
  let to_yojson (S(lst)) =
    `List (List.map abstract_clause_to_yojson lst)
  ;;
  let name = "2ddpa";;
end;;
