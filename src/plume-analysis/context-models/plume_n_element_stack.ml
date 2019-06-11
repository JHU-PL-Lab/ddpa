open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Abstract_ast;;
open Plume_context_model;;
open Pp_utils;;

type 'a dq = 'a Deque.dq;;

module type Spec =
sig
  val size : int
end;;

module Make(S : Spec) : Context_model =
struct
  type t = abstract_clause dq;;
  let equal x y = compare x y == 0;;
  let compare x y = Enum.compare compare (Deque.enum x) (Deque.enum y);;
  let empty = Deque.empty;;
  let push c x =
    let x' = Deque.cons c x in
    if Deque.size x' > S.size
    then fst @@ Option.get @@ Deque.rear x'
    else x'
  ;;
  let pop x =
    match Deque.front x with
    | None -> Deque.empty
    | Some(_,x') -> x'
  ;;
  let is_top c x =
    match Deque.front x with
    | None -> true
    | Some(c',_) -> c = c'
  ;;
  let pp formatter x =
    pp_concat_sep_delim "" "|?" "|" pp_var_of_abstract_clause formatter @@
    Deque.enum x
  ;;
  let show = pp_to_string pp;;
  let to_yojson c =
    `List (List.map abstract_clause_to_yojson @@ Deque.to_list c)
  ;;
  let name = string_of_int S.size ^ "plume";;
end;;
