(**
   A module defining data structures and basic operations to form a Plume graph.
*)

open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Odefa_ast;;

open Abstract_ast;;
open Ast;;
open Plume_context_model;;
open Pp_utils;;

module type Edge_sig = sig
  module C : Context_model;;
  type node =
    | Node of annotated_clause * (C.t);;
  type t =
    | Edge of node * node
  ;;
  (* type t;; *)
  val compare : t -> t -> int;;
  val pp : t pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.json;;
end;;
(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  module E : Edge_sig
  type t
  type edge = E.t
  type node


  val empty : t

  val add_edge : edge -> t -> t

  val edges_of : t -> edge Enum.t

  val has_edge : edge -> t -> bool

  val edges_from : node -> t -> edge Enum.t

  val edges_to : node -> t -> edge Enum.t

  val preds : node -> t -> node Enum.t

  val succs : node -> t -> node Enum.t

  val to_yojson : t -> Yojson.Safe.json

  val pp : t pretty_printer

  val show : t -> string

end;;

(* TODO: improve the performance of this implementation! *)
module Graph_impl (C : Context_model) : Graph_sig =
struct
  module C = C;;

  module E =
  struct
    module C = C;;
    type node =
      | Node of annotated_clause * (C.t)
    [@@deriving ord, show, to_yojson]
    ;;

    let _ = show_node;;

    type t =
      | Edge of node * node
    [@@deriving ord, show, to_yojson]
    ;;

  end;;

  open E;;

  type node = E.node;;

  type edge = E.t;;


  module Edge_set =
  struct
    module Impl = Set.Make(E);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(E);;
    include Yojson_utils.Set_to_yojson(Impl)(E);;
  end;;

  type t = Graph of Edge_set.t [@@deriving to_yojson];;

  let empty = Graph(Edge_set.empty);;

  let add_edge edge (Graph(s)) = Graph(Edge_set.add edge s);;

  let edges_of (Graph(s)) = Edge_set.enum s;;

  let has_edge edge (Graph(s)) = Edge_set.mem edge s;;

  let edges_from node (Graph(s)) =
    let Node(acl, c) = node in
    Edge_set.enum s
    |> Enum.filter (fun (Edge(n1, _)) ->
        let Node(acl', c') = n1 in
        let acl_check = equal_annotated_clause acl acl' in
        let c_check = C.equal c c' in
        acl_check && c_check
      )
  ;;

  let succs node g =
    edges_from node g |> Enum.map (fun (Edge(_,node)) -> node)
  ;;

  let edges_to node (Graph(s)) =
    let Node(acl, c) = node in
    Edge_set.enum s
    |> Enum.filter (fun (Edge(_, n1)) ->
        let Node(acl', c') = n1 in
        let acl_check = equal_annotated_clause acl acl' in
        let c_check = C.equal c c' in
        acl_check && c_check
      )
  ;;

  let preds node g =
    edges_to node g |> Enum.map (fun (Edge(node, _)) -> node)
  ;;

  let pp formatter g =
    (* pp_concat_sep_delim "{" "}" ", " pp_edge formatter @@ edges_of g *)
    pp_concat_sep_delim "{" "}" ", " (E.pp) formatter @@ edges_of g
  ;;

  let show = pp_to_string pp;;

end;;


let rec lift_expr (Expr(cls)) =
  Abs_expr(List.map lift_clause cls)

and lift_clause (Clause(x,b)) =
  Abs_clause(lift_var x, lift_clause_body b)

and lift_clause_body b =
  match b with
  | Value_body v -> Abs_value_body(lift_value v)
  | Var_body x -> Abs_var_body(lift_var x)
  | Appl_body(x,x') -> Abs_appl_body(lift_var x, lift_var x')
  | Conditional_body(x,p,f1,f2) ->
    Abs_conditional_body(lift_var x,p,lift_function_value f1,lift_function_value f2)
  | Projection_body(x,i) -> Abs_projection_body(lift_var x,i)
  | Deref_body(x) -> Abs_deref_body(lift_var x)
  | Update_body(x,x') -> Abs_update_body(lift_var x, lift_var x')
  | Binary_operation_body(x1,op,x2) ->
    Abs_binary_operation_body(lift_var x1, op, lift_var x2)
  | Unary_operation_body(op,x1) -> Abs_unary_operation_body(op, lift_var x1)

and lift_value v =
  match v with
  | Value_record r -> Abs_value_record (lift_record_value r)
  | Value_function f -> Abs_value_function(lift_function_value f)
  | Value_ref r -> Abs_value_ref (lift_ref_value r)
  | Value_int _ -> Abs_value_int
  | Value_bool b -> Abs_value_bool b
  | Value_string _ -> Abs_value_string

and lift_var (Var(i,_)) =
  Abs_var i

and lift_function_value (Function_value(x,e)) =
  Abs_function_value(lift_var x, lift_expr e)

and lift_ref_value (Ref_value x) =
  Abs_ref_value(lift_var x)

and lift_record_value (Record_value els) =
  Abs_record_value(Ident_map.map lift_var els)
;;
