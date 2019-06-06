open Batteries;;

open Odefa_ast;;
open Odefa_ddpa;;
open Toploop_types;;

open Ast;;
open Ddpa_abstract_ast;;

module type Stack = Ddpa_context_stack.Context_stack;;

let name_parsing_functions =
  [
    (* TODO: After implementing PLUME, include PLUME *)
    (* A function for the literally-named modules. *)
    (fun name ->
       match name with
       | "0ddpa" ->
         DDPA (module Ddpa_unit_stack.Stack : Stack)
       | "1ddpa" ->
         DDPA (module Ddpa_single_element_stack.Stack : Stack)
       | "2ddpa" ->
         DDPA (module Ddpa_two_element_stack.Stack : Stack)
       | "ddpaNR" ->
         DDPA (module Ddpa_nonrepeating_stack.Stack : Stack)
       (* | "none" -> None *)
       (* | "splume" ->
          Plume (module Plume_context_model.Context_model)
          | "osplume" ->
          Plume (module Plume_context_model.Context_model) *)
       | _ -> raise Not_found
    )
    ;
    (* A function for parsing kddpa *)
    (fun name ->
       if not @@ String.ends_with name "ddpa" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 4 in
       try
         let num = int_of_string num_str in
         let module Spec : Ddpa_n_element_stack.Spec =
         struct
           let size = num
         end
         in
         let module NStack = Ddpa_n_element_stack.Make(Spec) in
         DDPA (module NStack : Stack)
       with
       | Failure _ -> raise Not_found
    );
    (* TODO: Parising kplume *)
    (* (fun name ->
       if not @@ String.ends_with name "plume" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 4 in
       try
         let num = int_of_string num_str in
         let module Spec : Ddpa_n_element_stack.Spec =
         struct
           let size = num
         end
         in
         let module NStack = Ddpa_n_element_stack.Make(Spec) in
         DDPA (module NStack : Stack)
       with
       | Failure _ -> raise Not_found
    ) *)
  ];;

let analysis_from_name name =
  let rec loop fns =
    match fns with
    | [] -> raise Not_found
    | fn::fns' ->
      begin
        try
          fn name
        with
        | Not_found -> loop fns'
      end
  in
  loop name_parsing_functions
;;
(*
let stack_from_name name =
  let rec loop fns =
    match fns with
    | [] -> raise Not_found
    | fn::fns' ->
      begin
        try
          fn name
        with
        | Not_found -> loop fns'
      end
  in
  loop name_parsing_functions
;; *)

(** Iterate recursively over all clauses in an expression. *)
let rec iterate_abstract_clauses (Abs_expr(acls)) =
  let top_level = List.enum acls in
  let nested = Enum.delay
      (fun () -> Enum.concat @@
        Enum.map (fun e -> Enum.delay (fun () -> iterate_abstract_clauses e)) @@
        Enum.concat @@ List.enum @@ List.map _abs_exprs_of_clause acls)
  in
  Enum.append top_level nested

and _abs_exprs_of_clause (Abs_clause(_,b)) =
  match b with
  | Abs_conditional_body(_,_,Abs_function_value(_,e1),Abs_function_value(_,e2))
    -> Enum.append (Enum.singleton e1) (Enum.singleton e2)
  | Abs_value_body(v) ->
    _abs_exprs_of_value v
  | Abs_var_body _ | Abs_appl_body _ | Abs_projection_body _ | Abs_deref_body _
  | Abs_update_body _ | Abs_binary_operation_body _
  | Abs_unary_operation_body _ -> Enum.empty ()

and _abs_exprs_of_value v =
  match v with
  | Abs_value_function(Abs_function_value(_,e)) -> Enum.singleton e
  | Abs_value_record _ | Abs_value_ref _ | Abs_value_int | Abs_value_bool _
  | Abs_value_string -> Enum.empty ()
;;

let last_var_of (Expr(cls)) =
  let Clause(x,_) = List.last cls in x
;;
