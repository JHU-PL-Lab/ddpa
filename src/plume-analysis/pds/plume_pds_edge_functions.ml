open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;

open Abstract_ast;;
open Plume_graph;;
open Plume_utils;;

let logger = Logger_utils.make_logger "Plume_pds_edge_functions";;
let lazy_logger = Logger_utils.make_lazy_logger "Plume_pds_edge_functions";;

module Make
    (G : Graph_sig)
    (S : (module type of Plume_pds_structure_types.Make(G)) with module G = G)
    (T : (module type of Plume_pds_dynamic_pop_types.Make(G)(S))
     with module G = G
      and module S = S)
    (B : Pds_reachability_basis.Basis)
    (R : Pds_reachability_analysis.Analysis
     with type State.t = S.Pds_state.t
      and type Targeted_dynamic_pop_action.t =
            T.pds_targeted_dynamic_pop_action)
=
struct
  open G;;
  open S;;
  open T;;
  open R.Stack_action.T;;
  open R.Terminus.T;;

  (**
     Creates a PDS edge function for a particular Plume graph edge.  The
     resulting function produces transitions for PDS states, essentially serving
     as the first step toward implementing each Plume rule.  The remaining steps
     are addressed by the dynamic pop handler, which performs the closure of the
     dynamic pops generated by this function.
  *)
  let create_edge_function
      (edge : edge) (state : R.State.t)
    : (R.Stack_action.t list * R.Terminus.t) Enum.t =
    (* Unpack the edge *)
    let Edge(n1, n0) = edge in
    (* Generate PDS edge functions for this Plume edge *)
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () -> Printf.sprintf "Plume %s edge function at state %s"
          (G.E.show edge) (Pds_state.show state))
      (fun edges ->
         let string_of_output (actions,target) =
           String_utils.string_of_tuple
             (String_utils.string_of_list R.Stack_action.show)
             R.Terminus.show
             (actions,target)
         in
         Printf.sprintf "Generates edges: %s"
           (String_utils.string_of_list string_of_output @@
            List.of_enum @@ Enum.clone edges)) @@
    fun () ->
    let zero = Enum.empty in
    let%orzero Program_point_state(n0') = state in
    (* NOTE: Strictly speaking, this guard should not be necessary; edge
       functions are only added with classifications which will ensure that
       this guard always holds. *)
    [%guard (compare_node n0 n0' == 0) ];
    let open Option.Monad in
    let zero () = None in
    (* TODO: It'd be nice if we had a terser way to represent stack
             processing operations (those that simply reorder the stack
             without transitioning to a different node). *)
    begin
      let targeted_dynamic_pops = Enum.filter_map identity @@ List.enum
          [
            (* ********** Variable Discovery ********** *)
            (* Intermediate Value *)
            begin
              return (Value_drop, Program_point_state(n0))
            end
            ;
            (* ********** Variable Search ********** *)
            (* Value Alias *)
            begin
              let%orzero
                Node(Unannotated_clause(Abs_clause(x, Abs_var_body x')), _) = n1
              in
              (* x = x' *)
              return (Variable_aliasing(x,x'),Program_point_state(n1))
            end
            ;
            (* Clause Skip *)
            begin
              let%orzero Node(Unannotated_clause(Abs_clause(x,_)), _) = n1 in
              (* x' = b *)
              return ( Stateless_nonmatching_clause_skip_1_of_2 x
                     , Program_point_state(n1)
                     )
            end
            ;
            (* Block Marker Skip *)
            (* This is handled below as a special case because it does not
               involve a pop. *)
            (* ********** Navigation ********** *)
            (* Capture *)
            begin
              return ( Value_capture_1_of_3
                     , Program_point_state(n0)
                     )
            end
            ;
            (* ********** Function Wiring ********** *)
            (* Function Top: Parameter Variable *)
            begin
              let%orzero Node(Enter_clause(x,x',c), _) = n1 in
              let%orzero (Abs_clause(_,Abs_appl_body (_,x3'',_))) = c in
              (* NOTE: ignoring call site annotations as none apply to Plume
                 lookup. *)
              begin
                if not (equal_abstract_var x' x3'') then
                  raise @@ Utils.Invariant_failure "Ill-formed wiring node."
                else
                  ()
              end;
              return (Variable_aliasing(x,x'),Program_point_state(n1))
            end
            ;
            (* Function Bottom: Return Variable *)
            begin
              let%orzero Node(Exit_clause(x,x',c), _) = n1 in
              let%orzero (Abs_clause(x1'',Abs_appl_body _)) = c in
              begin
                if not (equal_abstract_var x x1'') then
                  raise @@ Utils.Invariant_failure "Ill-formed wiring node."
                else
                  ()
              end;
              return ( Variable_aliasing(x,x')
                     , Program_point_state(n1)
                     )
            end
            ;
            (* Function Top: Non-Local Variable *)
            begin
              let%orzero Node(Enter_clause(x'',x',c), _) = n1 in
              let%orzero (Abs_clause(_,Abs_appl_body(x2'',x3'',_))) = c in
              (* NOTE: ignoring call site annotations as none apply to Plume
                 lookup. *)
              begin
                if not (equal_abstract_var x' x3'') then
                  raise @@ Utils.Invariant_failure "Ill-formed wiring node."
                else
                  ()
              end;
              (* NOTE: check if Function_closure_lookup is truly what we want
                 to use here *)
              return ( Function_closure_lookup(x'',x2'')
                     , Program_point_state(n1)
                     )
            end
            ;
            (* ********** Conditional Wiring ********** *)
            (* Conditional Top: Subject Positive
               Conditional Top: Subject Negative
               Conditional Top: Non-Subject Variable *)
            begin
              (* This block represents *all* conditional closure handling on
                 the entering side. *)
              let%orzero Node(Enter_clause(x',x1,c), _) = n1 in
              let%orzero
                (Abs_clause(_,Abs_conditional_body(x1',p,f1,_))) = c
              in
              begin
                if not (equal_abstract_var x1 x1') then
                  raise @@ Utils.Invariant_failure "Ill-formed wiring node."
                else
                  ()
              end;
              let Abs_function_value(f1x,_) = f1 in
              (* x'' =(down)c x' for conditionals *)
              (* checking if var was var in the wiring node *)
              let closure_for_positive_path = equal_abstract_var f1x x' in
              return ( Conditional_closure_lookup
                         (x',x1,p,closure_for_positive_path)
                     , Program_point_state(n1)
                     )
            end
            ;
            (* Conditional Bottom: Return Positive
               Conditional Bottom: Return Negative *)
            begin
              let%orzero Node(Exit_clause(x,x',c), _) = n1 in
              let%orzero
                (Abs_clause(x2,Abs_conditional_body(_,_,_,_))) = c
              in
              begin
                if not (equal_abstract_var x x2) then
                  raise @@ Utils.Invariant_failure "Ill-formed wiring node."
                else
                  ()
              end;
              return ( Variable_aliasing(x, x')
                     , Program_point_state(n1)
                     )
            end
            ;
            (* ********** Record Construction/Destruction ********** *)
            (* Record Projection Start *)
            begin
              let%orzero
                Node(Unannotated_clause(
                    Abs_clause(x,Abs_projection_body(x',l))), _) = n1
              in
              (* x = x'.l *)
              return ( Record_projection_lookup(x,x',l)
                     , Program_point_state(n1)
                     )
            end
            ;
            (* Record Projection Stop *)
            begin
              return ( Record_projection_1_of_2
                     , Program_point_state(n0)
                     )
            end
            ;
            (* ********** Filter Validation ********** *)
            (* Filter Immediate *)
            begin
              let%orzero
                Node(Unannotated_clause(Abs_clause(x,Abs_value_body v)), _) = n1
              in
              (* x = v *)
              let%orzero (Some immediate_patterns) = immediately_matched_by v in
              return ( Immediate_filter_validation(x, immediate_patterns, v)
                     , Program_point_state(n1)
                     )
            end
            ;
            (* Filter Record *)
            begin
              let%orzero
                Node(Unannotated_clause(
                    Abs_clause(x,Abs_value_body(Abs_value_record(r)))), _) = n1
              in
              (* x = r *)
              let target_state = Program_point_state(n1) in
              return ( Record_filter_validation(x,r,n1), target_state )
            end
            ;

            (* ********** Operations ********** *)
            (* Binary Operation Start *)
            begin
              let%orzero
                Node(Unannotated_clause(Abs_clause(
                    x1, Abs_binary_operation_body(x2,_,x3))), _) = n1
              in
              (* x1 = x2 op x3 *)
              return ( Binary_operator_lookup_init(
                  x1,x2,x3,n1,n0)
                     , Program_point_state(n1)
                )
            end
            ;
            (* Binary Operation Evaluation *)
            begin
              let%orzero
                Node(Unannotated_clause(Abs_clause(
                    x1, Abs_binary_operation_body(_,op,_))), _) = n1
              in
              (* x1 = x2 op x3 *)
              return ( Binary_operator_resolution_1_of_4(x1,op)
                     , Program_point_state(n1)
                     )
            end
            ;
            (* Unary Operation Start *)
            begin
              let%orzero
                Node(Unannotated_clause(Abs_clause(
                    x1, Abs_unary_operation_body(_,x2))), _) = n1
              in
              (* x1 = op x2 *)
              return ( Unary_operator_lookup_init(
                  x1,x2,n0)
                     , Program_point_state(n1)
                )
            end
            ;
            (* Unary Operation Evaluation *)
            begin
              let%orzero
                Node(Unannotated_clause(Abs_clause(
                    x1, Abs_unary_operation_body(op,_))), _) = n1
              in
              (* x1 = op x2 *)
              return ( Unary_operator_resolution_1_of_3(x1,op)
                     , Program_point_state(n1)
                     )
            end
          ]
      in
      let nop_states =
        let Node(acl1, _) = n1 in
        match acl1 with
        | Start_clause _ | End_clause _ ->
          Enum.singleton @@ Program_point_state(n1)
        | _ -> Enum.empty ()
      in
      Enum.append
        (targeted_dynamic_pops
         |> Enum.map
           (fun (action,state) ->
              ([Pop_dynamic_targeted(action)], Static_terminus state)))
        (nop_states
         |> Enum.map
           (fun state -> ([], Static_terminus state)))
    end [@landmark "*edge_computation"]
  ;;

  let create_untargeted_dynamic_pop_action_function
      (edge : edge) (state : R.State.t) =
    let Edge(_, n0) = edge in
    let zero = Enum.empty in
    let%orzero (Program_point_state n0') = state in
    (* TODO: There should be a way to associate each action function with
             its corresponding acl0 rather than using this guard. *)
    [%guard (compare_node n0 n0' == 0)];
    let open Option.Monad in
    let untargeted_dynamic_pops = Enum.filter_map identity @@ List.enum
        [
          (* Value discovery. *)
          begin
            return @@ Value_discovery_1_of_2
          end
          ;
          (* Jump. *)
          begin
            return @@ Do_jump
          end
          ;
        ]
    in
    untargeted_dynamic_pops
  ;;

end;;
