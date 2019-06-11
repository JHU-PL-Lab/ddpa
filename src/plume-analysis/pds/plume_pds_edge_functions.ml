open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;

open Abstract_ast;;
open Plume_context_model;;
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
      and type Targeted_dynamic_pop_action.t = T.pds_targeted_dynamic_pop_action)
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
      (edge : plume_edge) (state : R.State.t)
    : (R.Stack_action.t list * R.Terminus.t) Enum.t =
    (* Unpack the edge *)
    let Edge(n1, n0) = edge in
    (* Generate PDS edge functions for this Plume edge *)
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () -> Printf.sprintf "Plume %s edge function at state %s"
          (show_edge edge) (Pds_state.show state))
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
    (* TODO: There should be a way to associate each edge function with
             its corresponding acl0 rather than using this guard. *)
    [%guard (compare_node n0 n0' == 0) ];
    let open Option.Monad in
    let zero () = None in
    (* TODO: It'd be nice if we had a terser way to represent stack
             processing operations (those that simply reorder the stack
             without transitioning to a different node). *)
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
              Node(Unannotated_clause(Abs_clause(x, Abs_var_body x')), ctx) = n1
            in
            (* x = x' *)
            return (Variable_aliasing(x,x'),Program_point_state(n1))
          end
          ;
          (* Clause Skip *)
          begin
            let%orzero Node(Unannotated_clause(Abs_clause(x,_)), ctx) = n1 in
            (* x' = b *)
            return ( Stateless_nonmatching_clause_skip_1_of_2 x
                   , Program_point_state(n1)
                   )
          end
          ;
          (* Block Marker Skip *)
          (* This is handled below as a special case because it does not involve
             a pop. *)
          (* ********** Navigation ********** *)
          (* Capture *)
          begin
            return ( Value_capture_1_of_3
                   , Program_point_state(n0)
                   )
          end
          ;
          (* Rewind *)
          (* This is handled below in untargeted dynamic pops. *)
          (* ********** Function Wiring ********** *)
          (* Function Top: Parameter Variable *)
          begin
            let%orzero Node(Enter_clause(x,x',c), ctx) = n1 in
            let%orzero (Abs_clause(_,Abs_appl_body (_,x3''))) = c in
            begin
              if not (equal_abstract_var x' x3'') then
                raise @@ Utils.Invariant_failure "Ill-formed wiring node."
              else
                ()
            end;
            return (Variable_aliasing(x,x'),Program_point_state(n1))
          end
          ;
          (* NOTE: if provided clause is gone, this is gone *)
          (* Function Bottom: Flow Check *)
          (* begin
            let%orzero (Exit_clause(x,_,c)) = acl1 in
            let%orzero (Abs_clause(x1'',Abs_appl_body(x2'',x3''))) = c in
            begin
              if not (equal_abstract_var x x1'') then
                raise @@ Utils.Invariant_failure "Ill-formed wiring node."
              else
                ()
            end;
            (* x =(up)c _ (for functions) *)
            return ( Function_call_flow_validation(x2'',x3'',acl0,ctx,Unannotated_clause(c),ctx,x)
                   , Program_point_state(Unannotated_clause(c),ctx)
                   )
          end
          ; *)
          (* Function Bottom: Return Variable *)
          begin
            let%orzero Node(Exit_clause(x,x',c), ctx) = n1 in
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
            let%orzero Node(Enter_clause(x'',x',c), ctx) = n1 in
            let%orzero (Abs_clause(_,Abs_appl_body(x2'',x3''))) = c in
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
            let%orzero Node(Enter_clause(x',x1,c), ctx) = n1 in
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
          (* FIXME: VERY confused!!! *)
          begin
            let%orzero Node(Exit_clause(x,x',c), ctx) = n1 in
            let%orzero
              (Abs_clause(x2,Abs_conditional_body(x1,pat,f1,_))) = c
            in
            begin
              if not (equal_abstract_var x x2) then
                raise @@ Utils.Invariant_failure "Ill-formed wiring node."
              else
                ()
            end;
            (* x =(up) x' for conditionals *)
            let Abs_function_value(_,Abs_expr(cls)) = f1 in
            let f1ret = rv cls in
            (* whether or not we're in the then branch *)
            let then_branch : bool = equal_abstract_var f1ret x' in
            return ( Conditional_subject_validation(
                x,x',x1,pat,then_branch,acl1,ctx)
                   , Program_point_state(Unannotated_clause(c),ctx)
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
              (Unannotated_clause(
                  Abs_clause(x,Abs_value_body(Abs_value_record(r))))) = acl1
            in
            (* x = r *)
            let target_state = Program_point_state(n1) in
            return ( Record_filter_validation(x,r,acl1,ctx), target_state )
          end
          ;
          (* ********** State ********** *)
          (* Update Is Empty Record *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(x, Abs_update_body _))) = acl1
            in
            (* x = x' <- x'' -- produce {} for x *)
            return ( Empty_record_value_discovery x
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Dereference Start *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(x, Abs_deref_body(x')))) = acl1
            in
            (* x = !x' *)
            return ( Dereference_lookup(x,x')
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Dereference Stop *)
          begin
            return ( Cell_dereference_1_of_2
                   , Program_point_state(acl0, ctx) )
          end
          ;
          (* ********** Alias Analysis (State) ********** *)
          (* Alias Analysis Start *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(
                   _, Abs_update_body(x',_)))) = acl1
            in
            (* x''' = x' <- x'' *)
            let source_state = Program_point_state(acl1,ctx) in
            let target_state = Program_point_state(acl0,ctx) in
            return ( Cell_update_alias_analysis_init_1_of_2(
                x',source_state,target_state)
                   , Program_point_state(acl0, ctx) )
          end
          ;
          (* Alias Analysis Stop *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(
                   _, Abs_update_body(_,x'')))) = acl1
            in
            (* x''' = x' <- x'' *)
            return ( Alias_analysis_resolution_1_of_5(x'')
                   , Program_point_state(acl1, ctx) )
          end
          ;
          (* ********** Side Effect Search (State) ********** *)
          (* Stateful Immediate Clause Skip *)
          begin
            let%orzero (Unannotated_clause(Abs_clause(x,b))) = acl1 in
            [% guard (is_immediate acl1) ];
            [% guard (b |>
                      (function
                        | Abs_update_body _ -> false
                        | _ -> true)) ];
            (* x' = b *)
            return ( Nonsideeffecting_nonmatching_clause_skip x
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Side Effect Search Start: Function Flow Check *)
          begin
            let%orzero (Exit_clause(x0'',_,c)) = acl1 in
            let%orzero (Abs_clause(_,Abs_appl_body(_,_))) = c in
            return ( Side_effect_search_start_function_flow_check_1_of_2(
                ctx,acl0,c,x0'')
                   , Program_point_state(Unannotated_clause(c),ctx)
              )
          end
          ;
          (* Side Effect Search Start: Function Flow Validated *)
          begin
            let%orzero (Exit_clause(x0'',x',c)) = acl1 in
            let%orzero (Abs_clause(_,Abs_appl_body(_,_))) = c in
            return ( Side_effect_search_start_function_flow_validated_1_of_4(
                acl0,ctx,x0'',x'
              )
                   , Program_point_state(acl1,C.push c ctx)
              )
          end
          ;
          (* Side Effect Search Start: Conditional Positive *)
          (* Side Effect Search Start: Conditional Negative *)
          begin
            let%orzero (Exit_clause(_,_,c)) = acl1 in
            let%orzero (Abs_clause(_,Abs_conditional_body _)) = c in
            return ( Side_effect_search_start_conditional_1_of_2(acl0,acl1,ctx)
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Side Effect Search Immediate Clause Skip *)
          begin
            let%orzero (Unannotated_clause(Abs_clause(_,b))) = acl1 in
            [% guard (is_immediate acl1) ];
            [% guard (b |>
                      (function
                        | Abs_update_body _ -> false
                        | _ -> true)) ];
            (* x' = b *)
            return ( Side_effect_search_nonmatching_clause_skip
                   , Program_point_state(acl1,ctx) )
          end
          ;
          (* Side Effect Search: Function Bottom: Flow Check *)
          begin
            let%orzero Exit_clause(_,_,c) = acl1 in
            let%orzero Abs_clause(_,Abs_appl_body _) = c in
            return ( Side_effect_search_function_bottom_flow_check(
                acl1,acl0,ctx)
                   , Program_point_state(Unannotated_clause(c),ctx)
              )
          end
          ;
          (* Side Effect Search: Function Bottom: Flow Validated *)
          begin
            let%orzero Exit_clause(_,x',c) = acl1 in
            let%orzero Abs_clause(_,Abs_appl_body _) = c in
            return ( Side_effect_search_function_bottom_flow_validated_1_of_3(
                x')
                   , Program_point_state(Unannotated_clause(c),C.push c ctx)
              )
          end
          ;
          (* Side Effect Search: Conditional Positive *)
          (* Side Effect Search: Conditional Negative *)
          begin
            let%orzero Exit_clause(_,_,c) = acl1 in
            let%orzero Abs_clause(_,Abs_conditional_body _) = c in
            return ( Side_effect_search_conditional(acl1,ctx)
                   , Program_point_state(Unannotated_clause(c),ctx)
                   )
          end
          ;
          (* Side Effect Search: Top *)
          begin
            let%orzero Enter_clause(_,_,c) = acl1 in
            let%bind ctx' =
              match c with
              | Abs_clause(_,Abs_appl_body _) -> return @@ C.pop ctx
              | Abs_clause(_,Abs_conditional_body _) -> return ctx
              | _ -> zero ()
            in
            return ( Side_effect_search_top
                   , Program_point_state(acl1,ctx')
                   )
          end
          ;
          (* Side Effect Search: Complete, None Found *)
          begin
            return ( Side_effect_search_complete_none_found
                   , Program_point_state(acl0, ctx)
                   )
          end
          ;
          (* Side Effect Search: Alias Analysis Start *)
          begin
            let%orzero
              Unannotated_clause(Abs_clause(_,Abs_update_body(x',_))) = acl1
            in
            return ( Side_effect_search_alias_analysis_start(acl0,ctx,x')
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Side Effect Search: May Not Alias *)
          begin
            let%orzero
              Unannotated_clause(Abs_clause(_,Abs_update_body(_,_))) = acl1
            in
            return ( Side_effect_search_may_not_alias_1_of_4
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Side Effect Search: May Alias *)
          begin
            let%orzero
              Unannotated_clause(Abs_clause(_,Abs_update_body(_,x''))) = acl1
            in
            return ( Side_effect_search_may_alias_1_of_4 x''
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Side Effect Search: Escape: Incremental *)
          begin
            return ( Side_effect_search_escape_incremental_1_of_2
                   , Program_point_state(acl0,ctx)
                   )
          end
          ;
          (* Side Effect Search Escape: Base *)
          begin
            return ( Side_effect_search_escape_base_1_of_4
                   , Program_point_state(acl0,ctx)
                   )
          end
          ;
          (* ********** Operations ********** *)
          (* Binary Operation Start *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(x1,
                                             Abs_binary_operation_body(x2,_,x3)))) = acl1
            in
            (* x1 = x2 op x3 *)
            return ( Binary_operator_lookup_init(
                x1,x2,x3,acl1,ctx,acl0,ctx)
                   , Program_point_state(acl1,ctx)
              )
          end
          ;
          (* Binary Operation Evaluation *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(x1,
                                             Abs_binary_operation_body(_,op,_)))) = acl1
            in
            (* x1 = x2 op x3 *)
            return ( Binary_operator_resolution_1_of_4(x1,op)
                   , Program_point_state(acl1,ctx)
                   )
          end
          ;
          (* Unary Operation Start *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(x1,
                                             Abs_unary_operation_body(_,x2)))) = acl1
            in
            (* x1 = op x2 *)
            return ( Unary_operator_lookup_init(
                x1,x2,acl0,ctx)
                   , Program_point_state(acl1,ctx)
              )
          end
          ;
          (* Unary Operation Evaluation *)
          begin
            let%orzero
              (Unannotated_clause(Abs_clause(x1,
                                             Abs_unary_operation_body(op,_)))) = acl1
            in
            (* x1 = op x2 *)
            return ( Unary_operator_resolution_1_of_3(x1,op)
                   , Program_point_state(acl1,ctx)
                   )
          end
        ]
    in
    let nop_states =
      match acl1 with
      | Start_clause _ | End_clause _ ->
        Enum.singleton @@ Program_point_state(acl1,ctx)
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
  ;;

  let create_untargeted_dynamic_pop_action_function
      (edge : plume_edge) (state : R.State.t) =
    let Plume_edge(_, acl0) = edge in
    let zero = Enum.empty in
    let%orzero (Program_point_state(acl0',_)) = state in
    (* TODO: There should be a way to associate each action function with
             its corresponding acl0 rather than using this guard. *)
    [%guard (compare_annotated_clause acl0 acl0' == 0)];
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
