open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_abstract_ast;;

open Abstract_ast;;
open Abstract_ast_utils;;
open Ast;;
open Plume_graph;;
open Plume_utils;;
open Nondeterminism;;
open Pds_reachability_types_stack;;

let logger = Logger_utils.make_logger "Plume_pds_dynamic_pop_handler";;
let lazy_logger = Logger_utils.make_lazy_logger "Plume_pds_dynamic_pop_handler";;

module Make
    (G : Graph_sig)
    (S : (module type of Plume_pds_structure_types.Make(G)) with module G = G)
    (T : (module type of Plume_pds_dynamic_pop_types.Make(G)(S))
     with module G = G
      and module S = S)
=
struct
  open S;;
  open T;;

  module Stack_element = Pds_continuation;;
  module State = Pds_state;;
  module Targeted_dynamic_pop_action = Pds_targeted_dynamic_pop_action;;
  module Untargeted_dynamic_pop_action = Pds_untargeted_dynamic_pop_action;;
  module Stack_action =
    Stack_action_constructor(Stack_element)(Targeted_dynamic_pop_action)
  ;;
  module Terminus =
    Terminus_constructor(State)(Untargeted_dynamic_pop_action)
  ;;
  open Stack_action.T;;
  open Terminus.T;;
  let perform_targeted_dynamic_pop element action =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () ->
         Printf.sprintf "perform_targeted_dynamic_pop (%s) (%s)"
           (Pds_continuation.show element)
           (show_pds_targeted_dynamic_pop_action action))
      (fun results ->
         String_utils.concat_sep_delim "[" "]" ", "
           (
             results
             |> Enum.clone
             |> Enum.map (String_utils.string_of_list Stack_action.show)
           )
      )
    @@ fun () ->
    Nondeterminism_monad.enum @@
    let open Nondeterminism_monad in
    match action with
    | Value_drop ->
      let%orzero Continuation_value _ = element in
      return []
    | Value_discovery_2_of_2 ->
      let%orzero Bottom_of_stack = element in
      return []
    | Variable_aliasing(x2,x1) ->
      let%orzero (Lookup_var(x',patsp,patsn)) = element in
      [%guard equal_abstract_var x' x2];
      (* We're looking for x2 and we've discovered here that it's aliased to
         x1. *)
      return [Push(Lookup_var(x1,patsp,patsn))]
    | Stateless_nonmatching_clause_skip_1_of_2 x'' ->
      let%orzero (Lookup_var(x,_,_)) = element in
      [%guard (not @@ equal_abstract_var x x'')];
      (* We're looking for a variable which does not match the one in this
         clause.  If we're stateless, that'll be fine. *)
      return [Pop_dynamic_targeted(
          Stateless_nonmatching_clause_skip_2_of_2 element)]
    | Stateless_nonmatching_clause_skip_2_of_2 element' ->
      begin
        match element with
        (* | Deref(_,_) ->
          (* This means we're in a stateful mode.  Stateless non-matching
             clause skip is inappropriate here. *)
          zero () *)
        | _ ->
          (* We're not in a stateful mode, so we can skip the clause.  We
             still have to put these elements back on the stack, though. *)
          return [Push(element);Push(element')]
      end
    | Value_capture_1_of_3 ->
      let%orzero (Continuation_value abs_filtered_value) = element in
      return [ Pop_dynamic_targeted(
          Value_capture_2_of_3(abs_filtered_value)) ]
    | Value_capture_2_of_3 fv ->
      let%orzero Capture(size) = element in
      return [ Pop_dynamic_targeted(Value_capture_3_of_3(fv,[],size)) ]
    | Value_capture_3_of_3(fv,collected_elements,size) ->
      let n = Bounded_capture_size.to_int size in
      if n > 1
      then
        begin
          let size' = Bounded_capture_size.of_int (n-1) in
          return
            [Pop_dynamic_targeted
               (Value_capture_3_of_3(fv,element::collected_elements,size'))]
        end
      else
        begin
          let pushes =
            List.map (fun x -> Push x) (element::collected_elements)
          in
          return @@ (Push (Continuation_value fv))::pushes
        end
    (* | Function_call_flow_validation(x2'',x3'',acl0,ctx0,c,ctxc,x) ->
      let%orzero (Lookup_var(x',_,_)) = element in
      [%guard (equal_abstract_var x x')];
      return [ Push(element)
             ; Push(Real_flow_huh)
             ; Push(Jump(acl0,ctx0))
             ; Push(Capture(Bounded_capture_size.of_int 2))
             ; Push(Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty))
             ; Push(Jump(c,ctxc))
             ; Push(Lookup_var(x3'',Pattern_set.empty,Pattern_set.empty))
             ]
    | Function_call_flow_validation_resolution_1_of_2(x,x') ->
      let%orzero Real_flow_huh = element in
      let action = Function_call_flow_validation_resolution_2_of_2(x,x') in
      return [ Pop_dynamic_targeted(action) ]
    | Function_call_flow_validation_resolution_2_of_2(x,x') ->
      let%orzero Continuation_value(v) = element in
      let Abs_filtered_value(v',_,_) = v in
      let%orzero
        Abs_value_function(Abs_function_value(_,Abs_expr(acls))) = v'
      in
      [%guard (equal_abstract_var x' @@ rv acls)];
      return [ Pop_dynamic_targeted(Variable_aliasing(x,x')) ] *)
    | Function_closure_lookup(x'',xf) ->
      let%orzero (Lookup_var(x,_,_)) = element in
      [%guard (not @@ equal_abstract_var x x'')];
      (* We're looking for a non-local variable.  Push a lookup for the
         function. *)
      return [ Push(element)
             ; Push(Lookup_var(xf,Pattern_set.empty,Pattern_set.empty))
             ]
(* applying filter if the var is part of conditional *)
    | Conditional_closure_lookup(x',x1,pat,positive_side) ->
      let%orzero (Lookup_var(x,patsp,patsn)) = element in
      if not @@ (equal_abstract_var x x' || equal_abstract_var x x1)
      then return [Push(element)]
      else
        let (patsp',patsn') =
          if positive_side
          then (Pattern_set.add pat patsp,patsn)
          else (patsp,Pattern_set.add pat patsn)
        in
        return [Push(Lookup_var(x1,patsp',patsn'))]
    (* | Conditional_subject_validation(x,x',x1,pat,then_branch,acl1,ctx) ->
      let%orzero (Lookup_var(x0,patsp,patsn)) = element in
      [%guard (equal_abstract_var x0 x)];
      let patsp',patsn' =
        if then_branch
        then (Pattern_set.singleton pat, Pattern_set.empty)
        else (Pattern_set.empty, Pattern_set.singleton pat)
      in
      return [ Push(Lookup_var(x',patsp,patsn))
             ; Push(Jump(acl1,ctx))
             ; Push(Lookup_var(x1,patsp',patsn'))
             ] *)
    | Record_projection_lookup(x,x',l) ->
      let%orzero (Lookup_var(x0,patsp,patsn)) = element in
      [%guard (equal_abstract_var x0 x)];
      return [ Push(Project(l,patsp,patsn))
             ; Push(Lookup_var(x',Pattern_set.empty,Pattern_set.empty))
             ]
    | Record_projection_1_of_2 ->
      let%orzero (Continuation_value fv) = element in
      let%orzero (Abs_filtered_value(Abs_value_record(r),patsp,patsn)) = fv in
      return [ Pop_dynamic_targeted(Record_projection_2_of_2(r,patsp,patsn)) ]
    | Record_projection_2_of_2(Abs_record_value(m) as r,patsp0,patsn0) ->
      let%orzero (Project(l,patsp1,patsn1)) = element in
      [%guard (Ident_map.mem l m)];
      if (not
            ((is_record_pattern_set patsp0) &&
             [Record_pattern Ident_map.empty; Any_pattern] |> List.for_all (fun pattern -> (not @@ Pattern_set.mem pattern patsn0))))
      then
        raise @@ Utils.Invariant_failure "Record projection received a value that doesn't satisfy to the record pattern. This might be an error in the record-value-filter-validation rule (7b at the time of this writing).";
      let%bind patsn2 = negative_pattern_set_selection r patsn0 in
      let x' = Ident_map.find l m in
      let patsp' = Pattern_set.union patsp1 @@
        pattern_set_projection patsp0 l in
      let patsn' = Pattern_set.union patsn1 @@
        pattern_set_projection patsn2 l in
      return @@ [ Push(Lookup_var(x',patsp',patsn')) ]
    | Immediate_filter_validation(x,pats_legal,v) ->
      let%orzero (Lookup_var(x0,patsp,patsn)) = element in
      [% guard (equal_abstract_var x x0) ];
      [% guard (Pattern_set.subset patsp pats_legal) ];
      [% guard (Pattern_set.is_empty @@ Pattern_set.inter patsn pats_legal) ];
      [% guard [Fun_pattern; Any_pattern] |> List.for_all (fun pattern -> (not @@ Pattern_set.mem pattern patsn)) ];
      let abs_filtered_value =
        Abs_filtered_value(v,Pattern_set.empty,Pattern_set.empty)
      in
      return [ Push(Continuation_value abs_filtered_value) ]
    | Record_filter_validation(x,r,n1) ->
      (* Make sure we're looking for this variable. *)
      let%orzero (Lookup_var(x0,patsp0,patsn0)) = element in
      [% guard (equal_abstract_var x x0) ];
      [% guard (is_record_pattern_set patsp0)];
      [% guard [Record_pattern Ident_map.empty; Any_pattern] |> List.for_all (fun pattern -> (not @@ Pattern_set.mem pattern patsn0)) ];
      let%bind patsn2 = negative_pattern_set_selection r patsn0 in
      let pattern_set_labels = labels_in_pattern_set patsp0 in
      let record_labels = labels_in_record r in
      let Abs_record_value(m) = r in
      [%guard (Ident_set.subset pattern_set_labels record_labels) ];
      let make_k'' l =
        let x'' = Ident_map.find l m in
        List.enum [ Push(Lookup_var( x''
                                   , pattern_set_projection patsp0 l
                                   , pattern_set_projection patsn2 l))
                  ; Push(Jump(n1))
                  ]
      in
      let first_pushes =
        List.enum [ Push(Continuation_value(Abs_filtered_value(
            Abs_value_record(r), patsp0, patsn2)))
                  ; Push(Jump(n1))
          ]
      in
      let all_pushes =
        record_labels
        |> Ident_set.enum
        |> Enum.map make_k''
        |> Enum.concat
        |> Enum.append first_pushes
      in
      return @@ List.of_enum all_pushes
    | Empty_record_value_discovery(x) ->
      let%orzero (Lookup_var(x0,patsp,patsn)) = element in
      [%guard (equal_abstract_var x x0)];
      let empty_record = Abs_value_record(Abs_record_value Ident_map.empty) in
      let empty_record_pattern = Record_pattern Ident_map.empty in
      [% guard (Pattern_set.subset patsp (Pattern_set.of_list [empty_record_pattern; Any_pattern])) ];
      [% guard [empty_record_pattern; Any_pattern] |> List.for_all (fun pattern -> (not @@ Pattern_set.mem pattern patsn)) ];
      return [ Push(Continuation_value(Abs_filtered_value(
          empty_record,Pattern_set.empty,Pattern_set.empty))) ]
    (* | Dereference_lookup(x,x') ->
      let%orzero (Lookup_var(x0,patsp,patsn)) = element in
      [% guard (equal_abstract_var x x0) ];
      return [ Push(Deref(patsp, patsn))
             ; Push(Lookup_var(x', Pattern_set.empty, Pattern_set.empty))
             ] *)
    (* | Cell_dereference_1_of_2 ->
      let%orzero
        (Continuation_value(Abs_filtered_value(
             Abs_value_ref(cell),patsp,patsn))) = element
      in
      [% guard (Pattern_set.is_empty patsp) ];
      [% guard (Pattern_set.is_empty patsn) ];
      (* From here, we need another stack frame element to confirm the
         dereference action and obtain its filters. *)
      return [ Pop_dynamic_targeted(Cell_dereference_2_of_2(cell)) ]
    | Cell_dereference_2_of_2(cell) ->
      let%orzero (Deref(patsp,patsn)) = element in
      let Abs_ref_value(x') = cell in
      return [ Push(Lookup_var(x', patsp, patsn)) ]
    | Cell_update_alias_analysis_init_1_of_2(x',source_state,target_state) ->
      let%orzero (Lookup_var(x,patsp,patsn)) = element in
      return [ Pop_dynamic_targeted(
          Cell_update_alias_analysis_init_2_of_2(
            x',source_state,target_state,x,patsp,patsn)) ]
    | Cell_update_alias_analysis_init_2_of_2(
        x',source_state,target_state,x,patsp0,patsn0) ->
      let%orzero (Deref _) = element in
      let%orzero Program_point_state(acl1,ctx1) = source_state in
      let%orzero Program_point_state(acl0,ctx0) = target_state in
      (* The lists below are in reverse order of their presentation in the
         formal rules because we are not directly modifying the stack;
         instead, we are pushing stack elements one at a time. *)
      let capture_size_5 = Bounded_capture_size.of_int 5 in
      let capture_size_2 = Bounded_capture_size.of_int 2 in
      let k1'' = [ Capture capture_size_5 ; Lookup_var(x,patsp0,patsn0) ] in
      let k2'' = [ Capture capture_size_2
                 ; Lookup_var(x',Pattern_set.empty,Pattern_set.empty)
                 ; Jump(acl1, ctx1) ] in
      let k3'' = [ Alias_huh ; Jump(acl0,ctx0) ] in
      let k0 = [ element ; Lookup_var(x,patsp0,patsn0) ] in
      return @@ List.map (fun x -> Push x) @@
      k0 @ k3'' @ k2'' @ k1''
    | Alias_analysis_resolution_1_of_5(x'') ->
      let%orzero Alias_huh = element in
      return [ Pop_dynamic_targeted
                 (Alias_analysis_resolution_2_of_5(x'')) ]
    | Alias_analysis_resolution_2_of_5(x'') ->
      let%orzero
        Continuation_value(Abs_filtered_value(v,patsp,patsn)) = element
      in
      [%guard (Pattern_set.is_empty patsp)];
      [%guard (Pattern_set.is_empty patsn)];
      return [ Pop_dynamic_targeted
                 (Alias_analysis_resolution_3_of_5(x'', v)) ]
    | Alias_analysis_resolution_3_of_5(x'',v) ->
      let%orzero
        Continuation_value(Abs_filtered_value(v',patsp,patsn)) = element
      in
      [%guard (Pattern_set.is_empty patsp)];
      [%guard (Pattern_set.is_empty patsn)];
      let equal_values = equal_abstract_value v v' in
      return [ Pop_dynamic_targeted
                 (Alias_analysis_resolution_4_of_5(x'',equal_values)) ]
    | Alias_analysis_resolution_4_of_5(x'',equal_values) ->
      let%orzero Lookup_var(x,patsp0,patsn0) = element in
      return [ Pop_dynamic_targeted
                 (Alias_analysis_resolution_5_of_5(
                     x'',equal_values,x,patsp0,patsn0)) ]
    | Alias_analysis_resolution_5_of_5(x'',equal_values,x,patsp0,patsn0) ->
      let%orzero Deref(patsp1,patsn1) = element in
      if equal_values
      then
        return [ Push(Lookup_var(x'',patsp1,patsn1)) ]
      else
        return [ Push(Deref(patsp1,patsn1))
               ; Push(Lookup_var(x,patsp0,patsn0)) ]
    | Nonsideeffecting_nonmatching_clause_skip(x'') ->
      let%orzero Lookup_var(x,_,_) = element in
      [%guard (not @@ equal_abstract_var x x'')];
      return [Push element]
    | Side_effect_search_start_function_flow_check_1_of_2(ctx,acl0,c,x0'') ->
      let%orzero Lookup_var(x,_,_) = element in
      [%guard (not @@ equal_abstract_var x x0'')];
      return [ Pop_dynamic_targeted(
          Side_effect_search_start_function_flow_check_2_of_2(ctx,acl0,c,element)
        )]
    | Side_effect_search_start_function_flow_check_2_of_2(ctx,acl0,c,xel) ->
      let%orzero Deref(_,_) = element in
      let%orzero Abs_clause(_,Abs_appl_body(x2'',x3'')) = c in
      let capture_size_2 = Bounded_capture_size.of_int 2 in
      return [ Push element
             ; Push xel
             ; Push Real_flow_huh
             ; Push (Jump(acl0,ctx))
             ; Push (Capture capture_size_2)
             ; Push (Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty))
             ; Push (Jump(Unannotated_clause(c),ctx))
             ; Push (Lookup_var(x3'',Pattern_set.empty,Pattern_set.empty))
             ]
    | Side_effect_search_start_function_flow_validated_1_of_4(
        acl0,ctx,x0'',x') ->
      let%orzero Real_flow_huh = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_start_function_flow_validated_2_of_4(
            acl0,ctx,x0'',x')
        )]
    | Side_effect_search_start_function_flow_validated_2_of_4(
        acl0,ctx,x0'',x') ->
      let%orzero Continuation_value fv = element in
      let Abs_filtered_value(v,_,_) = fv in
      let%orzero Abs_value_function(Abs_function_value(_,e)) = v in
      let Abs_expr cls = e in
      [%guard equal_abstract_var x' (rv cls)];
      return [ Pop_dynamic_targeted(
          Side_effect_search_start_function_flow_validated_3_of_4(acl0,ctx,x0'')
        )]
    | Side_effect_search_start_function_flow_validated_3_of_4(acl0,ctx,x0'') ->
      let%orzero Lookup_var(x,_,_)  = element in
      [%guard (not @@ equal_abstract_var x0'' x)];
      return [ Pop_dynamic_targeted(
          Side_effect_search_start_function_flow_validated_4_of_4(
            acl0,ctx,element
          )
        )]
    | Side_effect_search_start_function_flow_validated_4_of_4(acl0,ctx,xel) ->
      let%orzero Deref _ = element in
      let%orzero Lookup_var(x,patsp,patsn) = xel in
      return [ Push element
             ; Push xel
             ; Push Side_effect_search_start
             ; Push (Side_effect_lookup_var(x,patsp,patsn,acl0,ctx))
             ]
    | Side_effect_search_start_conditional_1_of_2(acl0,acl1,ctx) ->
      let%orzero Lookup_var(x,_,_) = element in
      let%orzero Exit_clause(x0'',_,_) = acl1 in
      [%guard (not @@ equal_abstract_var x0'' x)];
      return [ Pop_dynamic_targeted(
          Side_effect_search_start_conditional_2_of_2(acl0,acl1,ctx,element)) ]
    | Side_effect_search_start_conditional_2_of_2(acl0,acl1,ctx,xel) ->
      let%orzero Lookup_var(x,patsp0,patsn0) = xel in
      let%orzero Deref(_,_) = element in
      let%orzero Exit_clause(_,x',c) = acl1 in
      let%orzero Abs_clause(_,Abs_conditional_body(x2'',p,f1,f2)) = c in
      let Abs_function_value(_,Abs_expr f1cls) = f1 in
      let Abs_function_value(_,Abs_expr f2cls) = f2 in
      let%bind (patsp',patsn') =
        if equal_abstract_var x' (rv f1cls) then
          return (Pattern_set.singleton p, Pattern_set.empty)
        else if equal_abstract_var x' (rv f2cls) then
          return (Pattern_set.empty, Pattern_set.singleton p)
        else
          zero ()
      in
      return [ Push element
             ; Push xel
             ; Push Side_effect_search_start
             ; Push (Side_effect_lookup_var(x,patsp0,patsn0,acl0,ctx))
             ; Push (Jump(Unannotated_clause c, ctx))
             ; Push (Lookup_var(x2'',patsp',patsn'))
             ]
    | Side_effect_search_nonmatching_clause_skip ->
      let%orzero (Side_effect_lookup_var _) = element in
      return [ Push element ]
    | Side_effect_search_function_bottom_flow_check(acl1,acl0,ctx) ->
      let%orzero Side_effect_lookup_var _ = element in
      let%orzero Exit_clause(_,_,c) = acl1 in
      let%orzero Abs_clause(_,Abs_appl_body(x2'',x3'')) = c in
      let capture_size_2 = Bounded_capture_size.of_int 2 in
      return [ Push element
             ; Push Real_flow_huh
             ; Push (Jump(acl0,ctx))
             ; Push (Capture capture_size_2)
             ; Push (Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty))
             ; Push (Jump(Unannotated_clause(c),ctx))
             ; Push (Lookup_var(x3'',Pattern_set.empty,Pattern_set.empty))
             ]
    | Side_effect_search_function_bottom_flow_validated_1_of_3(x') ->
      let%orzero Real_flow_huh = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_function_bottom_flow_validated_2_of_3(x')) ]
    | Side_effect_search_function_bottom_flow_validated_2_of_3(x') ->
      let%orzero Continuation_value(Abs_filtered_value(v,_,_)) = element in
      let%orzero Abs_value_function(Abs_function_value(_,Abs_expr(cls))) = v in
      [%guard equal_abstract_var x' (rv cls)];
      return [ Pop_dynamic_targeted
                 Side_effect_search_function_bottom_flow_validated_3_of_3 ]
    | Side_effect_search_function_bottom_flow_validated_3_of_3 ->
      let%orzero Side_effect_lookup_var _ = element in
      return [ Push element
             ; Push element
             ]
    | Side_effect_search_conditional(acl1,ctx) ->
      let%orzero Side_effect_lookup_var _ = element in
      let%orzero Exit_clause(_,x',c) = acl1 in
      let%orzero Abs_clause(_,Abs_conditional_body(x1,p,f1,f2)) = c in
      let Abs_function_value(_,Abs_expr f1cls) = f1 in
      let Abs_function_value(_,Abs_expr f2cls) = f2 in
      let%bind (patsp',patsn') =
        if equal_abstract_var x' (rv f1cls) then
          return (Pattern_set.singleton p, Pattern_set.empty)
        else if equal_abstract_var x' (rv f2cls) then
          return (Pattern_set.empty, Pattern_set.singleton p)
        else
          zero ()
      in
      return [ Push element
             ; Push (Jump(acl1,ctx))
             ; Push (Lookup_var(x1,patsp',patsn'))
             ]
    | Side_effect_search_top ->
      let%orzero Side_effect_lookup_var _ = element in
      return []
    | Side_effect_search_complete_none_found ->
      let%orzero Side_effect_search_start = element in
      return []
    | Side_effect_search_alias_analysis_start(acl0,ctx,x') ->
      let%orzero Side_effect_lookup_var(x,patsp,patsn,acl',ctx') = element in
      let capture_size_2 = Bounded_capture_size.of_int 2 in
      let capture_size_5 = Bounded_capture_size.of_int 5 in
      return [ Push element
             ; Push Alias_huh
             ; Push (Jump(acl0,ctx))
             ; Push (Capture capture_size_2)
             ; Push (Lookup_var(x,patsp,patsn))
             ; Push (Jump(acl',ctx'))
             ; Push (Capture capture_size_5)
             ; Push (Lookup_var(x',Pattern_set.empty,Pattern_set.empty))
             ]
    | Side_effect_search_may_not_alias_1_of_4 ->
      let%orzero Alias_huh = element in
      return [ Pop_dynamic_targeted Side_effect_search_may_not_alias_2_of_4 ]
    | Side_effect_search_may_not_alias_2_of_4 ->
      let%orzero Continuation_value _ = element in
      return [ Pop_dynamic_targeted Side_effect_search_may_not_alias_3_of_4 ]
    | Side_effect_search_may_not_alias_3_of_4 ->
      let%orzero Continuation_value _ = element in
      return [ Pop_dynamic_targeted Side_effect_search_may_not_alias_4_of_4 ]
    | Side_effect_search_may_not_alias_4_of_4 ->
      let%orzero Side_effect_lookup_var _ = element in
      return [ Push element ]
    | Side_effect_search_may_alias_1_of_4 x' ->
      let%orzero Alias_huh = element in
      return [ Pop_dynamic_targeted (Side_effect_search_may_alias_2_of_4 x')]
    | Side_effect_search_may_alias_2_of_4 x' ->
      let%orzero Continuation_value(Abs_filtered_value(v,_,_)) = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_may_alias_3_of_4 (x',v))]
    | Side_effect_search_may_alias_3_of_4(x',v) ->
      let%orzero Continuation_value(Abs_filtered_value(v',_,_)) = element in
      [%guard equal_abstract_value v v'];
      return [ Pop_dynamic_targeted (Side_effect_search_may_alias_4_of_4 x')]
    | Side_effect_search_may_alias_4_of_4 x' ->
      let%orzero Side_effect_lookup_var _ = element in
      return [ Push (Side_effect_search_escape x') ]
    | Side_effect_search_escape_incremental_1_of_2 ->
      let%orzero Side_effect_search_escape _ = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_escape_incremental_2_of_2 element) ]
    | Side_effect_search_escape_incremental_2_of_2 element' ->
      let%orzero Side_effect_lookup_var _ = element in
      return [ Push element' ]
    | Side_effect_search_escape_base_1_of_4 ->
      let%orzero Side_effect_search_escape x' = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_escape_base_2_of_4(x')) ]
    | Side_effect_search_escape_base_2_of_4(x') ->
      let%orzero Side_effect_search_start = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_escape_base_3_of_4(x')) ]
    | Side_effect_search_escape_base_3_of_4(x') ->
      let%orzero Lookup_var _ = element in
      return [ Pop_dynamic_targeted(
          Side_effect_search_escape_base_4_of_4(x')) ]
    | Side_effect_search_escape_base_4_of_4(x') ->
      let%orzero Deref(patsp,patsn) = element in
      return [ Push (Lookup_var(x',patsp,patsn)) ] *)
    | Binary_operator_lookup_init(x1,x2,x3,n1,n0) ->
      let%orzero Lookup_var(x1',_,_) = element in
      [%guard (equal_abstract_var x1 x1') ];
      (* The lists below are in reverse order of their presentation in the
         formal rules because we are not directly modifying the stack;
         instead, we are pushing stack elements one at a time. *)
      let capture_size_5 = Bounded_capture_size.of_int 5 in
      let capture_size_2 = Bounded_capture_size.of_int 2 in
      let k1'' = [ Capture capture_size_5
                 ; Lookup_var(x2,Pattern_set.empty,Pattern_set.empty)
                 ] in
      let k2'' = [ Capture capture_size_2
                 ; Lookup_var(x3,Pattern_set.empty,Pattern_set.empty)
                 ; Jump(n1) ] in
      let k3'' = [ Binary_operation ; Jump(n0) ] in
      let k0 = [ element ] in
      return @@ List.map (fun x -> Push x) @@ k0 @ k3'' @ k2'' @ k1''
    (* | Unary_operator_lookup_init(x1,x2,n0) ->
      let%orzero Lookup_var(x1',_,_) = element in
      [%guard (equal_abstract_var x1 x1') ];
      (* The lists below are in reverse order of their presentation in the
         formal rules because we are not directly modifying the stack;
         instead, we are pushing stack elements one at a time. *)
      let capture_size_2 = Bounded_capture_size.of_int 2 in
      let k1'' = [ Capture capture_size_2
                 ; Lookup_var(x2,Pattern_set.empty,Pattern_set.empty)
                 ] in
      let k2'' = [ Unary_operation ; Jump(n0) ] in
      let k0 = [ element ] in
      return @@ List.map (fun x -> Push x) @@ k0 @ k2'' @ k1'' *)
    | Binary_operator_resolution_1_of_4(x1,op) ->
      let%orzero Binary_operation = element in
      return [ Pop_dynamic_targeted(
          Binary_operator_resolution_2_of_4(x1,op)) ]
    | Binary_operator_resolution_2_of_4(x1,op) ->
      (* TODO: Filter out invalid operands.  We did this before, but Zach
         removed it because (a) the logic was messy to embed here and (b) this
         all gets much easier to write once we have the assumption that all
         operators are monomorphic.  So for now, we'll just validate once we
         have the second operand.  This optimization isn't significant anyway.
      *)
      let%orzero
        Continuation_value(Abs_filtered_value(v2,patsp,patsn)) = element
      in
      [%guard (Pattern_set.is_empty patsp) ];
      [%guard (Pattern_set.is_empty patsn) ];
      return [ Pop_dynamic_targeted(
          Binary_operator_resolution_3_of_4(x1,op,v2)) ]
    | Binary_operator_resolution_3_of_4(x1,op,v2) ->
      let%orzero
        Continuation_value(Abs_filtered_value(v1,patsp,patsn)) = element
      in
      [%guard (Pattern_set.is_empty patsp) ];
      [%guard (Pattern_set.is_empty patsn) ];
      let%orzero Some result_values = abstract_binary_operation op v1 v2 in
      let%bind result_value = pick_enum result_values in
      return [ Pop_dynamic_targeted(
          Binary_operator_resolution_4_of_4(x1, result_value)) ]
    | Binary_operator_resolution_4_of_4(x1, result_value) ->
      let%orzero Lookup_var(x1',patsp,patsn) = element in
      [%guard (equal_abstract_var x1 x1') ];
      (* NOTE: For types that are not immediate (e.g. binary operations on
         records), we'll need a different handler for pattern matching.  It
         seems that our current theory for handling binary operators only works
         for operations that return immediately matchable types. *)
      let%orzero Some immediate_patterns = immediately_matched_by result_value in
      [%guard Pattern_set.subset patsp immediate_patterns];
      [%guard
        Pattern_set.is_empty @@ Pattern_set.inter immediate_patterns patsn ];
      return [ Push (Continuation_value(Abs_filtered_value(
          result_value, Pattern_set.empty, Pattern_set.empty))) ]
    (* | Unary_operator_resolution_1_of_3(x1,op) ->
      let%orzero Unary_operation = element in
      return [ Pop_dynamic_targeted(
          Unary_operator_resolution_2_of_3(x1,op)) ]
    | Unary_operator_resolution_2_of_3(x1,op) ->
      let%orzero
        Continuation_value(Abs_filtered_value(v,patsp,patsn)) = element
      in
      [%guard (Pattern_set.is_empty patsp) ];
      [%guard (Pattern_set.is_empty patsn) ];
      let%orzero Some result_values = abstract_unary_operation op v in
      let%bind result_value = pick_enum result_values in
      return [ Pop_dynamic_targeted(
          Unary_operator_resolution_3_of_3(x1, result_value)) ]
    | Unary_operator_resolution_3_of_3(x1, result_value) ->
      let%orzero Lookup_var(x1',patsp,patsn) = element in
      [%guard (equal_abstract_var x1 x1') ];
      (* NOTE: For types that are not immediate (e.g. unary operations on
         records), we'll need a different handler for pattern matching.  It
         seems that our current theory for handling unary operators only works
         for operations that return immediately matchable types. *)
      let%orzero Some immediate_patterns = immediately_matched_by result_value in
      [%guard Pattern_set.subset patsp immediate_patterns];
      [%guard
        Pattern_set.is_empty @@ Pattern_set.inter immediate_patterns patsn ];
      return [ Push (Continuation_value(Abs_filtered_value(
          result_value, Pattern_set.empty, Pattern_set.empty))) ] *)
  ;;

  let perform_untargeted_dynamic_pop element action =
    Nondeterminism_monad.enum @@
    let open Nondeterminism_monad in
    match action with
    | Do_jump ->
      let%orzero (Jump(n1)) = element in
      return ([], Static_terminus(Program_point_state(n1)))
    | Value_discovery_1_of_2 ->
      let%orzero (Continuation_value abs_filtered_value) = element in
      return ( [ Pop_dynamic_targeted(Value_discovery_2_of_2) ]
             , Static_terminus(Result_state abs_filtered_value)
             )
  ;;
end;;
