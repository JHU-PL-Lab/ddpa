open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ddpa_analysis;;
open Ddpa_analysis_logging;;
open Toploop_analysis_wrapper_types;;

module Make(A : Analysis_sig) : Analysis_wrapper
  with module C = A.C
   and type logging_config = ddpa_analysis_logging_config
  =
struct
  type analysis =
    { aref : A.ddpa_analysis ref
    ; expression : expr
    };;

  module C = A.C;;
  type logging_config = ddpa_analysis_logging_config;;

  let create_analysis ?logging_config:(logging_config=None) expr =
    let a =
      A.create_initial_analysis
        ~ddpa_logging_config:logging_config
        expr
    in
    { aref = ref @@ A.perform_full_closure a
    ; expression = expr
    }
  ;;

  let values_of_variable_from x acl analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.values_of_variable x acl a in
    analysis.aref := a';
    values
  ;;

  let contextual_values_of_variable_from x acl ctx analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.contextual_values_of_variable x acl ctx a in
    analysis.aref := a';
    values
  ;;

  let expression_of analysis = analysis.expression;;

  let pp_analysis formatter analysis =
    A.pp_ddpa_analysis formatter !(analysis.aref)
  ;;
  let show_analysis analysis =
    A.show_ddpa_analysis !(analysis.aref)
  ;;

  let get_size analysis =
    A.get_size !(analysis.aref)
  ;;
end;;
