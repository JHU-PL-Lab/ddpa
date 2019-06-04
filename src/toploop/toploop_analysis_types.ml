(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ast_pp;;
open Toploop_analysis_wrapper_types;;
open Ddpa_abstract_ast;;

type abs_filtered_value_set = Abs_filtered_value_set.t;;
let pp_abs_filtered_value_set =
  Pp_utils.pp_set pp_abs_filtered_value Abs_filtered_value_set.enum
and compare_abs_filtered_value_set =
  Abs_filtered_value_set.compare
and equal_abs_filtered_value_set =
  Abs_filtered_value_set.equal
;;

type error =
  | Application_of_non_function of
      abstract_var * abstract_var * abs_filtered_value * abs_filtered_value_set
  (** Represents the application of a non-function value.  The arguments
      are the variable identifying the call site clause, the invoked variable,
      and the abstract non-function value which appeared at the call site. *)
  | Projection_of_non_record of abstract_var * abstract_var * abs_filtered_value
  (** Represents the projection of a label from a non-record value.  The
      arguments are the variable identifying the clause containing the
      projection, the record variable, and the abstract value from which the
      projection occurred. *)
  | Projection_of_absent_label of
      abstract_var * abstract_var * abs_filtered_value * ident
  (** Represents the projection of a label from a record value which does not
      have that label.  The arguments are the variable identifying the clause
      containing the projection, the record variable, the abstract value from
      which the projection occurred, and the ident we failed to project. *)
  | Deref_of_non_ref of abstract_var * abstract_var * abs_filtered_value
  (** Represents the dereference of a non-ref value.  The arguments are the
      variable identifying the clause where the assignment occurred, the
      dereferenced variable, and the abstract non-cell value which appeared
      there. *)
  | Update_of_non_ref of abstract_var * abstract_var * abs_filtered_value
  (** Represents the cell-set update of a non-ref value.  The arguments are
      the variable identifying the clause where the assignment occurred, the
      updated variable, and the abstract non-cell value which appeared
      there. *)
  | Invalid_binary_operation of
      abstract_var * binary_operator * abstract_var * abs_filtered_value *
      abstract_var * abs_filtered_value
  (** Represents invalid use of a binary operator.  The arguments are, in order,
      - The variable identifying the clause where the assignment occurred.
      - The binary operator appearing in the clause.
      - The variable for the first operand.
      - A possible value of the first operand.
      - The variable for the second operand.
      - A possible value of the second operand.
  *)
  | Invalid_unary_operation of
      abstract_var * unary_operator * abstract_var * abs_filtered_value
  (** Represents invalid use of a unary operator.  The arguments are, in order,
      - The variable identifying the clause where the assignment occurred.
      - The unary operator appearing in the clause.
      - The variable for the operand.
      - A possible value of the operand.
  *)
  | Invalid_indexing_subject of
      abstract_var * abstract_var * abs_filtered_value
  (** Represents the indexing of a non-indexable subject.  The arguments are
      the variable identifying the indexing clause, the variable of the indexing
      subject, and a possible value of the indexing subject. *)
  | Invalid_indexing_argument of
      abstract_var * abstract_var * abs_filtered_value
  (** Represents an invalid indexing argument.  The arguments are the
      variable identifying the indexing clause, the variable of the index,
      and a possible value of the index. *)
  [@@deriving eq, ord, show]
;;

module Error_ord =
struct
  type t = error
  let compare = compare_error
end;;

module Error_set = Set.Make(Error_ord);;

module type Analysis_sig = sig
  module Analysis_wrapper : Analysis_wrapper
  val find_errors : Analysis_wrapper.analysis -> error Enum.t
end;;
