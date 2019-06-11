(** A "context stack" which performs no actions. *)

open Jhupllib;;

open Plume_context_model;;
open Pp_utils;;

module Stack : Context_model =
struct
  type t = unit;;
  let equal () () = true;;
  let compare () () = 0;;
  let empty = ();;
  let push _ () = ();;
  let pp formatter () = Format.pp_print_string formatter "-";;
  let show = pp_to_string pp;;
  let to_yojson _ = `List [];;
  let name = "0ddpa";;
end;;