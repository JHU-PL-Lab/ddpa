open Odefa_ast;;

type translation_context =
  { mutable tc_fresh_name_counter : int;
    tc_fresh_suffix_separator : string;
  }
;;

let new_translation_context (suffix : string) : translation_context =
  { tc_fresh_name_counter = 0;
    tc_fresh_suffix_separator = suffix;
  }
;;

module TranslationMonad :
sig
  type 'a m;;
  include Jhupllib.Monads.Monad with type 'a m := 'a m;;
  include Jhupllib.Monads.Utils with type 'a m := 'a m;;
  val run : translation_context -> 'a m -> 'a;;
  val fresh_name : string -> string m;;
  val fresh_var : string -> Ast.var m;;
  val freshness_string : string m;;
  val list_fold_left_m : ('acc -> 'el -> 'acc m) -> 'acc -> 'el list -> 'acc m
  val list_fold_right_m : ('el -> 'acc -> 'acc m) -> 'el list -> 'acc -> 'acc m
  val (@@@) : ('a -> 'b m) -> 'a m -> 'b m
end =
struct
  module Base = struct
    type 'a m = translation_context -> 'a;;
    let pure (x : 'a) : 'a m = (fun _ -> x);;
    let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
      fun ctx -> f (x ctx) ctx
    ;;
  end;;
  include Base;;
  include Jhupllib.Monads.MakeUtils(Base);;
  let run ctx m =
    m ctx
  ;;
  let fresh_name name ctx =
    let n = ctx.tc_fresh_name_counter in
    ctx.tc_fresh_name_counter <- n + 1;
    name ^ ctx.tc_fresh_suffix_separator ^ string_of_int n
  ;;
  let fresh_var name ctx =
    let name' = fresh_name name ctx in
    Ast.Var(Ast.Ident name', None)
  ;;
  let freshness_string ctx = ctx.tc_fresh_suffix_separator;;
  let rec list_fold_left_m fn acc els =
    match els with
    | [] -> return acc
    | h :: t ->
      let%bind acc' = fn acc h in
      list_fold_left_m fn acc' t
  ;;
  let rec list_fold_right_m fn els acc =
    match els with
    | [] -> return acc
    | h :: t ->
      let%bind acc' = list_fold_right_m fn t acc in
      fn h acc'
  ;;
  let (@@@) f x = bind x f;;
end;;
