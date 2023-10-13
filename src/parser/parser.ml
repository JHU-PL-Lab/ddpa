(**
   A front-end for the parser library.
*)

open Batteries;;

open Lexing;;

open Util;;

exception Parse_error of exn * int * int * string;;

let handle_parse_error buf f =
  try
    f ()
  with
  | exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error(exn,line,column,tok)
;;

let parse_expressions (input : IO.input) =
  let buf = Lexing.from_channel input in
  let read_expr () =
    handle_parse_error buf @@ fun () ->
    Generated_parser.delim_expr Generated_lexer.token buf
  in
  LazyList.unfold
    true
    (fun keep_going ->
      if not keep_going then None else
        match read_expr () with
        | NoExpr -> None
        | LastExpr e -> Some(e, false)
        | SomeExpr e -> Some(e, true)
    )

let parse_program (input : IO.input) =
  let buf = Lexing.from_channel input in
  handle_parse_error buf @@ fun () ->
  Generated_parser.prog Generated_lexer.token buf
;;
