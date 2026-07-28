open Nomad.Parser
open Nomad.Typecheck
open Nomad.Eval

let () =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: nomad <file>"
  else
    let filename = Sys.argv.(1) in
    let ic = open_in filename in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    match run program s with
    | Ok stmts ->
        (try
          check_program stmts;
          eval_program stmts
        with
        | TypeError e -> print_endline ("Type Error: " ^ e)
        | EvaluationError e -> print_endline ("Evaluation Error: " ^ e))
    | Error e -> print_endline ("Parse Error: " ^ e)
