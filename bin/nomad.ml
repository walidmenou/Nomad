open Nomad.Parser
open Nomad.Subst
open Nomad.Check
open Nomad.Eval

let repl () =
  print_endline "Nomad REPL v1.0.0 (Type 'exit' or Ctrl+D to quit)";
  let rec loop type_env eval_env =
    print_string ">> ";
    flush stdout;
    let line = try Some (read_line ()) with End_of_file -> None in
    match line with
    | None -> print_endline ""
    | Some line -> (
        if line = "exit" || line = "quit" then ()
        else if String.trim line = "" then loop type_env eval_env
        else
          match run program line with
          | Ok stmts -> (
              try
                let type_env' = List.fold_left check_stmt type_env stmts in
                let eval_env' =
                  List.fold_left
                    (fun e stmt ->
                      let e', res = eval_stmt e stmt in
                      (match res with
                      | Some v -> (
                          match show_val v with
                          | Some s -> print_endline s
                          | None -> ())
                      | None -> ());
                      e')
                    eval_env stmts
                in
                loop type_env' eval_env'
              with
              | TypeError e ->
                  print_endline ("Type Error: " ^ e);
                  loop type_env eval_env
              | EvaluationError e ->
                  print_endline ("Evaluation Error: " ^ e);
                  loop type_env eval_env)
          | Error e ->
              print_endline ("Parse Error: " ^ e);
              loop type_env eval_env)
  in
  loop [] []

let () =
  if Array.length Sys.argv < 2 then repl ()
  else
    let filename = Sys.argv.(1) in
    let ic = open_in filename in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    match run program s with
    | Ok stmts -> (
        try
          check_program stmts;
          eval_program stmts
        with
        | TypeError e -> print_endline ("Type Error: " ^ e)
        | EvaluationError e -> print_endline ("Evaluation Error: " ^ e))
    | Error e -> print_endline ("Parse Error: " ^ e)
