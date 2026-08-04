open Nomad.Parser
open Nomad.Subst
open Nomad.Check
open Nomad.Eval

let repl () =
  print_endline "Nomad REPL (Type 'exit' or Ctrl+D to quit)";
  let rec loop borrow type_env eval_env =
    print_string ">> ";
    flush stdout;
    let line = try Some (read_line ()) with End_of_file -> None in
    match line with
    | None -> print_endline ""
    | Some line -> (
        if line = "exit" || line = "quit" then ()
        else if String.trim line = "" then loop borrow type_env eval_env
        else
          match run program line with
          | Ok stmts -> (
              try
                let borrow' =
                  List.fold_left Nomad.Borrow.check_stmt borrow stmts
                in
                let type_env', types =
                  List.fold_left
                    (fun (e, ts) stmt ->
                      let e', t = check_stmt e stmt in
                      (e', t :: ts))
                    (type_env, []) stmts
                in
                let eval_env' =
                  List.fold_left2
                    (fun e t stmt ->
                      let e', v = eval_stmt e stmt in
                      (match stmt with
                      | TypeStmt _ -> ()
                      | _ -> print_endline (show_val t v));
                      e')
                    eval_env (List.rev types) stmts
                in
                loop borrow' type_env' eval_env'
              with
              | TypeError e ->
                  print_endline ("Type Error: " ^ e);
                  loop borrow type_env eval_env
              | EvaluationError e ->
                  print_endline ("Evaluation Error: " ^ e);
                  loop borrow type_env eval_env
              | Nomad.Borrow.BorrowError e ->
                  print_endline ("Borrow Error: " ^ e);
                  loop borrow type_env eval_env)
          | Error e ->
              print_endline ("Parse Error: " ^ e);
              loop borrow type_env eval_env)
  in
  loop Nomad.Borrow.initial Nomad.Builtin.types Nomad.Eval.values

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
        try eval_program (check_program stmts) stmts with
        | TypeError e -> print_endline ("Type Error: " ^ e)
        | EvaluationError e -> print_endline ("Evaluation Error: " ^ e)
        | Nomad.Borrow.BorrowError e -> print_endline ("Borrow Error: " ^ e))
    | Error e -> print_endline ("Parse Error: " ^ e)
