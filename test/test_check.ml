open Nomad.Ast
open Nomad.Typecheck

(* Types bound by a run of statements, oldest binding first. *)
let check_stmts stmts expected () =
  match List.fold_left check_stmt [] stmts with
  | env ->
      let binding (id, t) = id ^ " : " ^ string_of_typ t in
      Alcotest.(check (list string)) "bindings" expected (List.rev_map binding env)
  | exception TypeError e -> Alcotest.fail ("TypeError: " ^ e)

let check_error stmts expected () =
  match List.fold_left check_stmt [] stmts with
  | _ -> Alcotest.fail "expected type error"
  | exception TypeError e -> Alcotest.(check string) "error message" expected e

(* fun n -> if n = 0 then 1 else n * f (n - 1) *)
let fact =
  Fun ("n",
    If (BinOp (Var "n", Equal, Int 0),
      Int 1,
      BinOp (Var "n", Mul, App (Var "f", BinOp (Var "n", Sub, Int 1)))))

let test_let () =
  check_stmts [LetStmt ("x", Int 1)] ["x : int"] ();
  check_stmts
    [LetStmt ("x", Int 1); LetStmt ("y", BinOp (Var "x", Add, Int 1))]
    ["x : int"; "y : int"] ()

let test_rec () =
  check_stmts [RecStmt ("f", fact)] ["f : (int -> int)"] ();
  check_stmts
    [RecStmt ("f", fact); LetStmt ("x", App (Var "f", Int 5))]
    ["f : (int -> int)"; "x : int"] ()

let test_expr () =
  (* An expression statement is checked but binds nothing. *)
  check_stmts [LetStmt ("x", Int 1); ExprStmt (BinOp (Var "x", Add, Int 1))] ["x : int"] ()

let test_program () =
  check_program [LetStmt ("x", Int 1); RecStmt ("f", fact); ExprStmt (App (Var "f", Var "x"))];
  Alcotest.(check bool) "program checks" true true

let test_errors () =
  check_error [ExprStmt (Var "x")] "Unbound variable x" ();
  (* Errors from an earlier statement are not swallowed by a later one. *)
  check_error [LetStmt ("x", BinOp (Int 1, Add, Bool true)); LetStmt ("y", Int 2)]
    "Type mismatch: bool and int" ()

let tests = [
  "let", `Quick, test_let;
  "let rec", `Quick, test_rec;
  "expression", `Quick, test_expr;
  "program", `Quick, test_program;
  "errors", `Quick, test_errors;
]
