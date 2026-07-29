open Ast
open Infer

let check_stmt env stmt =
  match stmt with
  | ExprStmt e ->
      let _ = infer env e in
      env
  | LetStmt (id, e) ->
      let t = infer env e in
      (id, t) :: env
  | RecStmt (id, e) ->
      let s, t = rec_binding env id e in
      (id, t) :: Subst.apply_env s env

let check_program stmts =
  let _ = List.fold_left check_stmt [] stmts in
  ()
