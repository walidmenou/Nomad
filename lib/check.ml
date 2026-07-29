open Ast
open Infer

let check_stmt env stmt =
  match stmt with
  | ExprStmt e ->
      let s, _ = infer_w env e in
      apply_env s env
  | LetStmt (id, e) ->
      let s, t = infer_w env e in
      let env = apply_env s env in
      (id, generalize env (Subst.apply s t)) :: env
  | RecStmt (id, e) ->
      let s, t = rec_binding env id e in
      let env = apply_env s env in
      (id, generalize env t) :: env

let check_program stmts =
  let _ = List.fold_left check_stmt [] stmts in
  ()
