open Ast
open Infer

let check_stmt env stmt =
  match stmt with
  | ExprStmt e ->
      let s, t = infer_w env e in
      (apply_env s env, Subst.apply s t)
  | LetStmt (id, e) ->
      let s, t = infer_w env e in
      let env = apply_env s env in
      let t = Subst.apply s t in
      ((id, generalize env t) :: env, t)
  | RecStmt (id, e) ->
      let s, t = rec_binding env id e in
      let env = apply_env s env in
      ((id, generalize env t) :: env, t)

let check_program stmts =
  let step (env, ts) stmt =
    let env, t = check_stmt env stmt in
    (env, t :: ts)
  in
  List.rev (snd (List.fold_left step ([], []) stmts))
