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
      let t_rec = fresh () in
      let s1, t1 = infer_w ((id, t_rec) :: env) e in
      let s2 = Subst.unify (Subst.apply s1 t_rec) (Subst.apply s1 t1) in
      let s_acc = Subst.compose s2 s1 in
      let final_t = Subst.apply s_acc t1 in
      (id, final_t) :: Subst.apply_env s_acc env

let check_program stmts =
  let _ = List.fold_left check_stmt [] stmts in
  ()
