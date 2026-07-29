open Ast

type env = (ident * typ) list

let counter = ref 0

let fresh () =
  let v = !counter in
  counter := v + 1;
  TVar v

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Unit -> "()"
  | Var x -> x
  | BinOp (e1, _, e2) ->
      "BinOp(" ^ string_of_expr e1 ^ ", ..., " ^ string_of_expr e2 ^ ")"
  | If (_, _, _) -> "If(...)"
  | Fun (x, e) -> "Fun(" ^ x ^ ", " ^ string_of_expr e ^ ")"
  | App (e1, e2) -> "App(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Let (_, _, _) -> "Let(...)"
  | Rec (_, _, _) -> "Rec(...)"
  | List _ -> "List[...]"
  | Match (_, _) -> "Match(...)"

let infer_binop op t1 t2 =
  match op with
  | Add | Sub | Mul | Div ->
      let s1 = Subst.unify t1 TInt in
      let s2 = Subst.unify (Subst.apply s1 t2) TInt in
      (Subst.compose s2 s1, TInt)
  | Less | Leq | Greater | Geq ->
      let s1 = Subst.unify t1 TInt in
      let s2 = Subst.unify (Subst.apply s1 t2) TInt in
      (Subst.compose s2 s1, TBool)
  | Equal | Diff ->
      let s1 = Subst.unify t1 t2 in
      (s1, TBool)
  | And | Or ->
      let s1 = Subst.unify t1 TBool in
      let s2 = Subst.unify (Subst.apply s1 t2) TBool in
      (Subst.compose s2 s1, TBool)
  | Cons ->
      let s = Subst.unify (TList t1) t2 in
      (s, Subst.apply s t2)

let rec infer_pat pat =
  match pat with
  | PatWildcard -> ([], fresh ())
  | PatVar id ->
      let t = fresh () in
      ([ (id, t) ], t)
  | PatInt _ -> ([], TInt)
  | PatBool _ -> ([], TBool)
  | PatString _ -> ([], TString)
  | PatNil -> ([], TList (fresh ()))
  | PatCons (p1, p2) ->
      let b1, t1 = infer_pat p1 in
      let b2, t2 = infer_pat p2 in
      let s = Subst.unify t2 (TList t1) in
      (b1 @ b2, Subst.apply s t2)

let rec infer_w env e =
  match e with
  | Int _ -> ([], TInt)
  | Bool _ -> ([], TBool)
  | String _ -> ([], TString)
  | Unit -> ([], TUnit)
  | Var x -> (
      try ([], List.assoc x env)
      with Not_found -> raise (Subst.TypeError ("Unbound variable " ^ x)))
  | BinOp (e1, op, e2) ->
      let s1, t1 = infer_w env e1 in
      let s2, t2 = infer_w (Subst.apply_env s1 env) e2 in
      let s_acc = Subst.compose s2 s1 in
      let s3, ret_t = infer_binop op (Subst.apply s_acc t1) t2 in
      (Subst.compose s3 s_acc, ret_t)
  | If (cond, e1, e2) ->
      let s, t_cond = infer_w env cond in
      let s = Subst.compose (Subst.unify t_cond TBool) s in
      let s, t1 = step s env e1 in
      let s, t2 = step s env e2 in
      let s' = Subst.unify (Subst.apply s t1) t2 in
      (Subst.compose s' s, Subst.apply s' t2)
  | Fun (id, body) ->
      let t_arg = fresh () in
      let s, t_body = infer_w ((id, t_arg) :: env) body in
      (s, TArrow (Subst.apply s t_arg, t_body))
  | App (e1, e2) -> (
      let s1, t1 = infer_w env e1 in
      let s2, t2 = infer_w (Subst.apply_env s1 env) e2 in
      let s_acc = Subst.compose s2 s1 in
      let t_ret = fresh () in
      try
        let s3 = Subst.unify (Subst.apply s_acc t1) (TArrow (t2, t_ret)) in
        (Subst.compose s3 s_acc, Subst.apply s3 t_ret)
      with Subst.TypeError err ->
        raise
          (Subst.TypeError
             ("In App(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "): "
            ^ err)))
  | Let (id, e1, e2) ->
      let s1, t1 = infer_w env e1 in
      let env' = Subst.apply_env s1 env in
      let s2, t2 = infer_w ((id, Subst.apply s1 t1) :: env') e2 in
      (Subst.compose s2 s1, t2)
  | Rec (id, e1, e2) ->
      let t_rec = fresh () in
      let s1, t1 = infer_w ((id, t_rec) :: env) e1 in
      let s2 = Subst.unify (Subst.apply s1 t_rec) (Subst.apply s1 t1) in
      let s_acc = Subst.compose s2 s1 in
      let env' = Subst.apply_env s_acc env in
      let s3, t2 = infer_w ((id, Subst.apply s_acc t1) :: env') e2 in
      (Subst.compose s3 s_acc, t2)
  | List exprs ->
      let t_elem = fresh () in
      let s =
        List.fold_left
          (fun s e ->
            let s, t = step s env e in
            Subst.compose
              (Subst.unify (Subst.apply s t) (Subst.apply s t_elem))
              s)
          [] exprs
      in
      (s, TList (Subst.apply s t_elem))
  | Match (e, cases) ->
      let s, t_e = infer_w env e in
      let t_ret = fresh () in
      let s, _ =
        List.fold_left
          (fun (s, env_acc) (pat, body) ->
            let bindings, t_pat = infer_pat pat in
            let s_pat = Subst.unify (Subst.apply s t_e) t_pat in
            let s_body, t_body =
              infer_w (bindings @ Subst.apply_env s_pat env_acc) body
            in
            let s = Subst.compose s_body (Subst.compose s_pat s) in
            let s =
              Subst.compose (Subst.unify (Subst.apply s t_ret) t_body) s
            in
            (s, Subst.apply_env s env))
          (s, Subst.apply_env s env)
          cases
      in
      (s, Subst.apply s t_ret)

(* Infers [e] in [env] refined by everything learnt so far, folding the
   substitution it forces back into that accumulated one. *)
and step s env e =
  let s', t = infer_w (Subst.apply_env s env) e in
  (Subst.compose s' s, t)

let infer env expr =
  let s, t = infer_w env expr in
  Subst.apply s t
