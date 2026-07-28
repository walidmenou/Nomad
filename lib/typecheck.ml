open Ast

exception TypeError of string

type env = (ident * typ) list

let counter = ref 0

let new_tvar () =
  let v = !counter in
  counter := v + 1;
  TVar v

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TArrow (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TList t -> string_of_typ t ^ " list"
  | TVar v -> "'a" ^ string_of_int v

let rec apply_subst s t =
  match t with
  | TInt | TBool | TString | TUnit -> t
  | TVar v -> (
      match List.assoc_opt v s with Some t' -> apply_subst s t' | None -> t)
  | TArrow (t1, t2) -> TArrow (apply_subst s t1, apply_subst s t2)
  | TList t' -> TList (apply_subst s t')

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

let apply_env_subst s env = List.map (fun (id, t) -> (id, apply_subst s t)) env

let compose_subst s1 s2 =
  let s2' = List.map (fun (v, t) -> (v, apply_subst s1 t)) s2 in
  s1 @ s2'

let rec occurs_check v t =
  match t with
  | TVar v' -> v = v'
  | TArrow (t1, t2) -> occurs_check v t1 || occurs_check v t2
  | TList t' -> occurs_check v t'
  | _ -> false

let rec unify t1 t2 =
  match (t1, t2) with
  | t1, t2 when t1 = t2 -> []
  | TVar v, t | t, TVar v ->
      if occurs_check v t then raise (TypeError "Recursive types not supported")
      else [ (v, t) ]
  | TArrow (l1, r1), TArrow (l2, r2) ->
      let s1 = unify l1 l2 in
      let s2 = unify (apply_subst s1 r1) (apply_subst s1 r2) in
      compose_subst s2 s1
  | TList t1, TList t2 -> unify t1 t2
  | _ ->
      raise
        (TypeError
           ("Type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))

let infer_binop op t1 t2 =
  match op with
  | Add | Sub | Mul | Div ->
      let s1 = unify t1 TInt in
      let s2 = unify (apply_subst s1 t2) TInt in
      (compose_subst s2 s1, TInt)
  | Less | Leq | Greater | Geq ->
      let s1 = unify t1 TInt in
      let s2 = unify (apply_subst s1 t2) TInt in
      (compose_subst s2 s1, TBool)
  | Equal | Diff ->
      let s1 = unify t1 t2 in
      (s1, TBool)
  | And | Or ->
      let s1 = unify t1 TBool in
      let s2 = unify (apply_subst s1 t2) TBool in
      (compose_subst s2 s1, TBool)
  | Cons ->
      let s = unify (TList t1) t2 in
      (s, apply_subst s t2)

let rec infer_pat pat =
  match pat with
  | PatWildcard -> ([], new_tvar ())
  | PatVar id ->
      let t = new_tvar () in
      ([ (id, t) ], t)
  | PatInt _ -> ([], TInt)
  | PatBool _ -> ([], TBool)
  | PatString _ -> ([], TString)
  | PatNil -> ([], TList (new_tvar ()))
  | PatCons (p1, p2) ->
      let b1, t1 = infer_pat p1 in
      let b2, t2 = infer_pat p2 in
      let s = unify t2 (TList t1) in
      (b1 @ b2, apply_subst s t2)

let rec infer_w env e =
  match e with
  | Int _ -> ([], TInt)
  | Bool _ -> ([], TBool)
  | String _ -> ([], TString)
  | Unit -> ([], TUnit)
  | Var x -> (
      try ([], List.assoc x env)
      with Not_found -> raise (TypeError ("Unbound variable " ^ x)))
  | BinOp (e1, op, e2) ->
      let s1, t1 = infer_w env e1 in
      let s2, t2 = infer_w (apply_env_subst s1 env) e2 in
      let s_acc = compose_subst s2 s1 in
      let s3, ret_t = infer_binop op (apply_subst s_acc t1) t2 in
      (compose_subst s3 s_acc, ret_t)
  | If (cond, e1, e2) ->
      let s1, tcond = infer_w env cond in
      let s2 = unify tcond TBool in
      let s_acc1 = compose_subst s2 s1 in
      let env1 = apply_env_subst s_acc1 env in
      let s3, t1 = infer_w env1 e1 in
      let s_acc2 = compose_subst s3 s_acc1 in
      let env2 = apply_env_subst s_acc2 env in
      let s4, t2 = infer_w env2 e2 in
      let s_acc3 = compose_subst s4 s_acc2 in
      let s5 = unify (apply_subst s_acc3 t1) t2 in
      (compose_subst s5 s_acc3, apply_subst s5 t2)
  | Fun (id, body) ->
      let t_arg = new_tvar () in
      let s, t_body = infer_w ((id, t_arg) :: env) body in
      (s, TArrow (apply_subst s t_arg, t_body))
  | App (e1, e2) -> (
      let s1, t1 = infer_w env e1 in
      let s2, t2 = infer_w (apply_env_subst s1 env) e2 in
      let s_acc = compose_subst s2 s1 in
      let t_ret = new_tvar () in
      try
        let s3 = unify (apply_subst s_acc t1) (TArrow (t2, t_ret)) in
        (compose_subst s3 s_acc, apply_subst s3 t_ret)
      with TypeError err ->
        raise
          (TypeError
             ("In App(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "): "
            ^ err)))
  | Let (id, e1, e2) ->
      let s1, t1 = infer_w env e1 in
      let env' = apply_env_subst s1 env in
      let s2, t2 = infer_w ((id, apply_subst s1 t1) :: env') e2 in
      (compose_subst s2 s1, t2)
  | Rec (id, e1, e2) ->
      let t_rec = new_tvar () in
      let s1, t1 = infer_w ((id, t_rec) :: env) e1 in
      let s2 = unify (apply_subst s1 t_rec) (apply_subst s1 t1) in
      let s_acc = compose_subst s2 s1 in
      let env' = apply_env_subst s_acc env in
      let s3, t2 = infer_w ((id, apply_subst s_acc t1) :: env') e2 in
      (compose_subst s3 s_acc, t2)
  | List exprs ->
      let t_elem = new_tvar () in
      let s_final, _ =
        List.fold_left
          (fun (s_acc, env_acc) e ->
            let s, t = infer_w env_acc e in
            let s_acc2 = compose_subst s s_acc in
            let s' = unify (apply_subst s_acc2 t) (apply_subst s_acc2 t_elem) in
            let total_s = compose_subst s' s_acc2 in
            (total_s, apply_env_subst total_s env))
          ([], env) exprs
      in
      (s_final, TList (apply_subst s_final t_elem))
  | Match (e, cases) ->
      let s1, t_e = infer_w env e in
      let t_ret = new_tvar () in
      let s_final, _ =
        List.fold_left
          (fun (s_acc, env_acc) (pat, body) ->
            let pat_bindings, pat_type = infer_pat pat in
            let s_pat = unify (apply_subst s_acc t_e) pat_type in
            let env_body = pat_bindings @ apply_env_subst s_pat env_acc in
            let s_body, t_body = infer_w env_body body in
            let s_acc2 = compose_subst s_body (compose_subst s_pat s_acc) in
            let s_ret = unify (apply_subst s_acc2 t_ret) t_body in
            let total_s = compose_subst s_ret s_acc2 in
            (total_s, apply_env_subst total_s env))
          (s1, apply_env_subst s1 env)
          cases
      in
      (s_final, apply_subst s_final t_ret)

let infer env expr =
  let s, t = infer_w env expr in
  apply_subst s t

let check_stmt env stmt =
  match stmt with
  | ExprStmt e ->
      let _ = infer env e in
      env
  | LetStmt (id, e) ->
      let t = infer env e in
      (id, t) :: env
  | RecStmt (id, e) ->
      let t_rec = new_tvar () in
      let s1, t1 = infer_w ((id, t_rec) :: env) e in
      let s2 = unify (apply_subst s1 t_rec) (apply_subst s1 t1) in
      let s_acc = compose_subst s2 s1 in
      let final_t = apply_subst s_acc t1 in
      (id, final_t) :: apply_env_subst s_acc env

let check_program stmts =
  let _ = List.fold_left check_stmt [] stmts in
  ()
