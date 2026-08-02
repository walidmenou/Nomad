open Ast
open Pretty

type scheme = Forall of int list * typ
type env = (ident * scheme) list

let counter = ref 0

let fresh () =
  let v = !counter in
  counter := v + 1;
  TVar v

let rec vars = function
  | TVar v -> [ v ]
  | TArrow (t1, t2) -> vars t1 @ vars t2
  | TList t -> vars t
  | TTuple ts | TCon (_, ts) -> List.concat_map vars ts
  | _ -> []

let mono t = Forall ([], t)
let free (Forall (qs, t)) = List.filter (fun v -> not (List.mem v qs)) (vars t)

let apply_scheme s (Forall (qs, t)) =
  Forall (qs, Subst.apply (List.filter (fun (v, _) -> not (List.mem v qs)) s) t)

let apply_env s env = List.map (fun (id, sc) -> (id, apply_scheme s sc)) env

let generalize env t =
  let bound = List.concat_map (fun (_, sc) -> free sc) env in
  let loose = List.filter (fun v -> not (List.mem v bound)) (vars t) in
  Forall (List.sort_uniq compare loose, t)

let instantiate (Forall (qs, t)) =
  Subst.apply (List.map (fun v -> (v, fresh ())) qs) t

let infer_binop op t1 t2 =
  match op with
  | Add | Sub | Mul | Div | Mod ->
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
  | Append ->
      let s1 = Subst.unify t1 t2 in
      let s2 = Subst.unify (Subst.apply s1 t1) (TList (fresh ())) in
      let s = Subst.compose s2 s1 in
      (s, Subst.apply s t1)

let join b1 b2 =
  match List.find_opt (fun (id, _) -> List.mem_assoc id b2) b1 with
  | Some (id, _) ->
      raise (Subst.TypeError (id ^ " is bound twice in the same pattern"))
  | None -> b1 @ b2

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
      (join b1 b2, Subst.apply s t2)
  | PatTuple ps ->
      let bs, ts = List.split (List.map infer_pat ps) in
      (List.fold_left join [] bs, TTuple ts)

let rec infer_w env e =
  match e with
  | Int _ -> ([], TInt)
  | Bool _ -> ([], TBool)
  | String _ -> ([], TString)
  | Unit -> ([], TUnit)
  | Var x -> (
      try ([], instantiate (List.assoc x env))
      with Not_found -> raise (Subst.TypeError ("Unbound variable " ^ x)))
  | BinOp (e1, op, e2) ->
      let s1, t1 = infer_w env e1 in
      let s2, t2 = infer_w (apply_env s1 env) e2 in
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
      let s, t_body = infer_w ((id, mono t_arg) :: env) body in
      (s, TArrow (Subst.apply s t_arg, t_body))
  | App (e1, e2) -> (
      let s1, t1 = infer_w env e1 in
      let s2, t2 = infer_w (apply_env s1 env) e2 in
      let s_acc = Subst.compose s2 s1 in
      let t_ret = fresh () in
      try
        let s3 = Subst.unify (Subst.apply s_acc t1) (TArrow (t2, t_ret)) in
        (Subst.compose s3 s_acc, Subst.apply s3 t_ret)
      with Subst.TypeError err ->
        raise (Subst.TypeError ("In " ^ string_of_expr e ^ ": " ^ err)))
  | Let (id, e1, e2) ->
      let s1, t1 = infer_w env e1 in
      let env = apply_env s1 env in
      let sc = generalize env (Subst.apply s1 t1) in
      let s2, t2 = infer_w ((id, sc) :: env) e2 in
      (Subst.compose s2 s1, t2)
  | Rec (id, e1, e2) ->
      let s, t1 = rec_binding env id e1 in
      let env = apply_env s env in
      let s', t2 = infer_w ((id, generalize env t1) :: env) e2 in
      (Subst.compose s' s, t2)
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
  | Tuple exprs ->
      let s, ts =
        List.fold_left
          (fun (s, ts) e ->
            let s, t = step s env e in
            (s, t :: ts))
          ([], []) exprs
      in
      (s, TTuple (List.rev_map (Subst.apply s) ts))
  | Comp (body, qs) ->
      let s, env =
        List.fold_left (fun (s, env) q -> infer_qual s env q) ([], env) qs
      in
      let s, t = step s env body in
      (s, TList (Subst.apply s t))
  | Range (e1, e2) ->
      let s1, t1 = infer_w env e1 in
      let s = Subst.compose (Subst.unify t1 TInt) s1 in
      let s, t2 = step s env e2 in
      (Subst.compose (Subst.unify t2 TInt) s, TList TInt)
  | Match (e, cases) ->
      let s, t_e = infer_w env e in
      let t_ret = fresh () in
      let s =
        List.fold_left
          (fun s (pat, body) ->
            let bindings, t_pat = infer_pat pat in
            let s = Subst.compose (Subst.unify (Subst.apply s t_e) t_pat) s in
            let bound =
              List.map (fun (id, t) -> (id, mono (Subst.apply s t))) bindings
            in
            let s_body, t_body = infer_w (bound @ apply_env s env) body in
            let s = Subst.compose s_body s in
            Subst.compose (Subst.unify (Subst.apply s t_ret) t_body) s)
          s cases
      in
      if not (Pat.exhaustive (Subst.apply s t_e) (List.map fst cases)) then
        raise (Subst.TypeError "This match is not exhaustive");
      (s, Subst.apply s t_ret)

and infer_qual s env q =
  match q with
  | Gen (p, src) ->
      let s, t_src = step s env src in
      let bindings, t_pat = infer_pat p in
      let s = Subst.compose (Subst.unify t_src (TList t_pat)) s in
      let bound =
        List.map (fun (id, t) -> (id, mono (Subst.apply s t))) bindings
      in
      (s, bound @ apply_env s env)
  | Guard e ->
      let s, t = step s env e in
      (Subst.compose (Subst.unify t TBool) s, apply_env s env)
  | QLet (id, e) ->
      let s, t = step s env e in
      let env = apply_env s env in
      (s, (id, generalize env (Subst.apply s t)) :: env)

and step s env e =
  let s', t = infer_w (apply_env s env) e in
  (Subst.compose s' s, t)

and rec_binding env id e =
  let t_rec = fresh () in
  let s, t = infer_w ((id, mono t_rec) :: env) e in
  let s =
    Subst.compose (Subst.unify (Subst.apply s t_rec) (Subst.apply s t)) s
  in
  (s, Subst.apply s t)

let infer env expr =
  let s, t = infer_w env expr in
  Subst.apply s t
