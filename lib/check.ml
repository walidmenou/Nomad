open Ast
open Infer

let base =
  [ ("int", TInt); ("bool", TBool); ("string", TString); ("unit", TUnit) ]

let rec convert params te =
  match te with
  | TEVar v -> (
      match List.assoc_opt v params with
      | Some t -> t
      | None -> raise (Subst.TypeError ("Unbound type variable '" ^ v)))
  | TETuple ts -> TTuple (List.map (convert params) ts)
  | TEArrow (a, b) -> TArrow (convert params a, convert params b)
  | TECon ("list", [ t ]) -> TList (convert params t)
  | TECon ("array", [ t ]) -> TArray (convert params t)
  | TECon ("grid", [ t ]) -> TGrid (convert params t)
  | TECon ((("list" | "array" | "grid") as n), _) ->
      raise (Subst.TypeError ("Wrong number of arguments for " ^ n))
  | TECon (n, ts) -> (
      let ts = List.map (convert params) ts in
      match (List.assoc_opt n base, Adt.find n) with
      | Some t, _ when ts = [] -> t
      | _, Some d when List.length d.params = List.length ts -> TCon (n, ts)
      | _, Some _ ->
          raise (Subst.TypeError ("Wrong number of arguments for " ^ n))
      | None, None -> raise (Subst.TypeError ("Unknown type " ^ n))
      | Some _, None -> raise (Subst.TypeError ("Unknown type " ^ n)))

let declare name params cons =
  let bound = List.map (fun p -> (p, fresh ())) params in
  let ids =
    List.map (fun (_, t) -> match t with TVar v -> v | _ -> 0) bound
  in
  Adt.declare name { params = ids; cons = [] };
  let cons = List.map (fun (c, ts) -> (c, List.map (convert bound) ts)) cons in
  Adt.declare name { params = ids; cons };
  let result = TCon (name, List.map snd bound) in
  List.map
    (fun (c, ts) ->
      (c, Forall (ids, List.fold_right (fun t acc -> TArrow (t, acc)) ts result)))
    cons

let rec holds_array t =
  match t with
  | TArray _ | TGrid _ -> true
  | TList t -> holds_array t
  | TTuple ts | TCon (_, ts) -> List.exists holds_array ts
  | _ -> false

let rec flat t =
  match t with
  | TArray e | TGrid e | TList e ->
      if holds_array e then
        raise
          (Subst.TypeError
             "An array cannot be stored inside another structure yet")
      else flat e
  | TTuple ts | TCon (_, ts) ->
      if List.exists holds_array ts then
        raise
          (Subst.TypeError
             "An array cannot be stored inside another structure yet")
      else List.iter flat ts
  | TArrow (a, b) ->
      flat a;
      flat b
  | _ -> ()

let check_stmt env stmt =
  match stmt with
  | TypeStmt (name, params, cons) -> (declare name params cons @ env, TUnit)
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
  Adt.reset ();
  Borrow.check_program stmts;
  let step (env, ts) stmt =
    let env, t = check_stmt env stmt in
    flat t;
    (env, match stmt with TypeStmt _ -> ts | _ -> t :: ts)
  in
  List.rev (snd (List.fold_left step (Builtin.types, []) stmts))
