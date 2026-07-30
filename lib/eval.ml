open Ast

exception EvaluationError of string

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value list
  | VClos of ident * expr * env
  | VRecClos of ident * ident * expr * env

and env = (ident * value) list

let rec match_pattern pat v =
  match (pat, v) with
  | PatWildcard, _ -> Some []
  | PatVar id, _ -> Some [ (id, v) ]
  | PatInt n1, VInt n2 when n1 = n2 -> Some []
  | PatBool b1, VBool b2 when b1 = b2 -> Some []
  | PatString s1, VString s2 when s1 = s2 -> Some []
  | PatNil, VList [] -> Some []
  | PatCons (p1, p2), VList (h :: t) -> (
      match (match_pattern p1 h, match_pattern p2 (VList t)) with
      | Some b1, Some b2 -> Some (b1 @ b2)
      | _ -> None)
  | PatTuple ps, VTuple vs when List.length ps = List.length vs ->
      List.fold_left2
        (fun acc p v ->
          match (acc, match_pattern p v) with
          | Some b, Some b' -> Some (b @ b')
          | _ -> None)
        (Some []) ps vs
  | _, _ -> None

let rec eval env e =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | String s -> VString s
  | Unit -> VUnit
  | Fun (id, body) -> VClos (id, body, env)
  | Var x -> (
      try List.assoc x env
      with Not_found -> raise (EvaluationError ("Unbound Variable: " ^ x)))
  | BinOp (e1, And, e2) -> if truth env e1 then eval env e2 else VBool false
  | BinOp (e1, Or, e2) -> if truth env e1 then VBool true else eval env e2
  | BinOp (e1, op, e2) -> binop op (eval env e1) (eval env e2)
  | Let (id, e1, e2) -> eval ((id, eval env e1) :: env) e2
  | Rec (id, e1, e2) -> eval ((id, rec_value env id e1) :: env) e2
  | If (cond, e1, e2) -> if truth env cond then eval env e1 else eval env e2
  | App (e1, e2) -> apply (eval env e1) (eval env e2)
  | List exprs -> VList (List.map (eval env) exprs)
  | Tuple exprs -> VTuple (List.map (eval env) exprs)
  | Comp (body, qs) -> VList (List.rev (comp env qs body []))
  | Range (e1, e2) -> (
      match (eval env e1, eval env e2) with
      | VInt lo, VInt hi ->
          VList (List.init (max 0 (hi - lo + 1)) (fun i -> VInt (lo + i)))
      | _ -> raise (EvaluationError "A range needs two integers"))
  | Match (e, cases) -> try_match env cases (eval env e)

and apply f arg =
  match f with
  | VClos (id, body, env) -> eval ((id, arg) :: env) body
  | VRecClos (name, id, body, env) -> eval ((id, arg) :: (name, f) :: env) body
  | _ -> raise (EvaluationError "Application of non-function")

and rec_value env id e =
  match e with
  | Fun (arg, body) -> VRecClos (id, arg, body, env)
  | _ -> eval env e

and try_match env cases v =
  match cases with
  | [] -> raise (EvaluationError "Match failure")
  | (pat, body) :: rest -> (
      match match_pattern pat v with
      | Some bindings -> eval (bindings @ env) body
      | None -> try_match env rest v)

and comp env qs body acc =
  match qs with
  | [] -> eval env body :: acc
  | Gen (p, src) :: rest -> (
      match eval env src with
      | VList vs ->
          List.fold_left
            (fun acc v ->
              match match_pattern p v with
              | Some bindings -> comp (bindings @ env) rest body acc
              | None -> acc)
            acc vs
      | _ -> raise (EvaluationError "A generator needs a list"))
  | Guard e :: rest -> if truth env e then comp env rest body acc else acc
  | QLet (id, e) :: rest -> comp ((id, eval env e) :: env) rest body acc

and truth env e =
  match eval env e with
  | VBool b -> b
  | _ -> raise (EvaluationError "Expected a boolean")

and binop op v1 v2 =
  match (op, v1, v2) with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Sub, VInt a, VInt b -> VInt (a - b)
  | Mul, VInt a, VInt b -> VInt (a * b)
  | Div, VInt _, VInt 0 -> raise (EvaluationError "Division by zero")
  | Div, VInt a, VInt b -> VInt (a / b)
  | Less, VInt a, VInt b -> VBool (a < b)
  | Leq, VInt a, VInt b -> VBool (a <= b)
  | Greater, VInt a, VInt b -> VBool (a > b)
  | Geq, VInt a, VInt b -> VBool (a >= b)
  | Cons, v, VList vs -> VList (v :: vs)
  | Equal, _, _ -> VBool (equal v1 v2)
  | Diff, _, _ -> VBool (not (equal v1 v2))
  | _ -> raise (EvaluationError "Type mismatch in binary operation")

and equal v1 v2 =
  match (v1, v2) with
  | (VClos _ | VRecClos _), _ | _, (VClos _ | VRecClos _) ->
      raise (EvaluationError "Cannot compare functions")
  | (VList vs1 | VTuple vs1), (VList vs2 | VTuple vs2) ->
      List.length vs1 = List.length vs2 && List.for_all2 equal vs1 vs2
  | _ -> v1 = v2

let eval_stmt env stmt =
  match stmt with
  | ExprStmt e -> (env, eval env e)
  | LetStmt (id, e) ->
      let v = eval env e in
      ((id, v) :: env, v)
  | RecStmt (id, e) ->
      let v = rec_value env id e in
      ((id, v) :: env, v)

let rec show = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VList vs -> "[" ^ String.concat "; " (List.map show vs) ^ "]"
  | VTuple vs -> "(" ^ String.concat ", " (List.map show vs) ^ ")"
  | VClos _ | VRecClos _ -> "<fun>"

let show_val ty v =
  match v with
  | VClos _ | VRecClos _ -> "<fun> : " ^ display_typ ty
  | _ -> show v

let run_program stmts =
  let step (env, vs) stmt =
    let env, v = eval_stmt env stmt in
    (env, v :: vs)
  in
  List.rev (snd (List.fold_left step ([], []) stmts))

let eval_program types stmts =
  List.iter2 (fun t v -> print_endline (show_val t v)) types (run_program stmts)
