open Ast

exception EvaluationError of string

let match_pattern pat v =
  match (pat, v) with
  | PatWildcard, _ -> Some []
  | PatVar id, _ -> Some [ (id, v) ]
  | PatInt n1, Int n2 when n1 = n2 -> Some []
  | PatBool b1, Bool b2 when b1 = b2 -> Some []
  | PatString s1, String s2 when s1 = s2 -> Some []
  | _, _ -> None

let rec try_match cases v env eval_fn =
  match cases with
  | [] -> raise (EvaluationError "Match failure")
  | (pat, body) :: rest -> (
      match match_pattern pat v with
      | Some bindings -> eval_fn (bindings @ env) body
      | None -> try_match rest v env eval_fn)

let rec eval env e =
  match e with
  | Int _ | Bool _ | String _ | Unit | Fun _ -> e
  | Var x -> (
      try List.assoc x env
      with Not_found -> raise (EvaluationError ("Unbound Variable: " ^ x)))
  | BinOp (e1, op, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (
          match op with
          | Add -> Int (n1 + n2)
          | Sub -> Int (n1 - n2)
          | Mul -> Int (n1 * n2)
          | Div ->
              if n2 = 0 then raise (EvaluationError "Division by zero")
              else Int (n1 / n2)
          | Equal -> Bool (n1 = n2)
          | Diff -> Bool (n1 <> n2)
          | Less -> Bool (n1 < n2)
          | Leq -> Bool (n1 <= n2)
          | Greater -> Bool (n1 > n2)
          | Geq -> Bool (n1 >= n2)
          | _ -> raise (EvaluationError "Invalid operation on integers"))
      | Bool b1, Bool b2 -> (
          match op with
          | And -> Bool (b1 && b2)
          | Or -> Bool (b1 || b2)
          | Equal -> Bool (b1 = b2)
          | Diff -> Bool (b1 <> b2)
          | _ -> raise (EvaluationError "Invalid operation on booleans"))
      | _ -> raise (EvaluationError "Type mismatch in binary operation"))
  | Let (id, e1, e2) ->
      let v1 = eval env e1 in
      eval ((id, v1) :: env) e2
  | If (cond, e1, e2) -> (
      match eval env cond with
      | Bool true -> eval env e1
      | Bool false -> eval env e2
      | _ -> raise (EvaluationError "Condition must be a boolean"))
  | App (e1, e2) -> (
      let f = eval env e1 in
      let arg = eval env e2 in
      match f with
      | Fun (id, body) -> eval ((id, arg) :: env) body
      | _ -> raise (EvaluationError "Application of non-function"))
  | Rec (id, e1, e2) ->
      let f = eval env e1 in
      eval ((id, f) :: env) e2
  | Match (e, cases) ->
      let v = eval env e in
      try_match cases v env eval
  | List exprs -> List (Stdlib.List.map (eval env) exprs)
