open Ast

exception EvaluationError of string

let rec eval env e =
  match e with
  | Int _ | Bool _ | String _ | Unit | Fun _ -> e
  | Val x -> (
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
  | Rec _ -> raise (EvaluationError "Rec not fully implemented")
