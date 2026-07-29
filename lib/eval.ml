open Ast

exception EvaluationError of string

(* A function value carries the environment it was written in, which is what
   makes a free variable in its body mean what it meant there rather than
   whatever it happens to mean at the call. A recursive one also carries the
   name it calls itself by. *)
type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
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
  | BinOp (e1, op, e2) -> binop op (eval env e1) (eval env e2)
  | Let (id, e1, e2) -> eval ((id, eval env e1) :: env) e2
  | Rec (id, e1, e2) -> eval ((id, rec_value env id e1) :: env) e2
  | If (cond, e1, e2) -> (
      match eval env cond with
      | VBool true -> eval env e1
      | VBool false -> eval env e2
      | _ -> raise (EvaluationError "Condition must be a boolean"))
  | App (e1, e2) -> apply (eval env e1) (eval env e2)
  | List exprs -> VList (List.map (eval env) exprs)
  | Match (e, cases) -> try_match env cases (eval env e)

(* The body runs in the environment the function closed over, extended with the
   argument. A recursive function also puts its own name back in scope, which is
   what lets it call itself. *)
and apply f arg =
  match f with
  | VClos (id, body, env) -> eval ((id, arg) :: env) body
  | VRecClos (name, id, body, env) -> eval ((id, arg) :: (name, f) :: env) body
  | _ -> raise (EvaluationError "Application of non-function")

(* Only a function can refer to itself in its own definition, so anything else
   is evaluated the ordinary way. *)
and rec_value env id e =
  match e with
  | Fun (arg, body) -> VRecClos (id, arg, body, env)
  | _ -> eval env e

(* The first case whose pattern fits wins, and what it binds hides anything of
   the same name already in scope. *)
and try_match env cases v =
  match cases with
  | [] -> raise (EvaluationError "Match failure")
  | (pat, body) :: rest -> (
      match match_pattern pat v with
      | Some bindings -> eval (bindings @ env) body
      | None -> try_match env rest v)

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
  | And, VBool a, VBool b -> VBool (a && b)
  | Or, VBool a, VBool b -> VBool (a || b)
  | Cons, v, VList vs -> VList (v :: vs)
  | Equal, _, _ -> VBool (equal v1 v2)
  | Diff, _, _ -> VBool (not (equal v1 v2))
  | _ -> raise (EvaluationError "Type mismatch in binary operation")

(* Structural, so two values of the same type are equal when they are built the
   same way. Functions are the exception, since looking at the code cannot
   decide whether two of them agree. *)
and equal v1 v2 =
  match (v1, v2) with
  | (VClos _ | VRecClos _), _ | _, (VClos _ | VRecClos _) ->
      raise (EvaluationError "Cannot compare functions")
  | VList vs1, VList vs2 ->
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

let rec show_val = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VList vs -> "[" ^ String.concat "; " (List.map show_val vs) ^ "]"
  | VClos _ | VRecClos _ -> "<fun>"

(* The lines a program prints, in order. Kept separate from [eval_program] so a
   test can compare against them without capturing stdout. *)
let run_program stmts =
  let step (env, printed) stmt =
    let env, v = eval_stmt env stmt in
    (env, show_val v :: printed)
  in
  List.rev (snd (List.fold_left step ([], []) stmts))

let eval_program stmts = List.iter print_endline (run_program stmts)
