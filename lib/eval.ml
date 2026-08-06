open Ast
open Pretty

exception EvaluationError of string

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VArray of value array
  | VGrid of int * value array
  | VTuple of value list
  | VClos of ident * expr * env
  | VRecClos of ident * ident * expr * env
  | VCon of ident * int * value list
  | VBuiltin of ident * int * value list

and env = (ident * value) list

let values =
  List.map
    (fun (name, n, _) -> (name, VBuiltin (name, n, [])))
    Builtin.signatures

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
  | PatTuple ps, VTuple vs when List.length ps = List.length vs -> all ps vs
  | PatCon (c, ps), VCon (c', _, vs)
    when c = c' && List.length ps = List.length vs ->
      all ps vs
  | _, _ -> None

and all ps vs =
  List.fold_left2
    (fun acc p v ->
      match (acc, match_pattern p v) with
      | Some b, Some b' -> Some (b @ b')
      | _ -> None)
    (Some []) ps vs

let rec lookup env x =
  match env with
  | (y, v) :: rest -> if String.equal x y then v else lookup rest x
  | [] -> raise (EvaluationError ("Unbound Variable: " ^ x))

let rec eval env e =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | String s -> VString s
  | Unit -> VUnit
  | Fun (id, body) -> VClos (id, body, env)
  | Var x -> lookup env x
  | BinOp (e1, And, e2) -> if truth env e1 then eval env e2 else VBool false
  | BinOp (e1, Or, e2) -> if truth env e1 then VBool true else eval env e2
  | BinOp (e1, op, e2) -> binop op (eval env e1) (eval env e2)
  | Let (id, e1, e2) -> eval ((id, eval env e1) :: env) e2
  | Rec (id, e1, e2) -> eval ((id, rec_value env id e1) :: env) e2
  | If (cond, e1, e2) -> if truth env cond then eval env e1 else eval env e2
  | App (e1, e2) -> apply (eval env e1) (eval env e2)
  | List exprs -> VList (List.map (eval env) exprs)
  | Tuple exprs -> VTuple (List.map (eval env) exprs)
  | Comp ((Update (arr, _, _) as body), qs) ->
      let target = eval env (root arr) in
      sweep env qs body;
      target
  | Comp (body, qs) -> VList (List.rev (comp env qs body []))
  | Range (e1, st, e2) -> (
      let step =
        match st with
        | None -> 1
        | Some e -> (
            match (eval env e1, eval env e) with
            | VInt lo, VInt nxt -> nxt - lo
            | _ -> raise (EvaluationError "A range needs integers"))
      in
      if step = 0 then raise (EvaluationError "A range cannot stand still")
      else
        match (eval env e1, eval env e2) with
        | VInt lo, VInt hi ->
            let rec go i acc =
              if (step > 0 && i > hi) || (step < 0 && i < hi) then List.rev acc
              else go (i + step) (VInt i :: acc)
            in
            VList (go lo [])
        | _ -> raise (EvaluationError "A range needs integers"))
  | Index (Index (g, i), j) -> (
      match (eval env g, eval env i, eval env j) with
      | VGrid (w, cells), VInt i, VInt j -> cells.(cell w cells i j)
      | _ -> raise (EvaluationError "A grid is indexed by two integers"))
  | Update (Index (g, i), j, v) -> (
      match (eval env g, eval env i, eval env j) with
      | (VGrid (w, cells) as g), VInt i, VInt j ->
          cells.(cell w cells i j) <- eval env v;
          g
      | _ -> raise (EvaluationError "A grid is indexed by two integers"))
  | Index (arr, i) -> (
      match (eval env arr, eval env i) with
      | VArray a, VInt i ->
          bounds a i;
          a.(i)
      | _ -> raise (EvaluationError "Indexing needs an array and an integer"))
  | Update (arr, i, v) -> (
      match (eval env arr, eval env i) with
      | VArray a, VInt i ->
          bounds a i;
          a.(i) <- eval env v;
          VArray a
      | _ -> raise (EvaluationError "Indexing needs an array and an integer"))
  | Match (e, cases) -> try_match env cases (eval env e)

and apply f arg =
  match f with
  | VClos (id, body, env) -> eval ((id, arg) :: env) body
  | VRecClos (name, id, body, env) -> eval ((id, arg) :: (name, f) :: env) body
  | VCon (name, n, vs) -> VCon (name, n, vs @ [ arg ])
  | VBuiltin (name, n, vs) ->
      let vs = vs @ [ arg ] in
      if List.length vs = n then builtin name vs else VBuiltin (name, n, vs)
  | _ -> raise (EvaluationError "Application of non-function")

and root e = match e with Index (a, _) -> root a | _ -> e

and cell w cells i j =
  if i < 0 || j < 0 || j >= w || (i * w) + j >= Array.length cells then
    raise (EvaluationError "Index out of bounds")
  else (i * w) + j

and bounds a i =
  if i < 0 || i >= Array.length a then
    raise (EvaluationError "Index out of bounds")

and builtin name vs =
  match (name, vs) with
  | "array", [ VInt n; v ] ->
      if n < 0 then
        raise (EvaluationError "An array needs a length of zero or more")
      else VArray (Array.make n v)
  | "size", [ VArray a ] -> VInt (Array.length a)
  | "copy", [ VArray a ] -> VArray (Array.copy a)
  | "from_list", [ VList vs ] -> VArray (Array.of_list vs)
  | "grid", [ VInt r; VInt c; v ] ->
      if r < 0 || c < 0 then
        raise (EvaluationError "A grid needs sides of zero or more")
      else VGrid (c, Array.make (r * c) v)
  | "rows", [ VGrid (c, cells) ] ->
      VInt (if c = 0 then 0 else Array.length cells / c)
  | "cols", [ VGrid (c, _) ] -> VInt c
  | _ -> raise (EvaluationError ("Bad application of " ^ name))

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

and sweep env qs body =
  match qs with
  | [] -> ignore (eval env body)
  | Gen (p, src) :: rest -> (
      match eval env src with
      | VList vs ->
          List.iter
            (fun v ->
              match match_pattern p v with
              | Some bindings -> sweep (bindings @ env) rest body
              | None -> ())
            vs
      | _ -> raise (EvaluationError "A generator needs a list"))
  | Guard e :: rest -> if truth env e then sweep env rest body
  | QLet (id, e) :: rest -> sweep ((id, eval env e) :: env) rest body

and truth env e =
  match eval env e with
  | VBool b -> b
  | _ -> raise (EvaluationError "Expected a boolean")

and binop op v1 v2 =
  match (op, v1, v2) with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Sub, VInt a, VInt b -> VInt (a - b)
  | Mul, VInt a, VInt b -> VInt (a * b)
  | (Div | Mod), VInt _, VInt 0 -> raise (EvaluationError "Division by zero")
  | Div, VInt a, VInt b -> VInt (a / b)
  | Mod, VInt a, VInt b -> VInt (a mod b)
  | Less, VInt a, VInt b -> VBool (a < b)
  | Leq, VInt a, VInt b -> VBool (a <= b)
  | Greater, VInt a, VInt b -> VBool (a > b)
  | Geq, VInt a, VInt b -> VBool (a >= b)
  | Cons, v, VList vs -> VList (v :: vs)
  | Append, VList a, VList b -> VList (a @ b)
  | Equal, _, _ -> VBool (equal v1 v2)
  | Diff, _, _ -> VBool (not (equal v1 v2))
  | _ -> raise (EvaluationError "Type mismatch in binary operation")

and equal v1 v2 =
  match (v1, v2) with
  | (VClos _ | VRecClos _ | VBuiltin _), _
  | _, (VClos _ | VRecClos _ | VBuiltin _) ->
      raise (EvaluationError "Cannot compare functions")
  | VGrid (w1, a), VGrid (w2, b) -> w1 = w2 && equal (VArray a) (VArray b)
  | VArray a, VArray b ->
      Array.length a = Array.length b
      && List.for_all2 equal (Array.to_list a) (Array.to_list b)
  | VCon (n1, _, vs1), VCon (n2, _, vs2) ->
      n1 = n2
      && List.length vs1 = List.length vs2
      && List.for_all2 equal vs1 vs2
  | (VList vs1 | VTuple vs1), (VList vs2 | VTuple vs2) ->
      List.length vs1 = List.length vs2 && List.for_all2 equal vs1 vs2
  | _ -> v1 = v2

let eval_stmt env stmt =
  match stmt with
  | TypeStmt (_, _, cons) ->
      let bind (c, args) = (c, VCon (c, List.length args, [])) in
      (List.map bind cons @ env, VUnit)
  | ExprStmt e -> (env, eval env e)
  | LetStmt (p, e) -> (
      let v = eval env e in
      match match_pattern p v with
      | Some bindings -> (bindings @ env, v)
      | None -> raise (EvaluationError "Match failure"))
  | RecStmt (id, e) ->
      let v = rec_value env id e in
      ((id, v) :: env, v)

let rec show = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VList vs -> "[" ^ String.concat "; " (List.map show vs) ^ "]"
  | VArray a -> "{" ^ String.concat "; " (List.map show (Array.to_list a)) ^ "}"
  | VGrid (w, cells) ->
      let n = if w = 0 then 0 else Array.length cells / w in
      let row i = show (VArray (Array.sub cells (i * w) w)) in
      "{" ^ String.concat "; " (List.init n row) ^ "}"
  | VTuple vs -> "(" ^ String.concat ", " (List.map show vs) ^ ")"
  | VCon (name, _, []) -> name
  | VCon (name, _, vs) -> String.concat " " (name :: List.map nested vs)
  | VClos _ | VRecClos _ | VBuiltin _ -> "<fun>"

and nested v =
  match v with VCon (_, _, _ :: _) -> "(" ^ show v ^ ")" | _ -> show v

let show_val ty v =
  match v with
  | VClos _ | VRecClos _ -> "<fun> : " ^ display_typ ty
  | (VCon (_, n, vs) | VBuiltin (_, n, vs)) when List.length vs < n ->
      "<fun> : " ^ display_typ ty
  | _ -> show v

let show_program types stmts =
  let step (env, out, ts) stmt =
    let env, v = eval_stmt env stmt in
    match (stmt, ts) with
    | TypeStmt _, _ -> (env, out, ts)
    | _, t :: rest -> (env, show_val t v :: out, rest)
    | _, [] -> (env, out, [])
  in
  let _, out, _ = List.fold_left step (values, [], types) stmts in
  List.rev out

let eval_program types stmts =
  List.iter print_endline (show_program types stmts)
