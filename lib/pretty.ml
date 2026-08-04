open Ast

let rec typ_with name = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TArrow (t1, t2) -> "(" ^ typ_with name t1 ^ " -> " ^ typ_with name t2 ^ ")"
  | TList t -> typ_with name t ^ " list"
  | TArray t -> typ_with name t ^ " array"
  | TGrid t -> typ_with name t ^ " grid"
  | TTuple ts -> "(" ^ String.concat " * " (List.map (typ_with name) ts) ^ ")"
  | TCon (n, ts) -> String.concat " " (List.map (typ_with name) ts @ [ n ])
  | TVar v -> name v

let string_of_typ t = typ_with (fun v -> "'a" ^ string_of_int v) t

let display_typ t =
  let rec order acc = function
    | TVar v -> if List.mem v acc then acc else acc @ [ v ]
    | TArrow (t1, t2) -> order (order acc t1) t2
    | TList t | TArray t | TGrid t -> order acc t
    | TTuple ts | TCon (_, ts) -> List.fold_left order acc ts
    | _ -> acc
  in
  let names = List.mapi (fun i v -> (v, i)) (order [] t) in
  let name v =
    let i = List.assoc v names in
    let letter = Char.chr (Char.code 'a' + (i mod 26)) in
    if i < 26 then Printf.sprintf "'%c" letter
    else Printf.sprintf "'%c%d" letter (i / 26)
  in
  let rec show = function
    | TArrow (t1, t2) -> arg t1 ^ " -> " ^ show t2
    | TList t -> arg t ^ " list"
    | TArray t -> arg t ^ " array"
    | TGrid t -> arg t ^ " grid"
    | TTuple ts -> "(" ^ String.concat " * " (List.map show ts) ^ ")"
    | TCon (n, ts) -> String.concat " " (List.map arg ts @ [ n ])
    | t -> typ_with name t
  and arg = function TArrow _ as t -> "(" ^ show t ^ ")" | t -> show t in
  show t

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "="
  | Diff -> "<>"
  | And -> "&&"
  | Or -> "||"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Cons -> "::"
  | Append -> "@"

let rec string_of_pat = function
  | PatWildcard -> "_"
  | PatVar id -> id
  | PatInt i -> string_of_int i
  | PatBool b -> string_of_bool b
  | PatString s -> "\"" ^ s ^ "\""
  | PatNil -> "[]"
  | PatCons (p1, p2) -> string_of_pat p1 ^ " :: " ^ string_of_pat p2
  | PatTuple ps -> "(" ^ String.concat ", " (List.map string_of_pat ps) ^ ")"
  | PatCon (c, []) -> c
  | PatCon (c, ps) -> c ^ " " ^ String.concat " " (List.map nested_pat ps)

and nested_pat p =
  match p with
  | PatCon (_, _ :: _) | PatCons _ -> "(" ^ string_of_pat p ^ ")"
  | _ -> string_of_pat p

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Unit -> "()"
  | Var x -> x
  | BinOp (e1, op, e2) -> atom e1 ^ " " ^ string_of_binop op ^ " " ^ atom e2
  | If (cond, e1, e2) ->
      "if " ^ string_of_expr cond ^ " then " ^ string_of_expr e1 ^ " else "
      ^ string_of_expr e2
  | Fun (id, body) -> "fun " ^ id ^ " -> " ^ string_of_expr body
  | App (e1, e2) -> atom e1 ^ " " ^ atom e2
  | Let (id, e1, e2) ->
      "let " ^ id ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | Rec (id, e1, e2) ->
      "let rec " ^ id ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | List exprs -> "[" ^ String.concat "; " (List.map string_of_expr exprs) ^ "]"
  | Tuple es -> "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | Range (e1, st, e2) ->
      let mid =
        match st with Some e -> ", " ^ string_of_expr e | None -> ""
      in
      "[" ^ string_of_expr e1 ^ mid ^ ".." ^ string_of_expr e2 ^ "]"
  | Index (a, i) -> atom a ^ "[" ^ string_of_expr i ^ "]"
  | Update (a, i, v) ->
      atom a ^ "[" ^ string_of_expr i ^ "] := " ^ string_of_expr v
  | Comp (e, qs) ->
      "[" ^ string_of_expr e ^ " | "
      ^ String.concat ", " (List.map string_of_qual qs)
      ^ "]"
  | Match (e, cases) ->
      let case (pat, body) = string_of_pat pat ^ " -> " ^ string_of_expr body in
      "match " ^ string_of_expr e ^ " with "
      ^ String.concat " | " (List.map case cases)

and string_of_qual = function
  | Gen (p, e) -> string_of_pat p ^ " <- " ^ string_of_expr e
  | Guard e -> string_of_expr e
  | QLet (id, e) -> "let " ^ id ^ " = " ^ string_of_expr e

and atom e =
  match e with
  | Int _ | Bool _ | String _ | Unit | Var _ | List _ | Tuple _ | Range _
  | Comp _ ->
      string_of_expr e
  | _ -> "(" ^ string_of_expr e ^ ")"
