type ident = string
type literal = Int of int | Bool of bool | String of string | Unit

type typ =
  | TInt
  | TBool
  | TString
  | TUnit
  | TArrow of typ * typ
  | TList of typ
  | TVar of int

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | Diff
  | And
  | Or
  | Less
  | Leq
  | Geq
  | Greater
  | Cons

type pattern =
  | PatWildcard
  | PatVar of ident
  | PatInt of int
  | PatBool of bool
  | PatString of string
  | PatCons of pattern * pattern
  | PatNil

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
  | Var of ident
  | BinOp of expr * binary_op * expr
  | Let of ident * expr * expr
  | Rec of ident * expr * expr
  | If of expr * expr * expr
  | Fun of ident * expr
  | App of expr * expr
  | List of expr list
  | Match of expr * (pattern * expr) list

type statement =
  | LetStmt of ident * expr
  | RecStmt of ident * expr
  | ExprStmt of expr

type program = statement list

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TArrow (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TList t -> string_of_typ t ^ " list"
  | TVar v -> "'a" ^ string_of_int v

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "="
  | Diff -> "<>"
  | And -> "&&"
  | Or -> "||"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Cons -> "::"

let rec string_of_pat = function
  | PatWildcard -> "_"
  | PatVar id -> id
  | PatInt i -> string_of_int i
  | PatBool b -> string_of_bool b
  | PatString s -> "\"" ^ s ^ "\""
  | PatNil -> "[]"
  | PatCons (p1, p2) -> string_of_pat p1 ^ " :: " ^ string_of_pat p2

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
  | Match (e, cases) ->
      let case (pat, body) = string_of_pat pat ^ " -> " ^ string_of_expr body in
      "match " ^ string_of_expr e ^ " with "
      ^ String.concat " | " (List.map case cases)

and atom e =
  match e with
  | Int _ | Bool _ | String _ | Unit | Var _ | List _ -> string_of_expr e
  | _ -> "(" ^ string_of_expr e ^ ")"
