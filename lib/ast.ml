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

type uop = Negate

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
