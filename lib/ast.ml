type ident = string
type literal = Int of int | Bool of bool | String of string | Unit

type typ =
  | TInt
  | TBool
  | TString
  | TUnit
  | TArrow of typ * typ
  | TList of typ
  | TTuple of typ list
  | TCon of ident * typ list
  | TVar of int

type type_expr =
  | TEVar of ident
  | TECon of ident * type_expr list
  | TETuple of type_expr list
  | TEArrow of type_expr * type_expr

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | Diff
  | And
  | Or
  | Less
  | Leq
  | Geq
  | Greater
  | Cons
  | Append

type pattern =
  | PatWildcard
  | PatVar of ident
  | PatInt of int
  | PatBool of bool
  | PatString of string
  | PatCons of pattern * pattern
  | PatTuple of pattern list
  | PatCon of ident * pattern list
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
  | Tuple of expr list
  | Range of expr * expr
  | Comp of expr * qualifier list
  | Match of expr * (pattern * expr) list

and qualifier = Gen of pattern * expr | Guard of expr | QLet of ident * expr

type statement =
  | LetStmt of ident * expr
  | RecStmt of ident * expr
  | TypeStmt of ident * ident list * (ident * type_expr list) list
  | ExprStmt of expr

type program = statement list
