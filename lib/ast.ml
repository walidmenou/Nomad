type ident = string
type literal = Int of int | Bool of bool | String of string | Unit
type typ = Int | Bool | Unit | Comma | Arrow
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

type pattern =
  | PatWildcard
  | PatVar of ident
  | PatInt of int
  | PatBool of bool
  | PatString of string

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
