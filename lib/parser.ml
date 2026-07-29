open Util
open Ast

type 'a t = char list -> ('a * char list) option

let keywords = [ "let"; "in"; "fun"; "if"; "then"; "else"; "match"; "with" ]
let return x = fun input -> Some (x, input)
let none = fun _ -> None

let ( |*> ) p k =
 fun input -> match p input with Some (x, rest) -> (k x) rest | _ -> None

let ( <|> ) p q = fun input -> match p input with None -> q input | r -> r
let ( |>> ) p q = p |*> fun _ -> q

let ( <<| ) p q =
  p |*> fun x ->
  q |*> fun _ -> return x

let satisfies p =
 fun input ->
  match input with [] -> None | c :: cs -> if p c then Some (c, cs) else None

let char c = satisfies (fun x -> x = c)

let digit =
  satisfies (function '0' .. '9' -> true | _ -> false) |*> fun c ->
  return (Char.code c - Char.code '0')

let rec many (p : 'a t) : 'a list t = some p <|> return []

and some (q : 'a t) : 'a list t =
  q |*> fun x ->
  many q |*> fun xs -> return (x :: xs)

let map f p = p |*> fun r -> return (f r)
let maybe p = map (fun r -> Some r) p <|> return None

let sepby sep p =
  p
  |*> (fun first -> many (sep |>> p) |*> fun rest -> return (first :: rest))
  <|> return []

let rec skip_comment = function
  | '\n' :: _ as cs -> cs
  | _ :: cs -> skip_comment cs
  | [] -> []

let rec spaces input =
  match input with
  | (' ' | '\t' | '\n') :: cs -> spaces cs
  | '-' :: '-' :: cs -> spaces (skip_comment cs)
  | _ -> Some ((), input)

let rec inline_spaces input =
  match input with
  | (' ' | '\t') :: cs -> inline_spaces cs
  | '-' :: '-' :: cs -> inline_spaces (skip_comment cs)
  | '\n' :: cs when indented cs -> inline_spaces cs
  | _ -> Some ((), input)

and indented cs = line_start cs false

and line_start cs seen =
  match cs with
  | (' ' | '\t') :: rest -> line_start rest true
  | '\n' :: rest -> line_start rest false
  | '-' :: '-' :: rest -> line_start (skip_comment rest) false
  | [] -> false
  | _ -> seen

let token p = p <<| inline_spaces

let nat =
  some digit |*> fun xs ->
  return (List.fold_left (fun acc x -> (acc * 10) + x) 0 xs)

let int = char '-' |>> nat |*> (fun x -> return (-x)) <|> nat
let natural = token nat
let integer = token int

let keyword s =
  let rec check = function [] -> return () | c :: cs -> char c |>> check cs in

  let is_alphanum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in

  let needs_boundary =
    String.length s > 0 && is_alphanum s.[String.length s - 1]
  in

  check (explode s) |*> fun () ->
  fun input ->
   match input with
   | c :: _ when needs_boundary && is_alphanum c -> None
   | _ -> token (return ()) input

let between p1 p2 p = p1 |>> p <<| p2
let parenthesized p = between (token (char '(')) (token (char ')')) p
let alpha = satisfies (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

let alphanumeric =
  satisfies (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false)

let ident =
  token
    ( alpha |*> fun x ->
      many alphanumeric |*> fun xs ->
      let s = ltos (x :: xs) in
      if List.mem s keywords then none else return s )

let int_lit = natural |*> fun x -> return (Int x)

let boolean =
  keyword "true" |>> return true <|> (keyword "false" |>> return false)

let bool_lit = boolean |*> fun x -> return (Bool x)
let unit_lit = keyword "()" |>> return Unit

let string =
  between (char '"') (char '"') (many (satisfies (fun c -> c <> '"')))
  |*> fun cs -> return (ltos cs)

let string_lit = token (string |*> fun s -> return (String s))
let var_expr = ident |*> fun x -> return (Var x)
let lit_expr = int_lit <|> bool_lit <|> unit_lit <|> string_lit

let addop =
  keyword "+" |*> (fun _ -> return Add) <|> (keyword "-" |*> fun _ -> return Sub)

let mulop =
  keyword "*" |*> (fun _ -> return Mul) <|> (keyword "/" |*> fun _ -> return Div)

let orop = keyword "||" |*> fun _ -> return Or
let andop = keyword "&&" |*> fun _ -> return And

let cmpop =
  keyword "<="
  |*> (fun _ -> return Leq)
  <|> (keyword ">=" |*> fun _ -> return Geq)
  <|> (keyword "<>" |*> fun _ -> return Diff)
  <|> (keyword "<" |*> fun _ -> return Less)
  <|> (keyword ">" |*> fun _ -> return Greater)
  <|> (keyword "=" |*> fun _ -> return Equal)

let consop = keyword "::" |*> fun _ -> return Cons
let arrow = keyword "->"

let chain_left op_p exp_p =
  exp_p |*> fun r ->
  many
    ( op_p |*> fun op ->
      exp_p |*> fun exp -> return (op, exp) )
  |*> fun rs ->
  return (List.fold_left (fun acc (op, exp) -> BinOp (acc, op, exp)) r rs)

let rec chain_right op_p exp_p input =
  ( exp_p |*> fun first ->
    op_p
    |*> (fun op ->
    chain_right op_p exp_p |*> fun rest -> return (BinOp (first, op, rest)))
    <|> return first )
    input

let cmp_helper first pairs =
  let cmps, _ =
    List.fold_left
      (fun (cmps, prev) (op, exp) -> (BinOp (prev, op, exp) :: cmps, exp))
      ([], first) pairs
  in
  match List.rev cmps with
  | [] -> first
  | c :: cs -> List.fold_left (fun acc cmp -> BinOp (acc, And, cmp)) c cs

let chain_cmps op_p exp_p =
  exp_p |*> fun first ->
  many
    ( op_p |*> fun op ->
      exp_p |*> fun exp -> return (op, exp) )
  |*> fun pairs -> return (cmp_helper first pairs)

let pipe = keyword "|>"
let curry params body = List.fold_right (fun p acc -> Fun (p, acc)) params body
let nil_pat = keyword "[]" |>> return PatNil
let wildcard_pat = keyword "_" |>> return PatWildcard
let var_pat = ident |*> fun x -> return (PatVar x)
let int_pat = integer |*> fun i -> return (PatInt i)
let bool_pat = boolean |*> fun b -> return (PatBool b)
let string_pat = token (string |*> fun s -> return (PatString s))

let atom_pat =
  nil_pat <|> wildcard_pat <|> bool_pat <|> var_pat <|> int_pat <|> string_pat

let rec chain_right_pat op_p exp_p input =
  ( exp_p |*> fun first ->
    op_p
    |*> (fun _ ->
    chain_right_pat op_p exp_p |*> fun rest -> return (PatCons (first, rest)))
    <|> return first )
    input

let pattern input = chain_right_pat (keyword "::") atom_pat input

let rec expr input =
  (if_expr <|> fun_expr <|> let_rec_expr <|> let_expr <|> match_expr <|> or_expr)
    input

and or_expr input = chain_left orop and_expr input
and and_expr input = chain_left andop cmp_expr input
and cmp_expr input = chain_cmps cmpop pipe_expr input
and cons_expr input = chain_right consop add_expr input
and add_expr input = chain_left addop mul_expr input
and mul_expr input = chain_left mulop neg_expr input

and neg_expr input =
  (keyword "-" |>> neg_expr
  |*> (fun e -> return (BinOp (Int 0, Sub, e)))
  <|> app_expr)
    input

and pipe_expr input =
  ( cons_expr |*> fun first ->
    many (pipe |>> cons_expr) |*> fun rest ->
    return (List.fold_left (fun acc f -> App (f, acc)) first rest) )
    input

and app_expr input =
  ( atom_expr |*> fun f ->
    many atom_expr |*> fun args ->
    return (List.fold_left (fun acc arg -> App (acc, arg)) f args) )
    input

and range_body input =
  ( expr |*> fun lo ->
    keyword ".." |>> expr |*> fun hi -> return (Range (lo, hi)) )
    input

and list_body input =
  (sepby (token (char ';')) expr |*> fun exprs -> return (List exprs)) input

and list_expr input =
  between (token (char '[')) (token (char ']')) (range_body <|> list_body) input

and atom_expr input =
  (lit_expr <|> var_expr <|> list_expr <|> parenthesized expr) input

and if_expr input =
  ( keyword "if" |>> expr |*> fun exp1 ->
    keyword "then" |>> expr |*> fun exp2 ->
    keyword "else" |>> expr |*> fun exp3 -> return (If (exp1, exp2, exp3)) )
    input

and fun_expr input =
  ( keyword "fun" |>> some ident |*> fun params ->
    arrow |>> expr |*> fun exp -> return (curry params exp) )
    input

and let_expr input =
  ( keyword "let" |>> ident |*> fun id ->
    many ident |*> fun params ->
    keyword "=" |>> expr |*> fun exp1 ->
    keyword "in" |>> expr |*> fun exp2 ->
    return (Let (id, curry params exp1, exp2)) )
    input

and match_expr input =
  ( keyword "match" |>> expr |*> fun exp ->
    keyword "with"
    |>> sepby (keyword "|")
          ( pattern <<| arrow |*> fun p ->
            expr |*> fun exp -> return (p, exp) )
    |*> fun pairs -> return (Match (exp, pairs)) )
    input

and let_rec_expr input =
  ( keyword "let" |>> keyword "rec" |>> ident |*> fun id ->
    many ident |*> fun params ->
    keyword "=" |>> expr |*> fun exp1 ->
    keyword "in" |>> expr |*> fun exp2 ->
    return (Rec (id, curry params exp1, exp2)) )
    input

let let_stmt input =
  ( keyword "let" |>> ident |*> fun id ->
    many ident |*> fun params ->
    keyword "=" |>> expr |*> fun exp -> return (LetStmt (id, curry params exp))
  )
    input

let rec_stmt input =
  ( keyword "let" |>> keyword "rec" |>> ident |*> fun id ->
    many ident |*> fun params ->
    keyword "=" |>> expr |*> fun exp -> return (RecStmt (id, curry params exp))
  )
    input

let expr_stmt input = (expr |*> fun exp -> return (ExprStmt exp)) input
let statement input = (rec_stmt <|> let_stmt <|> expr_stmt) input

let program input =
  let sep = maybe (keyword ";;") |>> spaces in
  (spaces |>> many (statement <<| sep)) input

let run p s =
  match p (explode s) with
  | Some (res, []) -> Ok res
  | Some (_, rest) -> Error ("Unparsed trailing input " ^ ltos rest)
  | _ -> Error "Parse error"
