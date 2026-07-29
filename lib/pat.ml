open Ast

type head = HNil | HCons | HInt of int | HBool of bool | HStr of string

let head_of = function
  | PatNil -> Some HNil
  | PatCons _ -> Some HCons
  | PatInt n -> Some (HInt n)
  | PatBool b -> Some (HBool b)
  | PatString s -> Some (HStr s)
  | PatWildcard | PatVar _ -> None

let sub_types h t =
  match (h, t) with
  | HCons, TList e -> [ e; TList e ]
  | HCons, _ -> [ TVar 0; t ]
  | _ -> []

let wildcards n = List.init n (fun _ -> PatWildcard)

let specialize h n rows =
  let row = function
    | PatCons (p1, p2) :: ps when h = HCons -> Some (p1 :: p2 :: ps)
    | (PatWildcard | PatVar _) :: ps -> Some (wildcards n @ ps)
    | p :: ps when head_of p = Some h -> Some ps
    | _ -> None
  in
  List.filter_map row rows

let default rows =
  let row = function (PatWildcard | PatVar _) :: ps -> Some ps | _ -> None in
  List.filter_map row rows

let heads rows =
  let head = function p :: _ -> head_of p | [] -> None in
  List.sort_uniq compare (List.filter_map head rows)

let complete t hs =
  match t with
  | TBool -> List.mem (HBool true) hs && List.mem (HBool false) hs
  | TList _ -> List.mem HNil hs && List.mem HCons hs
  | _ -> false

let rec useful types rows q =
  match (types, q) with
  | t :: ts, p :: qs -> (
      match head_of p with
      | Some h ->
          let sub = sub_types h t in
          let args = match p with PatCons (a, b) -> [ a; b ] | _ -> [] in
          useful (sub @ ts) (specialize h (List.length sub) rows) (args @ qs)
      | None ->
          let hs = heads rows in
          if complete t hs then
            List.exists
              (fun h ->
                let sub = sub_types h t in
                let n = List.length sub in
                useful (sub @ ts) (specialize h n rows) (wildcards n @ qs))
              hs
          else useful ts (default rows) qs)
  | _ -> rows = []

let exhaustive t pats =
  not (useful [ t ] (List.map (fun p -> [ p ]) pats) [ PatWildcard ])
