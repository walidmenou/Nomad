open Ast
open Pretty

exception TypeError of string

type t = (int * typ) list

let rec apply s t =
  match t with
  | TInt | TBool | TString | TUnit -> t
  | TVar v -> (
      match List.assoc_opt v s with Some t' -> apply s t' | None -> t)
  | TArrow (t1, t2) -> TArrow (apply s t1, apply s t2)
  | TList t' -> TList (apply s t')
  | TArray t' -> TArray (apply s t')
  | TGrid t' -> TGrid (apply s t')
  | TTuple ts -> TTuple (List.map (apply s) ts)
  | TCon (n, ts) -> TCon (n, List.map (apply s) ts)

let compose s1 s2 = s1 @ List.map (fun (v, t) -> (v, apply s1 t)) s2

let rec unify t1 t2 =
  let rec occurs v = function
    | TVar v' -> v = v'
    | TArrow (t1, t2) -> occurs v t1 || occurs v t2
    | TList t | TArray t | TGrid t -> occurs v t
    | TTuple ts | TCon (_, ts) -> List.exists (occurs v) ts
    | _ -> false
  in
  match (t1, t2) with
  | t1, t2 when t1 = t2 -> []
  | TVar v, t | t, TVar v ->
      if occurs v t then raise (TypeError "Recursive types not supported")
      else [ (v, t) ]
  | TArrow (l1, r1), TArrow (l2, r2) ->
      let s = unify l1 l2 in
      compose (unify (apply s r1) (apply s r2)) s
  | TList t1, TList t2 | TArray t1, TArray t2 | TGrid t1, TGrid t2 ->
      unify t1 t2
  | TCon (n1, ts1), TCon (n2, ts2)
    when n1 = n2 && List.length ts1 = List.length ts2 ->
      List.fold_left2
        (fun s a b -> compose (unify (apply s a) (apply s b)) s)
        [] ts1 ts2
  | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
      List.fold_left2
        (fun s a b -> compose (unify (apply s a) (apply s b)) s)
        [] ts1 ts2
  | _ ->
      raise
        (TypeError
           ("Type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
