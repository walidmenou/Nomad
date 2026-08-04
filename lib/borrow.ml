open Ast

exception BorrowError of string

let rec spine e args =
  match e with App (f, a) -> spine f (a :: args) | _ -> (e, args)

let rec base e =
  match e with Index (a, _) -> base a | Var x -> Some x | _ -> None

let rec params e =
  match e with
  | Fun (p, b) ->
      let ps, body = params b in
      (p :: ps, body)
  | _ -> ([], e)

let consumes cs = List.exists (fun c -> c) cs

let needed cs =
  let rec last i = function
    | [] -> 0
    | c :: rest ->
        if c then max (i + 1) (last (i + 1) rest) else last (i + 1) rest
  in
  last 0 cs

let live dead x =
  match List.assoc_opt x dead with
  | Some why -> raise (BorrowError (x ^ " was already given to " ^ why))
  | None -> ()

let join d1 d2 = d1 @ List.filter (fun (x, _) -> not (List.mem_assoc x d1)) d2

let rec walk sigs dead e =
  match e with
  | Int _ | Bool _ | String _ | Unit -> dead
  | Var x ->
      live dead x;
      (match List.assoc_opt x sigs with
      | Some cs when consumes cs ->
          raise
            (BorrowError
               (x
              ^ " updates an array it is given, so it has to be called with \
                 all of its arguments"))
      | _ -> ());
      dead
  | Index (a, i) -> walk sigs (walk sigs dead a) i
  | Update (a, i, v) ->
      let dead = walk sigs (walk sigs (walk sigs dead a) i) v in
      consume dead a "an update"
  | BinOp (a, _, b) -> walk sigs (walk sigs dead a) b
  | If (c, a, b) ->
      let dead = walk sigs dead c in
      join (walk sigs dead a) (walk sigs dead b)
  | App _ -> apply sigs dead e
  | Fun _ ->
      shut sigs e
        "a function written where a value is expected cannot update an array";
      dead
  | Let (x, e1, e2) ->
      let sigs, dead = local sigs dead x e1 in
      forget (walk sigs dead e2) x
  | Rec (f, e1, e2) ->
      let sigs = (f, recursive sigs f e1) :: List.remove_assoc f sigs in
      walk sigs (List.remove_assoc f dead) e2
  | List es | Tuple es -> List.fold_left (walk sigs) dead es
  | Range (a, st, b) ->
      let dead = walk sigs dead a in
      let dead = match st with Some e -> walk sigs dead e | None -> dead in
      walk sigs dead b
  | Match (e, cases) ->
      let dead = walk sigs dead e in
      List.fold_left
        (fun acc (p, body) ->
          let ns = names p in
          let out = walk sigs (List.fold_left forget dead ns) body in
          join acc (List.fold_left forget out ns))
        [] cases
  | Comp (body, qs) -> (
      let target =
        match body with Update (arr, _, _) -> base arr | _ -> None
      in
      let inner = List.fold_left (qual sigs) [] qs in
      let inner = walk sigs inner body in
      List.iter
        (fun (y, _) ->
          if Some y <> target then
            raise
              (BorrowError
                 (y
                ^ " is updated by a comprehension but is not the array it \
                   produces")))
        inner;
      let dead = List.fold_left (qual sigs) dead qs in
      (match target with
      | Some x when List.mem x (List.concat_map qual_names qs) ->
          raise
            (BorrowError
               (x
              ^ " is bound by the comprehension that updates it, so there is \
                 nothing for it to produce"))
      | _ -> ());
      match body with
      | Update (arr, _, _) -> consume dead arr "a comprehension"
      | _ -> dead)

and qual sigs dead q =
  match q with
  | Gen (p, src) -> List.fold_left forget (walk sigs dead src) (names p)
  | Guard e -> walk sigs dead e
  | QLet (x, e) -> forget (walk sigs dead e) x

and forget dead x = List.remove_assoc x dead

and qual_names q =
  match q with Gen (p, _) -> names p | Guard _ -> [] | QLet (x, _) -> [ x ]

and names p =
  match p with
  | PatVar x -> [ x ]
  | PatCons (a, b) -> names a @ names b
  | PatTuple ps | PatCon (_, ps) -> List.concat_map names ps
  | _ -> []

and consume dead e why =
  match base e with Some x -> (x, why) :: dead | None -> dead

and apply sigs dead e =
  let head, args = spine e [] in
  match head with
  | Var f when List.mem_assoc f sigs ->
      live dead f;
      let cs = List.assoc f sigs in
      if List.length args < needed cs then
        raise
          (BorrowError
             (f
            ^ " updates an array it is given, so it has to be called with all \
               of its arguments"));
      let dead = List.fold_left (walk sigs) dead args in
      List.fold_left
        (fun d (c, a) -> if c then consume d a f else d)
        dead
        (List.filteri (fun i _ -> i < List.length cs) args
        |> List.mapi (fun i a -> (List.nth cs i, a)))
  | _ -> List.fold_left (walk sigs) (walk sigs dead head) args

and local sigs dead x e1 =
  match e1 with
  | Fun _ -> ((x, signature sigs e1) :: List.remove_assoc x sigs, forget dead x)
  | _ ->
      let dead = walk sigs dead e1 in
      (List.remove_assoc x sigs, forget dead x)

and signature sigs e =
  let ps, body = params e in
  let dead = walk sigs [] body in
  List.iter
    (fun (x, _) ->
      if not (List.mem x ps) then
        raise
          (BorrowError (x ^ " is updated inside a function but bound outside it")))
    dead;
  List.map (fun p -> List.mem_assoc p dead) ps

and recursive sigs f e =
  let ps, _ = params e in
  let rec fix cur =
    let next = signature ((f, cur) :: List.remove_assoc f sigs) e in
    if next = cur then cur else fix next
  in
  fix (List.map (fun _ -> false) ps)

and shut sigs e why =
  if walk sigs [] (snd (params e)) <> [] then raise (BorrowError why)

let check_stmt (sigs, dead) stmt =
  match stmt with
  | TypeStmt _ -> (sigs, dead)
  | ExprStmt e -> (sigs, walk sigs dead e)
  | LetStmt (x, e) -> local sigs dead x e
  | RecStmt (f, e) ->
      ((f, recursive sigs f e) :: List.remove_assoc f sigs, forget dead f)

let check_program stmts = ignore (List.fold_left check_stmt ([], []) stmts)
