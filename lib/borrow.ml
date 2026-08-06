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

type env = { sigs : (ident * bool list) list; alias : (ident * ident) list }

let live dead x =
  match List.assoc_opt x dead with
  | Some why -> raise (BorrowError (x ^ " was already given to " ^ why))
  | None -> ()

let join d1 d2 = d1 @ List.filter (fun (x, _) -> not (List.mem_assoc x d1)) d2

let rec walk env dead e =
  match e with
  | Int _ | Bool _ | String _ | Unit -> dead
  | Var x ->
      live dead x;
      (match List.assoc_opt x env.sigs with
      | Some cs when consumes cs ->
          raise
            (BorrowError
               (x
              ^ " updates an array it is given, so it has to be called with \
                 all of its arguments"))
      | _ -> ());
      dead
  | Index (a, i) -> walk env (walk env dead a) i
  | Update (a, i, v) ->
      let dead = walk env (walk env (walk env dead a) i) v in
      consume env dead a "an update"
  | BinOp (a, _, b) -> walk env (walk env dead a) b
  | If (c, a, b) ->
      let dead = walk env dead c in
      join (walk env dead a) (walk env dead b)
  | App _ -> apply env dead e
  | Fun _ ->
      shut env e
        "a function written where a value is expected cannot update an array";
      dead
  | Let (x, e1, e2) ->
      let env, dead = local env dead x e1 in
      forget (walk env dead e2) x
  | Rec (f, e1, e2) ->
      let env =
        {
          env with
          sigs = (f, recursive env f e1) :: List.remove_assoc f env.sigs;
        }
      in
      walk env (List.remove_assoc f dead) e2
  | List es | Tuple es -> List.fold_left (walk env) dead es
  | Range (a, st, b) ->
      let dead = walk env dead a in
      let dead = match st with Some e -> walk env dead e | None -> dead in
      walk env dead b
  | Match (e, cases) ->
      let dead = walk env dead e in
      List.fold_left
        (fun acc (p, body) ->
          let ns = names p in
          let out = walk env (List.fold_left forget dead ns) body in
          join acc (List.fold_left forget out ns))
        [] cases
  | Comp (body, qs) -> (
      let target =
        match body with Update (arr, _, _) -> base arr | _ -> None
      in
      let inner = List.fold_left (qual env) [] qs in
      let inner = walk env inner body in
      List.iter
        (fun (y, _) ->
          if Some y <> target then
            raise
              (BorrowError
                 (y
                ^ " is updated by a comprehension but is not the array it \
                   produces")))
        inner;
      let dead = List.fold_left (qual env) dead qs in
      (match target with
      | Some x when List.mem x (List.concat_map qual_names qs) ->
          raise
            (BorrowError
               (x
              ^ " is bound by the comprehension that updates it, so there is \
                 nothing for it to produce"))
      | _ -> ());
      match body with
      | Update (arr, _, _) -> consume env dead arr "a comprehension"
      | _ -> dead)

and qual env dead q =
  match q with
  | Gen (p, src) -> List.fold_left forget (walk env dead src) (names p)
  | Guard e -> walk env dead e
  | QLet (x, e) -> forget (walk env dead e) x

and forget dead x = List.remove_assoc x dead

and qual_names q =
  match q with Gen (p, _) -> names p | Guard _ -> [] | QLet (x, _) -> [ x ]

and names p =
  match p with
  | PatVar x -> [ x ]
  | PatCons (a, b) -> names a @ names b
  | PatTuple ps | PatCon (_, ps) -> List.concat_map names ps
  | _ -> []

and consume env dead e why =
  match base e with
  | Some x -> List.map (fun y -> (y, why)) (related env x) @ dead
  | None -> dead

and related env x =
  let neighbours y =
    List.concat_map
      (fun (a, b) ->
        if String.equal a y then [ b ]
        else if String.equal b y then [ a ]
        else [])
      env.alias
  in
  let rec go seen = function
    | [] -> seen
    | y :: rest ->
        if List.mem y seen then go seen rest
        else go (y :: seen) (neighbours y @ rest)
  in
  go [] [ x ]

and roots env e =
  match e with
  | Var x -> [ x ]
  | If (_, a, b) -> roots env a @ roots env b
  | Let (x, _, b) -> List.filter (fun y -> not (String.equal y x)) (roots env b)
  | Rec (f, _, b) -> List.filter (fun y -> not (String.equal y f)) (roots env b)
  | Match (_, cases) ->
      List.concat_map
        (fun (p, b) ->
          let ns = names p in
          List.filter (fun y -> not (List.mem y ns)) (roots env b))
        cases
  | App _ -> (
      let head, args = spine e [] in
      match head with
      | Var f
        when List.exists (fun (n, _, _) -> String.equal n f) Builtin.signatures
        ->
          []
      | _ ->
          let cs =
            match head with
            | Var f -> Option.value ~default:[] (List.assoc_opt f env.sigs)
            | _ -> []
          in
          let moved i = i < List.length cs && List.nth cs i in
          List.concat_map (roots env)
            (List.filteri (fun i _ -> not (moved i)) args))
  | _ -> []

and apply env dead e =
  let head, args = spine e [] in
  match head with
  | Var f when List.mem_assoc f env.sigs ->
      live dead f;
      let cs = List.assoc f env.sigs in
      if List.length args < needed cs then
        raise
          (BorrowError
             (f
            ^ " updates an array it is given, so it has to be called with all \
               of its arguments"));
      let dead = List.fold_left (walk env) dead args in
      List.fold_left
        (fun d (c, a) -> if c then consume env d a f else d)
        dead
        (List.filteri (fun i _ -> i < List.length cs) args
        |> List.mapi (fun i a -> (List.nth cs i, a)))
  | _ -> List.fold_left (walk env) (walk env dead head) args

and local env dead x e1 =
  match e1 with
  | Fun _ ->
      ( { env with sigs = (x, signature env e1) :: List.remove_assoc x env.sigs },
        forget dead x )
  | _ ->
      let dead = walk env dead e1 in
      let kept =
        List.filter
          (fun (a, b) -> not (String.equal a x || String.equal b x))
          env.alias
      in
      ( {
          sigs = List.remove_assoc x env.sigs;
          alias = List.map (fun r -> (x, r)) (roots env e1) @ kept;
        },
        forget dead x )

and signature env e =
  let ps, body = params e in
  let dead = walk env [] body in
  List.iter
    (fun (x, _) ->
      if not (List.mem x ps) then
        raise
          (BorrowError (x ^ " is updated inside a function but bound outside it")))
    dead;
  List.map (fun p -> List.mem_assoc p dead) ps

and recursive env f e =
  let ps, _ = params e in
  let rec fix cur =
    let next =
      signature { env with sigs = (f, cur) :: List.remove_assoc f env.sigs } e
    in
    if next = cur then cur else fix next
  in
  fix (List.map (fun _ -> false) ps)

and shut env e why =
  if walk env [] (snd (params e)) <> [] then raise (BorrowError why)

type state = env * (ident * string) list

let initial : state = ({ sigs = []; alias = [] }, [])

let check_stmt (env, dead) stmt =
  match stmt with
  | TypeStmt _ -> (env, dead)
  | ExprStmt e -> (env, walk env dead e)
  | LetStmt (x, e) -> local env dead x e
  | RecStmt (f, e) ->
      ( {
          env with
          sigs = (f, recursive env f e) :: List.remove_assoc f env.sigs;
        },
        forget dead f )

let check_program stmts = ignore (List.fold_left check_stmt initial stmts)
