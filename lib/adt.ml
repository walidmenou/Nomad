open Ast

type decl = { params : int list; cons : (ident * typ list) list }

let table : (ident, decl) Hashtbl.t = Hashtbl.create 16
let declare name d = Hashtbl.replace table name d
let find name = Hashtbl.find_opt table name
let reset () = Hashtbl.reset table
