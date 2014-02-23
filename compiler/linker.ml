(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


open Util

let parse_annot loc s =
  let buf = Lexing.from_string s in
  try
    match Annot_parser.annot Annot_lexer.initial buf with
      | `Requires (_,l) -> Some (`Requires (Some loc,l))
      | `Provides (_,n,k) -> Some (`Provides (Some loc,n,k))
  with
    | Not_found -> None
    | exc ->
    (* Format.eprintf "Not found for %s : %s @." (Printexc.to_string exc) s; *)
    None

let error s =
  Format.kprintf (fun s -> Format.eprintf "error: %s@." s; exit 1) s

let debug l =
  match l with
    `Provides (_, nm, k) -> Format.eprintf "provides %s (%s)@." nm k
  | `Requires (_, l) -> Format.eprintf "requires";
      List.iter (fun nm -> Format.eprintf " %s" nm) l;
      Format.eprintf "@."
  | `Comment  -> Format.eprintf "comment@."
  | `Code _   -> Format.eprintf "code@."
  | `EOF      -> Format.eprintf "eof@."


let return x cont l = cont x l

let bind x f cont l = x (fun v l -> f v cont l) l

let accept f cont l =
  match l with
    [] ->
      cont None []
  | x :: r ->
      let v = f x in
      if v = None then cont None l else cont v r

let (++) f g =
  bind f (fun v1 -> bind g (fun v2 -> return (v1, v2)))

let rec collect f =
  bind (accept f) (fun v ->
  match v with
    None   -> return []
  | Some v -> bind (collect f) (fun r -> return (v :: r)))

let rec repeat f cont l =
  if l = [] then return [] cont l else
  bind f (fun v -> bind (repeat f) (fun r -> return (v :: r))) cont l

let collect_provides l =
  collect (fun l -> match l with `Provides v -> Some v | _ -> None) l

let collect_requires l =
  collect (fun l -> match l with `Requires v -> Some v | _ -> None) l

let collect_code l =
  collect (fun l -> match l with `Code v -> Some v | _ -> None) l

let (>>) x f = f x

let split_dir =
  let reg = Str.regexp_string Filename.dir_sep in
  Str.split reg

let parse_file f =
  let file =
    try
      match Util.path_require_findlib f with
        | Some f ->
          let pkg,f' = match split_dir f with
            | [] -> assert false
            | [f] -> "js_of_ocaml",f
            | pkg::l -> pkg, List.fold_left Filename.concat "" l in
          Filename.concat (Util.find_pkg_dir pkg)  f'
        | None -> f
    with
      | Not_found ->
        error "cannot find file '%s'. @." f
      | Sys_error s ->
        error "%s@." s
  in

  let lex = Parse_js.lexer_from_file ~rm_comment:false file in
  let status,lexs = Parse_js.lexer_fold (fun (status,lexs) t ->
    match t with
      | Js_token.TComment (info,str) -> begin
        match parse_annot info str with
          | None -> (status,lexs)
          | Some a ->
            match status with
              | `Annot annot -> `Annot (a::annot),lexs
              | `Code (an,co) -> `Annot [a], ((List.rev an,List.rev co)::lexs)
      end
      | _ when Js_token.is_comment t -> (status,lexs)
      | c -> match status with
          | `Code (annot,code) -> `Code (annot,c::code),lexs
          | `Annot (annot) -> `Code(annot,[c]),lexs
  ) (`Annot [],[]) lex in
  let lexs = match status with
    | `Annot _ -> lexs
    | `Code(annot,code) -> (List.rev annot,List.rev code)::lexs in

  let res = List.rev_map (fun (annot,code) ->
    let lex = Parse_js.lexer_from_list code in
    try
      annot,Parse_js.parse lex
    with Parse_js.Parsing_error pi ->
      error "cannot parse file %s from l:%d, c:%d@." f pi.Parse_info.line pi.Parse_info.col)
    lexs in
  res

let last_code_id = ref 0
let provided = Hashtbl.create 31
let provided_rev = Hashtbl.create 31
let code_pieces = Hashtbl.create 31
let always_included = ref []

let loc pi = match pi with
  | None -> "unknown location"
  | Some pi -> Printf.sprintf "%s:%d" pi.Parse_info.name pi.Parse_info.line

let add_file f =

  List.iter
    (fun (annot,code) ->
      incr last_code_id;
      let id = !last_code_id in

      let req,has_provide = List.fold_left (fun (req,has_provide) a -> match a with
        | `Provides (pi,name,kind) ->
          Primitive.register name kind;
          if Hashtbl.mem provided name
          then begin
            let ploc = snd(Hashtbl.find provided name) in
            Format.eprintf "warning: overriding primitive %S\n  old: %s\n  new: %s@." name (loc ploc) (loc pi)
          end;
          Hashtbl.add provided name (id,pi);
          Hashtbl.add provided_rev id (name,pi);
          req,true
        | `Requires (_,mn) -> (mn@req),has_provide) ([],false) annot in

      if not has_provide then always_included := id :: !always_included;
      Hashtbl.add code_pieces id (code, req))
    (parse_file f)

type visited = {
  ids : IntSet.t;
  codes : Javascript.program list ;
}

let rec resolve_dep_name_rev visited path nm =
  let id =
    try
      fst(Hashtbl.find provided nm)
    with Not_found ->
      error "missing dependency '%s'@." nm
  in
  resolve_dep_id_rev visited path id

and resolve_dep_id_rev visited path id =
  if IntSet.mem id visited.ids then begin
    if false && List.memq id path then error  "circular dependency: %s" (String.concat ", " (List.map (fun id -> fst(Hashtbl.find provided_rev id)) path));
    visited
  end else begin
    let path = id :: path in
    let (code, req) = Hashtbl.find code_pieces id in
    let visited = {visited with ids = IntSet.add id visited.ids} in
    let visited =
      List.fold_left
        (fun visited nm -> resolve_dep_name_rev visited path nm)
        visited req in
    let visited = {visited with codes = code::visited.codes} in
    visited
  end

let resolve_deps ?(linkall = false) program used =
  (* link the special files *)
  let visited_rev = List.fold_left
      (fun visited id -> resolve_dep_id_rev visited [] id)
       {ids=IntSet.empty; codes=[]} !always_included
  in
  let missing,visited_rev =
    if linkall
    then
      begin
        (* link all primitives *)
        StringSet.empty, Hashtbl.fold (fun nm (id,_) visited -> resolve_dep_name_rev visited [] nm ) provided visited_rev
      end
    else (* link used primitives *)
      StringSet.fold
        (fun nm (missing, visited)->
           if Hashtbl.mem provided nm then
             (missing, resolve_dep_name_rev visited [] nm)
           else
             (StringSet.add nm missing, visited))
        used (StringSet.empty, visited_rev) in
  List.flatten (List.rev (program::visited_rev.codes)), missing

let get_provided () =
  Hashtbl.fold (fun k _ acc -> StringSet.add k acc) provided StringSet.empty

let check_deps () =
  let provided = get_provided () in
  let fail = ref false in
  Hashtbl.iter (fun id (code,requires) ->
    let traverse = new Js_traverse.free in
    let _js = traverse#program code in
    let free = traverse#get_free_name in
    let requires = List.fold_right StringSet.add requires StringSet.empty in
    let real = StringSet.inter free provided in
    let missing = StringSet.diff real requires in
    if not (StringSet.is_empty missing)
    then begin
      let (name,ploc) = Hashtbl.find provided_rev id in
      Format.eprintf "code providing %s (%s) may miss dependencies: %s\n"
        name
        (loc ploc)
        (String.concat ", " (StringSet.elements missing));
      fail := true
    end
  ) code_pieces
