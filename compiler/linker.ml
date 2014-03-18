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
      | `Version (_,l) -> Some (`Version  (Some loc, l))
  with
    | Not_found -> None
    | exc ->
    (* Format.eprintf "Not found for %s : %s @." (Printexc.to_string exc) s; *)
    None

let error s = Format.kprintf (fun s -> failwith s) s

let parse_file f =
  let file =
    try
      match Util.path_require_findlib f with
        | Some f ->
          let pkg,f' = match Util.split Filename.dir_sep f with
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
      error "cannot parse file %S (orig:%S from l:%d, c:%d)@." f pi.Parse_info.name pi.Parse_info.line pi.Parse_info.col)
    lexs in
  res

let last_code_id = ref 0
let provided = Hashtbl.create 31
let provided_rev = Hashtbl.create 31
let code_pieces = Hashtbl.create 31
let always_included = ref []

let version_match =
  let v = Sys.ocaml_version in
  let cano v = match Util.split_char '+' v with
    | [] -> assert false
    | x::_ -> List.map int_of_string (Util.split_char '.' x) in
  let rec compute op v v' = match v,v' with
    | [x],[y] -> op x y
    | [],[] -> op 0 0
    | [],y::_ -> op 0 y
    | x::_,[] -> op x 0
    | x::xs,y::ys ->
      if x = y
      then compute op xs ys
      else op x y
  in
  let v' = cano v in
  (fun l ->
     List.for_all (fun (op,str) ->
         compute op v' (cano str)
       ) l)

let loc pi = match pi with
  | None -> "unknown location"
  | Some pi -> Printf.sprintf "%s:%d" pi.Parse_info.name pi.Parse_info.line

let add_file f =

  List.iter
    (fun (annot,code) ->
      incr last_code_id;
      let id = !last_code_id in

      let req,has_provide,vmatch = List.fold_left (fun (req,has_provide,vmatch) a -> match a with
        | `Provides (pi,name,kind) ->
          req,Some (name,pi,kind),vmatch
        | `Requires (_,mn) -> (mn@req),has_provide,vmatch
        | `Version (_,l) ->
          let vmatch = match vmatch with
            | Some true -> vmatch
            | _ -> Some (version_match l) in
          req,has_provide,vmatch
        ) ([],None,None) annot in

      (match vmatch with
       | Some false -> ()
       | None | Some true ->

         (match has_provide with
          | None -> always_included := id :: !always_included
          | Some (name,pi,kind) ->

            Primitive.register name kind;
            if Hashtbl.mem provided name
            then begin
              let ploc = snd(Hashtbl.find provided name) in
              Format.eprintf "warning: overriding primitive %S\n  old: %s\n  new: %s@." name (loc ploc) (loc pi)
            end;
            Hashtbl.add provided name (id,pi);
            Hashtbl.add provided_rev id (name,pi);

            let free = new Js_traverse.free in
            let _code = free#program code in
            let freename = free#get_free_name in
            let freename = List.fold_left (fun freename x -> StringSet.remove x freename) freename req in
            let freename = StringSet.diff freename Reserved.keyword in
            let freename = StringSet.diff freename Reserved.provided in
            let freename = StringSet.remove Option.global_object freename in
            if not(StringSet.mem name free#get_def_name)
            then begin
              Format.eprintf "warning: primitive code does not define value with the expected name: %s (%s)@." name (loc pi)
            end;
            if not(StringSet.is_empty freename)
            then begin
              Format.eprintf "warning: free variables in primitive code %S (%s)@." name (loc pi);
              Format.eprintf "vars: %s@." (String.concat ", " (StringSet.elements freename))
            end);
         Hashtbl.add code_pieces id (code, req)
      ))
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
    if List.memq id path
    then error  "circular dependency: %s" (String.concat ", " (List.map (fun id -> fst(Hashtbl.find provided_rev id)) path));
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
        let prog,set =
          Hashtbl.fold (fun nm (id,_) (visited,set) ->
              resolve_dep_name_rev visited [] nm,
              StringSet.add nm set
            )
            provided
            (visited_rev,StringSet.empty) in
        let missing = StringSet.diff used set in
        missing,prog
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
