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
      | `Provides (_,n,k,ka) -> Some (`Provides (Some loc,n,k,ka))
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
        let code = Parse_js.parse lex in
        let req,has_provide,versions = List.fold_left (fun (req,has_provide,versions) a -> match a with
            | `Provides (pi,name,kind,ka) ->
              req,Some (pi,name,kind,ka),versions
            | `Requires (_,mn) -> (mn@req),has_provide,versions
            | `Version (_,l) -> req,has_provide,l::versions
          ) ([],None,[]) annot in
        has_provide,req,versions,code
      with Parse_js.Parsing_error pi ->
        error "cannot parse file %S (orig:%S from l:%d, c:%d)@."
          f pi.Parse_info.name pi.Parse_info.line pi.Parse_info.col)
      lexs in
  res

let loc pi = match pi with
  | None -> "unknown location"
  | Some pi -> Printf.sprintf "%s:%d" pi.Parse_info.name pi.Parse_info.line


let check_primitive name pi code req =
  let free = new Js_traverse.free in
  let _code = free#program code in
  let freename = free#get_free_name in
  let freename = List.fold_left (fun freename x -> StringSet.remove x freename) freename req in
  let freename = StringSet.diff freename Reserved.keyword in
  let freename = StringSet.diff freename Reserved.provided in
  let freename = StringSet.remove Option.global_object_inside_jsoo freename in
  if not(StringSet.mem name free#get_def_name)
  then begin
    Format.eprintf "warning: primitive code does not define value with the expected name: %s (%s)@." name (loc pi)
  end;
  if not(StringSet.is_empty freename)
  then begin
    Format.eprintf "warning: free variables in primitive code %S (%s)@." name (loc pi);
    Format.eprintf "vars: %s@." (String.concat ", " (StringSet.elements freename))
  end

let version_match =
  List.for_all (fun (op,str) ->
      op (Util.Version.(compare current (split str))) 0
    )

type state = {
  ids : IntSet.t;
  codes : Javascript.program list ;
}

let last_code_id = ref 0
let provided = Hashtbl.create 31
let provided_rev = Hashtbl.create 31
let code_pieces = Hashtbl.create 31
let always_included = ref []


let add_file f =
  List.iter
    (fun (provide,req,versions,code) ->
       incr last_code_id;
       let id = !last_code_id in
       let vmatch = match versions with
         | [] -> true
         | l -> List.exists version_match l in
       if vmatch
       then begin
         (match provide with
          | None -> always_included := id :: !always_included
          | Some (pi,name,kind,ka) ->
            let module J = Javascript in
            let rec find = function
              | [] -> None
              | J.Function_declaration (J.S{J.name=n},l,_,_)::_ when name=n -> Some(List.length l)
              | _::rem -> find rem in
            let arity = find code in
            Primitive.register name kind ka arity;
            if Hashtbl.mem provided name
            then begin
              let ploc = snd(Hashtbl.find provided name) in
              Format.eprintf "warning: overriding primitive %S\n  old: %s\n  new: %s@." name (loc ploc) (loc pi)
            end;

            Hashtbl.add provided name (id,pi);
            Hashtbl.add provided_rev id (name,pi);
            check_primitive name pi code req
         );
         Hashtbl.add code_pieces id (code, req)
       end
    )
    (parse_file f)

let get_provided () =
  Hashtbl.fold (fun k _ acc -> StringSet.add k acc) provided StringSet.empty

let check_deps () =
  let provided = get_provided () in
  Hashtbl.iter (fun id (code,requires) ->
    let traverse = new Js_traverse.free in
    let _js = traverse#program code in
    let free = traverse#get_free_name in
    let requires = List.fold_right StringSet.add requires StringSet.empty in
    let real = StringSet.inter free provided in
    let missing = StringSet.diff real requires in
    if not (StringSet.is_empty missing)
    then begin
      try
        let (name,ploc) = Hashtbl.find provided_rev id in
        Format.eprintf "code providing %s (%s) may miss dependencies: %s\n"
          name
          (loc ploc)
          (String.concat ", " (StringSet.elements missing))
      with Not_found ->
        (* there is no //Provides for this piece of code *)
        (* FIXME handle missing deps in this case *)
        ()
    end
  ) code_pieces

let load_files l =
  List.iter add_file l;
  check_deps ()

(* resolve *)
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


let init () =
  List.fold_left
    (fun visited id -> resolve_dep_id_rev visited [] id)
    {ids=IntSet.empty; codes=[]} !always_included

let resolve_deps ?(linkall = false) visited_rev used =
  (* link the special files *)
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
  visited_rev, missing

let link program state = List.flatten (List.rev (program::state.codes))
