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
module Primitive = Jsoo_primitive

let loc pi = match pi with
  | Some {Parse_info.src  = Some src; line}
  | Some {Parse_info.name = Some src; line} ->
     Printf.sprintf "%s:%d" src line
  | None
  | Some _ -> "unknown location"

let parse_annot loc s =
  let buf = Lexing.from_string s in
  try
    match Annot_parser.annot Annot_lexer.initial buf with
      | `Requires (_,l) -> Some (`Requires (Some loc,l))
      | `Provides (_,n,k,ka) -> Some (`Provides (Some loc,n,k,ka))
      | `Version (_,l) -> Some (`Version  (Some loc, l))
  with
    | Not_found -> None
    | _exc ->
    (* Format.eprintf "Not found for %s : %s @." (Printexc.to_string exc) s; *)
    None

let error s = Format.ksprintf (fun s -> failwith s) s

let is_file_directive cmt =
  let lexbuf = Lexing.from_string cmt in
  try
    let _file,_line = Js_lexer.pos lexbuf in
    true
  with _ -> false

let parse_file f =
  let file =
    try
      match Util.path_require_findlib f with
        | Some f ->
          let pkg,f' = match Util.split Filename.dir_sep f with
            | [] -> assert false
            | [f] -> "js_of_ocaml",f
            | pkg::l -> pkg, List.fold_left Filename.concat "" l in
          Util.absolute_path (Filename.concat (Util.find_pkg_dir pkg)  f')
        | None -> Util.absolute_path f
    with
      | Not_found ->
        error "cannot find file '%s'. @." f
      | Sys_error s ->
        error "%s@." s
  in

  let lex = Parse_js.lexer_from_file ~rm_comment:false file in
  let status,lexs = Parse_js.lexer_fold (fun (status,lexs) t ->
      match t with
      | Js_token.TComment (_info,str) when is_file_directive str -> begin
          match status with
          | `Annot _ -> `Annot [],lexs
          | `Code (an,co) -> `Annot [], ((List.rev an,List.rev co)::lexs)
        end
      | Js_token.TComment (info,str) -> begin
          match parse_annot info str with
          | None -> (status,lexs)
          | Some a ->
            match status with
            | `Annot annot -> `Annot (a::annot),lexs
            | `Code (an,co) -> `Annot [a], ((List.rev an,List.rev co)::lexs)
        end
      | _ when Js_token.is_comment t -> (status,lexs)
      | Js_token.TUnknown (info,_) ->
        Format.eprintf "Unknown token while parsing JavaScript at %s@." (loc (Some info));
        if not (Filename.check_suffix file ".js")
        then Format.eprintf "%S doesn't look like a JavaScript file@." file;
        failwith "Error while parsing JavaScript"
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
        let name = match pi with
          | {Parse_info.src  = Some x; _}
          | {Parse_info.name = Some x; _} -> x
          | _ -> "??" in
        error "cannot parse file %S (orig:%S from l:%d, c:%d)@."
          f name pi.Parse_info.line pi.Parse_info.col)
      lexs in
  res


class check_and_warn name pi = object
  inherit Js_traverse.free as super
  method merge_info from =
    let def = from#get_def_name in
    let use = from#get_use_name in
    let diff = StringSet.diff def use in
    let diff = StringSet.remove name diff in
    let diff = StringSet.filter (fun s -> String.length s <> 0 && s.[0] <> '_') diff in
    if not (StringSet.is_empty diff)
    then Util.warn "WARN unused for primitive %s at %s:@. %s@."
        name (loc pi) (String.concat ", " (StringSet.elements diff));
    super#merge_info from
end

exception May_not_return

(*
let all_return p =
  let open Javascript in
  let rec loop_st = function
    | [] -> raise  May_not_return
    | [Return_statement (Some _), _] -> ()
    | [Return_statement None, _] -> raise May_not_return
    | [If_statement(_,th,el), _] ->
      loop_st [th];
      (match el with
       | None -> raise May_not_return
       | Some x -> loop_st [x])
    | [Do_while_statement(st,_), _] -> loop_st [st]
    | [While_statement(_,st), _] -> loop_st [st]
    | [For_statement (_,_,_,st), _] -> loop_st [st]
    | [Switch_statement (_,l,def), _] ->
      List.iter (fun (_,sts) -> loop_st sts) l
    | [Try_statement(b,_,_),_] -> loop_st b
    | [Throw_statement _, _] -> ()
    | x::xs -> loop_st xs
  in
  let rec loop_sources = function
    | [] -> raise May_not_return
    | [(Statement x, loc)] -> loop_st [(x, loc)]
    | [_] -> raise May_not_return
    | x::xs -> loop_sources xs
  in
  let rec loop_all_sources = function
    | [] -> ()
    | Statement x :: xs -> loop_all_sources xs
    | Function_declaration(_,_,b,_) :: xs ->
      loop_sources b;
      loop_all_sources xs in
  try loop_all_sources p; true with May_not_return -> false
*)

let check_primitive name pi code req =
  let free =
    if Option.Optim.warn_unused ()
    then new check_and_warn name pi
    else new Js_traverse.free in
  let _code = free#program code in
  let freename = free#get_free_name in
  let freename = List.fold_left (fun freename x -> StringSet.remove x freename) freename req in
  let freename = StringSet.diff freename Reserved.keyword in
  let freename = StringSet.diff freename Reserved.provided in
  let freename = StringSet.remove Option.global_object freename in
  if not(StringSet.mem name free#get_def_name)
  then begin
    Util.warn "warning: primitive code does not define value with the expected name: %s (%s)@." name (loc pi)
  end;
  if not(StringSet.is_empty freename)
  then begin
    Util.warn "warning: free variables in primitive code %S (%s)@." name (loc pi);
    Util.warn "vars: %s@." (String.concat ", " (StringSet.elements freename))
  end
  (* ; *)
  (* return checks disabled *)
  (* if false && not (all_return code) *)
  (* then Format.eprintf "warning: returns may be missing for primitive code %S (%s)@." name (loc pi) *)

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

class traverse_and_find_named_values all =
  object
    inherit Js_traverse.map as self
    method expression x =
      let open Javascript in
      (match x with
        | ECall(EVar (S {name="caml_named_value"}),[EStr (v,_)],_) ->
          all:=StringSet.add v !all
        | _ -> ()
      );
      self#expression x
  end

let find_named_value code =
  let all = ref StringSet.empty in
  let p = new traverse_and_find_named_values all in
  ignore(p#program code);
  !all

let add_file f =
  List.iter
    (fun (provide,req,versions,(code:Javascript.program)) ->
       let vmatch = match versions with
         | [] -> true
         | l -> List.exists version_match l in
       if vmatch
       then begin
         incr last_code_id;
         let id = !last_code_id in
         (match provide with
          | None ->
            always_included := id :: !always_included
          | Some (pi,name,kind,ka) ->
            let module J = Javascript in
            let rec find = function
              | [] -> None
              | (J.Function_declaration (J.S{J.name=n},l,_,_), _)::_ when name=n -> Some(List.length l)
              | _::rem -> find rem in
            let arity = find code in
            let named_values = find_named_value code in
            Primitive.register name kind ka arity;
            StringSet.iter Primitive.register_named_value named_values;
            if Hashtbl.mem provided name
            then begin
              let ploc = snd(Hashtbl.find provided name) in
	      Util.warn "warning: overriding primitive %S\n  old: %s\n  new: %s@." name (loc ploc) (loc pi)
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
        Util.warn "code providing %s (%s) may miss dependencies: %s\n"
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
          Hashtbl.fold (fun nm (_id,_) (visited,set) ->
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

let all state =
  IntSet.fold (fun id acc ->
    try
      let name,_ = Hashtbl.find provided_rev id in
      name :: acc
    with Not_found ->
      acc
  ) state.ids []
