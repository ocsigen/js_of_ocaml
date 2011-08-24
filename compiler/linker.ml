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

let ws_opt = "[ \t]*"
let ws = "[ \t]+"
let ident = "[a-zA-Z$_][a-zA-Z$_0-9]*"

let comment_rx =
  Str.regexp
    (String.concat "\\|"
       [Format.sprintf "^%s$" ws_opt;
        Format.sprintf "^%s//" ws_opt;
        Format.sprintf "^%s/[*]\\([^*]\\|[*]+[^/]\\)*[*]/%s$" ws_opt ws_opt])

let provides_start_rx =
  Str.regexp (Format.sprintf "^//%sProvides:" ws_opt)

let provides_rx =
  Str.regexp (Format.sprintf "^//%sProvides:%s\\(%s\\)\\(%s\\(%s\\)\\)?%s$"
                ws_opt ws_opt ident ws ident ws_opt)

let requires_start_rx =
  Str.regexp (Format.sprintf "^//%sRequires:" ws_opt)

let requires_rx =
  Str.regexp (Format.sprintf "^//%sRequires:%s\\(\\(%s%s,%s\\)*%s\\)%s$"
                ws_opt ws_opt ident ws_opt ws_opt ident ws_opt)

let comma_rx = Str.regexp (Format.sprintf "%s,%s" ws_opt ws_opt)

let error (f, i) s =
  Format.eprintf "%s:%d: error: %s@." f i s;
  exit 1

let read_line ch loc =
  let l = input_line ch in
  if Str.string_match provides_start_rx l 0 then begin
    if not (Str.string_match provides_rx l 0) then
      error loc "malformed provide line";
    `Provides (loc, Str.matched_group 1 l,
               try Str.matched_group 3 l with Not_found -> "mutator")
  end else if Str.string_match requires_start_rx l 0 then begin
    if not (Str.string_match requires_rx l 0) then
      error loc "malformed requirement line";
    `Requires (loc, Str.split comma_rx (Str.matched_group 1 l))
  end else if Str.string_match comment_rx l 0 then
    `Comment
  else
    `Code l

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

let parse_file f =
  let ch =
    try
      open_in f
    with Sys_error s ->
      Format.eprintf "%s: %s@." Sys.argv.(0) s;
      exit 1
  in
  let l = ref [] in
  let i = ref 0 in
  begin try
    while true do
      incr i;
      let x = read_line ch (f, !i) in
(*
      debug x;
*)
      l := x :: !l
    done
  with End_of_file -> () end;
  close_in ch;
  !l >> List.rev >> List.filter (fun x -> x <> `Comment)
  >> repeat
       (collect_provides ++ collect_requires ++ collect_code)
       (fun info _ -> info)
  >> List.map
       (fun ((prov, req), code) ->
          let req =
            req >> List.map (fun (loc, l) -> List.map (fun r -> (loc, r)) l)
                >> List.flatten
          in
          (prov, req, code))

let last_code_id = ref 0
let provided = Hashtbl.create 31
let code_pieces = Hashtbl.create 31

let add_file f =
  List.iter
    (fun (prov, req, code) ->
       incr last_code_id;
       let id = !last_code_id in
       List.iter
         (fun (loc, nm, kind) ->
            let kind =
              match kind with
                "pure" | "const" -> `Pure
              | "mutable" -> `Mutable
              | "mutator" -> `Mutator
              | _ ->
                  error loc (Format.sprintf "invalid primitive kind '%s'" kind)
            in
            Primitive.register nm kind;
            Hashtbl.add provided nm (id, loc))
         prov;
       Hashtbl.add code_pieces id (code, req))
    (parse_file f)

let rec resolve_dep f visited path loc nm =
  let (id, loc) =
    try
      Hashtbl.find provided nm
    with Not_found ->
      error loc (Format.sprintf "missing dependency '%s'@." nm)
  in
  if Util.IntSet.mem id visited then begin
(*    if List.mem_assoc id path then error loc "circular dependency";*)
    visited
  end else begin
    let visited = Util.IntSet.add id visited in
    let path = (id, loc) :: path in
    let (code, req) = Hashtbl.find code_pieces id in
    let visited =
      List.fold_left
        (fun visited (loc, nm) -> resolve_dep f visited path loc nm)
        visited req
    in
    List.iter (fun s -> Pretty_print.string f s; Pretty_print.newline f) code;
    visited
  end

let resolve_deps compact f l =
  let (missing, _) =
    List.fold_left
      (fun (missing, visited) nm ->
         if Hashtbl.mem provided nm then
           (missing, resolve_dep f visited [] ("", -1) nm)
         else
           (nm :: missing, visited))
      ([], Util.IntSet.empty) l
  in
  List.rev missing

(*
let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    add_file Sys.argv.(i)
  done;
  let missing = ref [] in
  Format.eprintf "%a@."
    (fun f v -> missing := resolve_deps f v) ["caml_array_get"]
*)
