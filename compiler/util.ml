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

module Int = struct type t = int let compare (x : int) y = compare x y end
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let opt_map f x = match x with None -> None | Some v -> Some (f v)
let opt_iter f x = match x with None -> () | Some v -> f v
let opt_bind x f = match x with None -> None | Some v -> f v
let opt_filter p x =
  match x with None -> None | Some v -> if p v then Some v else None

(****)

let rec find_in_paths paths name =
  match paths with
    [] ->
      raise Not_found
  | path :: rem ->
      let file = Filename.concat path name in
      if Sys.file_exists file then file else find_in_paths rem name

let read_file f =
  let ch = open_in_bin f in
  let b = Buffer.create 4096 in
  let s = String.create 4096 in
  while
    let n = input ch s 0 4096 in
    Buffer.add_substring b s 0 n;
    n <> 0
  do () done;
  close_in ch;
  Buffer.contents b


let move_file source dest =
  try Sys.rename source dest with
    | Sys_error _ ->
      (* it may fail if not on the same device
         copy file instead *)
      let oc = open_out dest in
      let ic = open_in source in
      let buff = String.create (1024 * 1024) in
      let maxlen = String.length buff in
      let rec copy () =
        let count = input ic buff 0 maxlen in
        match count with
          | 0 -> ()
          | _ ->
            output oc buff 0 count;
            copy ()
      in
      copy ();
      close_out oc;
      close_in ic;
      Sys.remove source

module Timer = struct
  type t = float
  let timer = ref (fun _ -> 0.)
  let init f = timer := f
  let make () = !timer ()
  let get t = !timer () -. t
  let print f t = Format.fprintf f "%.2f" (get t)
end



module VarPrinter = struct

  type t = {
    names : (int,string) Hashtbl.t;
    known : (int,string) Hashtbl.t;
    mutable last : int;
    mutable pretty : bool;
  }

  let c1 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
  let c2 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

  let name_raw t v nm = Hashtbl.add t.names v nm
  let propagate_name t v v' =
    try name_raw t v' (Hashtbl.find t.names v) with Not_found -> ()

  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  let is_num c = (c >= '0' && c <= '9')

  let name t v nm =
    if String.length nm > 0 then begin
      let nm = String.copy nm in
      if not (is_alpha nm.[0]) then nm.[0] <- '_';
      for i = 1 to String.length nm - 1 do
        if not (is_alpha nm.[i] || is_num nm.[i]) then nm.[i] <- '_';
      done;
      let c = ref 0 in
      for i = 0 to String.length nm - 1 do
        if nm.[i] = '_' then incr c
      done;
      if !c < String.length nm then name_raw t v nm
    end

  let rec format_ident x =
    assert (x >= 0);
    let char c x = String.make 1 (c.[x]) in
    if x < 54 then
      char c1 x
    else
      format_ident ((x - 54) / 64) ^ char c2 ((x - 54) mod 64)

  let format_var t i x =
    let s = format_ident x in
    if t.pretty then begin
      try
        let nm = Hashtbl.find t.names i in
        Format.sprintf "%s_%s_" nm s
      with Not_found ->
        Format.sprintf "_%s_" s
    end else
      s

  let rec to_string t i =
    try
      Hashtbl.find t.known i
    with Not_found ->
      t.last <- t.last + 1;
      let j = t.last in
      let s = format_var t i j in
      if Reserved.mem s then
        to_string t i
      else begin
        Hashtbl.add t.known i s;
        s
      end

  let set_pretty t b = t.pretty <- b


  let reset t =
    Hashtbl.clear t.names; Hashtbl.clear t.known; t.last <- -1

  let create ?(pretty=false) () =
    let t = {
      names = Hashtbl.create 107;
      known = Hashtbl.create 1001;
      last = -1;
      pretty;
    } in
    reset t; t
end
