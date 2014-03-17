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

let findlib_init = Lazy.lazy_from_fun Findlib.init

let find_pkg_dir pkg =
  let () = Lazy.force findlib_init in
  try Findlib.package_directory pkg with _ -> raise Not_found

let path_require_findlib path =
  if path <> "" && path.[0] = '+'
  then Some (String.sub  path 1 (String.length path - 1))
  else None

let rec find_in_paths ?(pkg="stdlib") paths name =
  match paths with
    | [] ->
      raise Not_found
    | path :: rem ->
      try
        let file = match path_require_findlib path with
          | Some path ->
            let () = Lazy.force findlib_init in
            Filename.concat (Filename.concat (find_pkg_dir pkg) path) name
          | None -> Filename.concat path name in

        if Sys.file_exists file then file else
          find_in_paths rem name
      with Not_found -> find_in_paths rem name

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

let filter_map f l =
  let l = List.fold_left (fun acc x -> match f x with
    | Some x -> x::acc
    | None -> acc) [] l
  in List.rev l

let rec take' acc n l =
  if n = 0
  then acc,l
  else match l with
    | [] -> acc,[]
    | x::xs -> take' (x::acc) (pred n) xs

let take n l =
  let x,xs = take' [] n l in
  List.rev x, xs

module Timer = struct
  type t = float
  let timer = ref (fun _ -> 0.)
  let init f = timer := f
  let make () = !timer ()
  let get t = !timer () -. t
  let print f t = Format.fprintf f "%.2f" (get t)
end


let is_ascii s =
  let res = ref true in
  for i = 0 to String.length s - 1 do
    if s.[i] >= '\127' || s.[i] <= '\031' then res := false
  done;
  !res

let has_backslash s =
  let res = ref false in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\\' then res := true
  done;
  !res
