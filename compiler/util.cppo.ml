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
let opt_filter p x =
  match x with None -> None | Some v -> if p v then Some v else None

(****)
let quiet = ref false
let warn fmt = Format.ksprintf (fun s -> if not !quiet then Format.eprintf "%s%!" s) fmt

let find_pkg_dir_ref = ref (fun _ -> raise Not_found)
let set_find_pkg_dir f = find_pkg_dir_ref:=f
let find_pkg_dir pkg = !find_pkg_dir_ref pkg

let path_require_findlib path =
  if path <> "" && path.[0] = '+'
  then Some (String.sub  path 1 (String.length path - 1))
  else None

let rec find_in_findlib_paths ?(pkg="stdlib") paths name =
  match paths with
  | [] ->
    raise Not_found
  | path :: rem ->
    try
      let file = match path_require_findlib path with
        | Some path ->
          Filename.concat (Filename.concat (find_pkg_dir pkg) path) name
        | None -> Filename.concat path name in

      if Sys.file_exists file then file else
        find_in_findlib_paths rem name
    with Not_found -> find_in_findlib_paths rem name

let rec find_in_path paths name =
  match paths with
  | [] -> raise Not_found
  | path :: rem ->
    let file = Filename.concat path name in
    if Sys.file_exists file
    then file
    else find_in_path rem name

let absolute_path f =
  if Filename.is_relative f
  then Filename.concat (Sys.getcwd()) f
  else f

let read_file f =
  let ic = open_in_bin f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let filter_map f l =
  let l = List.fold_left (fun acc x -> match f x with
    | Some x -> x::acc
    | None -> acc) [] l
  in List.rev l

let sort_uniq compare l =
  let l = List.sort compare l in
  match l with
  | [] -> []
  | [x] -> [x]
  | x::xs ->
    let rec loop prev = function
      | [] -> [prev]
      | x::rest when compare x prev = 0 -> loop prev rest
      | x::rest -> prev :: loop x rest
    in loop x xs

let array_fold_right_i f a x =
  let r = ref x in
  for i = Array.length a - 1 downto 0 do
    r := f i (Array.unsafe_get a i) !r
  done;
  !r

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
    if s.[i] > '\127' then res := false
  done;
  !res

let has_backslash s =
  let res = ref false in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\\' then res := true
  done;
  !res

let fail = ref true

let failwith_ fmt =
  Printf.ksprintf (fun s ->
    if !fail
    then failwith s
    else Format.eprintf "%s@." s) fmt

let raise_ exn =
  if !fail
  then raise exn
  else begin
    Format.eprintf "%s@." (Printexc.to_string exn)
  end

let split_char sep p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then
      if cur - beg > 0
      then [String.sub p beg (cur - beg)]
      else []
    else if p.[cur] = sep then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  split 0 0

(* copied from https://github.com/ocaml/ocaml/pull/10 *)
let split sep s =
  let sep_len = String.length sep in
  if sep_len = 1
  then split_char sep.[0] s
  else
    let sep_max = sep_len - 1 in
    if sep_max < 0 then invalid_arg "String.split: empty separator" else
      let s_max = String.length s - 1 in
      if s_max < 0 then [""] else
        let acc = ref [] in
        let sub_start = ref 0 in
        let k = ref 0 in
        let i = ref 0 in
        (* We build the substrings by running from the start of [s] to the
           end with [i] trying to match the first character of [sep] in
           [s]. If this matches, we verify that the whole [sep] is matched
           using [k]. If this matches we extract a substring from the start
           of the current substring [sub_start] to [!i - 1] (the position
           before the [sep] we found).  We then continue to try to match
           with [i] by starting after the [sep] we just found, this is also
           becomes the start position of the next substring. If [i] is such
           that no separator can be found we exit the loop and make a
           substring from [sub_start] until the end of the string. *)
        while (!i + sep_max <= s_max) do
          if String.unsafe_get s !i <> String.unsafe_get sep 0 then incr i else
            begin
              (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
                 guaranteed by loop invariant. *)
              k := 1;
              while (!k <= sep_max && String.unsafe_get s (!i + !k) = String.unsafe_get sep !k)
              do incr k done;
              if !k <= sep_max then (* no match *) incr i else begin
                let new_sub_start = !i + sep_max + 1 in
                let sub_end = !i - 1 in
                let sub_len = sub_end - !sub_start + 1 in
                acc := String.sub s !sub_start sub_len :: !acc;
                sub_start := new_sub_start;
                i := new_sub_start;
              end
            end
        done;
        List.rev (String.sub s !sub_start (s_max - !sub_start + 1) :: !acc)

exception Found of int
let find sep s =
  let sep_max = String.length sep - 1 in
  let s_max = Bytes.length s - 1 in
  if sep_max < 0 then invalid_arg "find: empty string";
  let k = ref 0 in
  let i = ref 0 in
  try
    while (!i + sep_max <= s_max) do
      if Bytes.unsafe_get s !i <> String.unsafe_get sep 0
      then incr i
      else
        begin
          (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
             guaranteed by loop invariant. *)
          k := 1;
          while (!k <= sep_max && Bytes.unsafe_get s (!i + !k) = String.unsafe_get sep !k)
          do incr k done;
          if !k <= sep_max then (* no match *) incr i else raise (Found !i)
        end
    done;
    raise Not_found
  with Found i -> i

module Version = struct
  type t = int list
  let split v =
    match split_char '+' v with
    | [] -> assert false
    | x::_ -> List.map int_of_string (split_char '.' x)

  let current = split Sys.ocaml_version

  let compint (a : int) b = compare a b

  let rec compare v v' = match v,v' with
    | [x],[y] -> compint x y
    | [],[] -> 0
    | [],y::_ -> compint 0 y
    | x::_,[] -> compint x 0
    | x::xs,y::ys ->
      match compint x y with
      | 0 -> compare xs ys
      | n -> n

  let v =
    if compare current [4;2] < 0 then
      `V3
    else
      `V4_02

end

module MagicNumber = struct
  type t = string * int

  exception Bad_magic_number of string
  exception Bad_magic_version of t

  let size = 12

  let kind_of_string = function
    | "Caml1999X" -> "exe"
    | "Caml1999I" -> "cmi"
    | "Caml1999O" -> "cmo"
    | "Caml1999A" -> "cma"
    | "Caml1999Y" -> "cmx"
    | "Caml1999Z" -> "cmxa"
    | "Caml2007D" -> "cmxs"
    | "Caml2012T" -> "cmt"
    | "Caml1999M" -> "impl"
    | "Caml1999N" -> "intf"
    | _ -> raise Not_found

  let of_string s =
    try
      if String.length s <> size
      then raise Not_found;
      let kind = String.sub s 0 9 in
      let v = String.sub s 9 3 in
      let _ = kind_of_string kind in
      kind, int_of_string v
    with _ -> raise (Bad_magic_number s)

  let kind (s,_) =
    match kind_of_string s with
    | "exe" -> `Exe
    | "cmo" -> `Cmo
    | "cma" -> `Cma
    | other -> `Other other

  let to_string (k,v) = Printf.sprintf "%s%03d" k v

  let compare (p1,n1) (p2,n2) =
    if p1 <> p2 then raise Not_found;
    compare n1 n2

  let current_exe =
    let v = match Version.v with
      | `V3 -> 8
      | `V4_02 -> 11 in
    ("Caml1999X",v)

  let current_cmo =
    let v = match Version.v with
      | `V3 -> 7
      | `V4_02 -> 10 in
    ("Caml1999O", v)

  let current_cma =
    let v = match Version.v with
      | `V3 -> 8
      | `V4_02 -> 11 in
    ("Caml1999A", v)

  let current = function
    | `Exe -> current_exe
    | `Cmo -> current_cmo
    | `Cma -> current_cma

end


let normalize_argv ?(warn_=false) a =
  let bad = ref [] in
  let a = Array.map (fun s ->
    let size = String.length s in
    if size <= 2 then s
    else if s.[0] = '-' && s.[1] <> '-' && s.[2] <> '='
    then begin
      bad:=s::!bad;
      (* long option with one dash lets double the dash *)
      "-"^s
    end
    else s
  ) a in
  if (warn_ && !bad <> [])
  then
    warn
      "[Warning] long options with a single '-' are now deprecated.\ Please use '--' for the following options: %s@." (String.concat ", " !bad);
  a

let rec obj_of_const =
  let open Lambda in
  let open Asttypes in
  function
  | Const_base (Const_int i) -> Obj.repr i
  | Const_base (Const_char c) -> Obj.repr c
#if OCAML_VERSION < (4,02,0)
  | Const_base (Const_string s) -> Obj.repr s
#else
  | Const_base (Const_string (s,_)) -> Obj.repr s
#endif
  | Const_base (Const_float s) -> Obj.repr (float_of_string s)
  | Const_base (Const_int32 i) -> Obj.repr i
  | Const_base (Const_int64 i) -> Obj.repr i
  | Const_base (Const_nativeint i) -> Obj.repr i
  | Const_immstring s -> Obj.repr s
  | Const_float_array sl ->
    let l = List.map float_of_string sl in
    Obj.repr (Array.of_list l)
  | Const_pointer i ->
    Obj.repr i
  | Const_block (tag,l) ->
    let b = Obj.new_block tag (List.length l) in
    List.iteri (fun i x ->
      Obj.set_field b i (obj_of_const x)
    ) l;
    b
