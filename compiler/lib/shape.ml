(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
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

open! Stdlib

type t =
  | Top of string
  | Block of t list
  | Function of
      { arity : int
      ; pure : bool
      ; res : t
      }

type shape = t

let rec equal a b =
  match a, b with
  | Top _, Top _ -> true
  | ( Function { arity = a1; pure = p1; res = r1 }
    , Function { arity = a2; pure = p2; res = r2 } ) ->
      a1 = a2 && Bool.(p1 = p2) && equal r1 r2
  | Block b1, Block b2 -> (
      try List.for_all2 ~f:equal b1 b2 with Invalid_argument _ -> false)
  | Top _, (Function _ | Block _)
  | Function _, (Top _ | Block _)
  | Block _, (Top _ | Function _) -> false

let rec to_string (shape : t) =
  match shape with
  | Top s -> if true then "N" else Printf.sprintf "N(%s)" s
  | Block l -> "[" ^ String.concat ~sep:"," (List.map ~f:to_string l) ^ "]"
  | Function { arity; pure; _ } ->
      Printf.sprintf "F(%d)%s" arity (if pure then "" else "")

module Store = struct
  module T = Hashtbl.Make (struct
    type t = string

    let equal (a : t) (b : t) = String.equal a b

    let hash = Hashtbl.hash
  end)

  let ext = ".jsoo-shape"

  let filename ~dir ~name = Filename.concat dir (name ^ ext)

  let t = T.create 17

  let loaded = Hashtbl.create 17

  let set ~name shape = T.replace t name shape

  let get ~name = T.find_opt t name

  let magic = "JsooShape000"

  let load' fn =
    let ic = open_in_bin fn in
    let m = really_input_string ic (String.length magic) in
    if not (String.equal m magic)
    then failwith (Printf.sprintf "Invalid magic number for shape file %s" fn);
    let shapes : (string * shape) list = Marshal.from_channel ic in
    close_in ic;
    List.iter shapes ~f:(fun (name, shape) -> set ~name shape)

  let load ~name dirs =
    if T.mem t name
    then get ~name
    else
      match Fs.find_in_path dirs (filename ~dir:"." ~name) with
      | Some f ->
          load' f;
          get ~name
      | None ->
          let rec scan : _ -> shape option = function
            | [] -> None
            | dir :: xs -> (
                let l =
                  Sys.readdir dir
                  |> Array.to_list
                  |> List.sort ~cmp:String.compare
                  |> List.map ~f:(fun n -> Filename.concat dir n)
                in
                match
                  List.find_map l ~f:(fun s ->
                      if Filename.check_suffix s ext && not (Hashtbl.mem loaded s)
                      then (
                        load' s;
                        Hashtbl.add loaded s ();
                        match get ~name with
                        | None -> None
                        | Some shape -> Some (s, shape))
                      else None)
                with
                | None -> scan xs
                | Some (fn, shape) ->
                    Format.eprintf "Shape: %s loaded from %s\n" name fn;
                    Some shape)
          in
          scan dirs

  let save' fn (l : (string * shape) list) =
    let oc = open_out_bin fn in
    output_string oc magic;
    Marshal.to_channel oc l [];
    close_out oc

  let save ~name ~dir =
    match get ~name with
    | None -> failwith (Printf.sprintf "Don't know any shape for %s" name)
    | Some shape ->
        let fn = filename ~dir ~name in
        save' fn [ name, shape ]
end

module State = struct
  type key = Code.Var.t

  module T = Hashtbl.Make (struct
    type t = key

    let equal a b = Poly.(a = b)

    let hash = Code.Var.idx
  end)

  let t = T.create 17

  let assign x shape = T.add t x shape

  let propagate x offset target =
    match T.find_opt t x with
    | None -> ()
    | Some (Top _ | Function _) -> ()
    | Some (Block l) -> T.replace t target (List.nth l offset)

  let get x = T.find_opt t x

  let is_pure_fun x =
    match T.find_opt t x with
    | None -> false
    | Some (Top _ | Block _) -> false
    | Some (Function { pure; _ }) -> pure

  let reset () = T.clear t
end
