(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Gabriel Scherer, projet Parsifal, INRIA Saclay              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

type path = string

type path_prefix = string

type error_message = string

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let errorf fmt = Printf.ksprintf (fun err -> Error err) fmt

let encode_prefix str =
  let buf = Buffer.create (String.length str) in
  let push_char = function
    | '%' -> Buffer.add_string buf "%#"
    | '=' -> Buffer.add_string buf "%+"
    | ':' -> Buffer.add_string buf "%."
    | c -> Buffer.add_char buf c
  in
  String.iter ~f:push_char str;
  Buffer.contents buf

let decode_prefix str =
  let buf = Buffer.create (String.length str) in
  let rec loop i =
    if i >= String.length str
    then Ok (Buffer.contents buf)
    else
      match str.[i] with
      | ('=' | ':') as c -> errorf "invalid character '%c' in key or value" c
      | '%' -> (
          let push c =
            Buffer.add_char buf c;
            loop (i + 2)
          in
          if i + 1 = String.length str
          then errorf "invalid encoded string %S (trailing '%%')" str
          else
            match str.[i + 1] with
            | '#' -> push '%'
            | '+' -> push '='
            | '.' -> push ':'
            | c -> errorf "invalid %%-escaped character '%c'" c)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

type pair =
  { target : path_prefix
  ; source : path_prefix
  }

let encode_pair { target; source } =
  String.concat ~sep:"=" [ encode_prefix target; encode_prefix source ]

let decode_pair str =
  match String.lsplit2 str ~on:'=' with
  | None -> errorf "invalid key/value pair %S, no '=' separator" str
  | Some (encoded_target, encoded_source) -> (
      match decode_prefix encoded_target, decode_prefix encoded_source with
      | Ok target, Ok source -> Ok { target; source }
      | (Error _ as err), _ | _, (Error _ as err) -> err)

type map = pair option list

let encode_map map =
  let encode_elem = function
    | None -> ""
    | Some pair -> encode_pair pair
  in
  List.map ~f:encode_elem map |> String.concat ~sep:":"

exception Shortcut of error_message

let decode_map str =
  let decode_or_empty = function
    | "" -> None
    | pair -> (
        match decode_pair pair with
        | Ok str -> Some str
        | Error err -> raise (Shortcut err))
  in
  let pairs = String.split_on_char ~sep:':' str in
  match List.map ~f:decode_or_empty pairs with
  | exception Shortcut err -> Error err
  | map -> Ok map

let rewrite_opt prefix_map path =
  let is_prefix = function
    | None -> false
    | Some { target = _; source } -> String.starts_with path ~prefix:source
  in
  match
    List.find
      ~f:is_prefix
      (* read key/value pairs from right to left, as the spec demands *)
      (List.rev prefix_map)
  with
  | exception Not_found -> None
  | None -> None
  | Some { source; target } ->
      Some
        (target
        ^ String.sub
            path
            ~pos:(String.length source)
            ~len:(String.length path - String.length source))

let rewrite prefix_map path =
  match rewrite_opt prefix_map path with
  | None -> path
  | Some path -> path

let flip l =
  List.map l ~f:(Option.map ~f:(fun x -> { source = x.target; target = x.source }))

(* copied from ocaml/utils/misc.ml *)
let get_build_path_prefix_map =
  let init = ref false in
  let map_cache = ref None in
  fun () ->
    if not !init
    then (
      init := true;
      match Sys.getenv "BUILD_PATH_PREFIX_MAP" with
      | exception Not_found -> ()
      | encoded_map -> (
          match decode_map encoded_map with
          | Error err ->
              failwith
              @@ Printf.sprintf
                   "Invalid value for the environment variable BUILD_PATH_PREFIX_MAP: %s"
                   err
          | Ok map -> map_cache := Some map));
    !map_cache
