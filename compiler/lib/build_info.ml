(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

type kind =
  [ `Runtime
  | `Exe
  | `Cmo
  | `Cma
  | `Unknown
  ]

let all = [ `Runtime; `Exe; `Cmo; `Cma; `Unknown ]

let string_of_kind = function
  | `Runtime -> "runtime"
  | `Exe -> "exe"
  | `Cmo -> "cmo"
  | `Cma -> "cma"
  | `Unknown -> "unknown"

let string_of_effects_backend : Config.effects_backend -> string = function
  | `Disabled -> "disabled"
  | `Cps -> "cps"
  | `Double_translation -> "double-translation"
  | `Jspi -> "jspi"

let effects_backend_of_string = function
  | "disabled" -> `Disabled
  | "cps" -> `Cps
  | "double-translation" -> `Double_translation
  | "jspi" -> `Jspi
  | _ -> invalid_arg "effects_backend_of_string"

type config_key =
  | Bool_key of
      { name : string
      ; get : unit -> bool
      ; set : bool -> unit
      }
  | Enum_key of
      { name : string
      ; get : unit -> string
      ; set : string -> unit
      ; valid : string list
      }

let config_key_name = function
  | Bool_key { name; _ } -> name
  | Enum_key { name; _ } -> name

let config_keys target =
  let effects_valid, effects_get =
    match target with
    | `JavaScript ->
        ( [ "disabled"; "cps"; "double-translation" ]
        , fun () -> string_of_effects_backend (Config.effects ()) )
    | `Wasm ->
        ( [ "disabled"; "cps"; "jspi" ]
        , fun () -> string_of_effects_backend (Config.effects ()) )
  in
  [ Enum_key
      { name = "effects"
      ; get = effects_get
      ; set = (fun s -> Config.set_effects_backend (effects_backend_of_string s))
      ; valid = effects_valid
      }
  ; Bool_key
      { name = "use-js-string"
      ; get = Config.Flag.use_js_string
      ; set = Config.Flag.set "use-js-string"
      }
  ]
  @
  match target with
  | `JavaScript ->
      [ Bool_key
          { name = "toplevel"
          ; get = Config.Flag.toplevel
          ; set = Config.Flag.set "toplevel"
          }
      ]
  | `Wasm -> []

let config_key_values = function
  | Bool_key _ -> [ "true"; "false" ]
  | Enum_key { valid; _ } -> valid

let get_values keys =
  List.map
    ~f:(fun key ->
      match key with
      | Bool_key { name; get; _ } -> name, string_of_bool (get ())
      | Enum_key { name; get; _ } -> name, get ())
    keys

let set_values keys entries =
  List.iter entries ~f:(fun (k, v) ->
      match List.find_opt keys ~f:(fun key -> String.equal (config_key_name key) k) with
      | None -> ()
      | Some (Bool_key { set; _ }) -> set (bool_of_string v)
      | Some (Enum_key { set; _ }) -> set v)

let to_config_string entries =
  let entries = List.sort ~cmp:(fun (k1, _) (k2, _) -> String.compare k1 k2) entries in
  String.concat ~sep:"+" (List.map ~f:(fun (k, v) -> Printf.sprintf "%s=%s" k v) entries)

let parse_config_string s =
  if String.is_empty s
  then []
  else
    s
    |> String.split_on_char ~sep:'+'
    |> List.map ~f:String.trim
    |> List.map ~f:(fun s ->
        match String.lsplit2 ~on:'=' s with
        | None -> s, ""
        | Some (k, v) -> k, v)

let kind_of_string s =
  match List.find_opt all ~f:(fun k -> String.equal s (string_of_kind k)) with
  | None -> `Unknown
  | Some k -> k

type t = string StringMap.t

let kind t =
  match StringMap.find "kind" t with
  | exception Not_found -> `Unknown
  | s -> kind_of_string s

let create kind =
  let version =
    match Compiler_version.git_version with
    | "" -> Compiler_version.s
    | v -> Printf.sprintf "%s+%s" Compiler_version.s v
  in
  get_values (config_keys (Config.target ()))
  @ [ "version", version; "kind", string_of_kind kind ]
  |> List.fold_left ~init:StringMap.empty ~f:(fun acc (k, v) -> StringMap.add k v acc)

let with_kind t kind = StringMap.add "kind" (string_of_kind kind) t

let prefix = "//# buildInfo:"

let to_string info =
  let str =
    StringMap.bindings info
    |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s=%s" k v)
    |> String.concat ~sep:", "
  in
  Printf.sprintf "%s%s\n" prefix str

let parse s =
  match String.drop_prefix ~prefix s with
  | None -> None
  | Some suffix ->
      let t =
        suffix
        |> String.split_on_char ~sep:','
        |> List.map ~f:String.trim
        |> List.map ~f:(fun s ->
            match String.lsplit2 ~on:'=' s with
            | None -> s, ""
            | Some (k, v) -> k, v)
        |> List.fold_left ~init:StringMap.empty ~f:(fun acc (k, v) ->
            StringMap.add k v acc)
      in
      Some t

let to_map : t -> string StringMap.t = Fun.id

let of_map : string StringMap.t -> t = Fun.id

exception
  Incompatible_build_info of
    { key : string
    ; first : (string * string option)
    ; second : (string * string option)
    }

let merge fname1 info1 fname2 info2 =
  if String.equal fname1 fname2
  then
    StringMap.merge
      (fun k v1 v2 ->
        match v1, v2 with
        | Some v1, Some v2 when String.equal v1 v2 -> Some v1
        | Some v1, Some v2 ->
            failwith
              (Printf.sprintf
                 "%s: Duplicated build info with incompatible value. key=%s, v1=%s, v2=%s"
                 fname1
                 k
                 v1
                 v2)
        | Some x, None | None, Some x -> Some x
        | None, None -> assert false)
      info1
      info2
  else
    StringMap.merge
      (fun k v1 v2 ->
        match k, v1, v2 with
        | "kind", v1, v2 ->
            if Option.equal String.equal v1 v2 then v1 else Some (string_of_kind `Unknown)
        | ("effects" | "use-js-string" | "version"), Some v1, Some v2
          when String.equal v1 v2 -> Some v1
        | (("effects" | "use-js-string" | "version") as key), v1, v2 ->
            raise
              (Incompatible_build_info { key; first = fname1, v1; second = fname2, v2 })
        | _, Some v1, Some v2 when String.equal v1 v2 -> Some v1
        (* ignore info that are present on one side only or have a different value *)
        | _, Some _, Some _ -> None
        | _, None, Some _ | _, Some _, None -> None
        | _, None, None -> assert false)
      info1
      info2

let configure t =
  let entries = StringMap.fold (fun k v acc -> (k, v) :: acc) t [] in
  set_values (config_keys (Config.target ())) entries
