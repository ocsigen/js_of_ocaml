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
  [ "use-js-string", string_of_bool (Config.Flag.use_js_string ())
  ; "effects", string_of_effects_backend (Config.effects ())
  ; "version", version
  ; "kind", string_of_kind kind
  ]
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
  StringMap.iter
    (fun k v ->
      match k with
      | "use-js-string" -> Config.Flag.set k (bool_of_string v)
      | "effects" -> Config.set_effects_backend (effects_backend_of_string v)
      | _ -> ())
    t
