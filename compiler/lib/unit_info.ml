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

type t =
  { provides : StringSet.t
  ; requires : StringSet.t
  ; primitives : string list
  ; crcs : Digest.t option StringMap.t
  ; force_link : bool
  ; effects_without_cps : bool
  }

let empty =
  { provides = StringSet.empty
  ; requires = StringSet.empty
  ; primitives = []
  ; crcs = StringMap.empty
  ; force_link = false
  ; effects_without_cps = false
  }

let of_primitives l =
  { provides = StringSet.empty
  ; requires = StringSet.empty
  ; primitives = l
  ; crcs = StringMap.empty
  ; force_link = true
  ; effects_without_cps = false
  }

let of_cmo (cmo : Cmo_format.compilation_unit) =
  let open Ocaml_compiler in
  (* A packed librariy register global for packed modules. *)
  let provides = StringSet.of_list (Cmo_format.name cmo :: Cmo_format.provides cmo) in
  let requires = StringSet.of_list (Cmo_format.requires cmo) in
  let requires = StringSet.diff requires provides in
  let effects_without_cps =
    (not (Config.Flag.effects ()))
    && List.exists (Cmo_format.primitives cmo) ~f:(function
         | "%resume" | "%reperform" | "%perform" -> true
         | _ -> false)
  in
  let force_link = Cmo_format.force_link cmo in
  let crcs =
    List.fold_left (Cmo_format.imports cmo) ~init:StringMap.empty ~f:(fun acc (s, o) ->
        StringMap.add s o acc)
  in
  { provides; requires; primitives = []; force_link; effects_without_cps; crcs }

let union t1 t2 =
  let provides = StringSet.union t1.provides t2.provides in
  let requires = StringSet.union t1.requires t2.requires in
  let requires = StringSet.diff requires provides in
  let primitives = t1.primitives @ t2.primitives in
  let crcs =
    StringMap.merge
      (fun _ v1 v2 ->
        match v1, v2 with
        | None, x -> x
        | x, None -> x
        | Some None, Some x -> Some x
        | Some x, Some None -> Some x
        | Some (Some x), Some (Some y) ->
            if String.equal x y
            then Some (Some x)
            else failwith (Printf.sprintf "Inconsistent assumption blah.."))
      t1.crcs
      t2.crcs
  in
  { provides
  ; requires
  ; primitives
  ; force_link = t1.force_link || t2.force_link
  ; effects_without_cps = t1.effects_without_cps || t2.effects_without_cps
  ; crcs
  }

let prefix = "//# unitInfo:"

let to_string t =
  [ [ prefix; "Provides:"; String.concat ~sep:", " (StringSet.elements t.provides) ]
  ; (if StringSet.equal empty.requires t.requires
     then []
     else [ prefix; "Requires:"; String.concat ~sep:", " (StringSet.elements t.requires) ])
  ; (if List.equal ~eq:String.equal empty.primitives t.primitives
     then []
     else [ prefix; "Primitives:"; String.concat ~sep:", " t.primitives ])
  ; (if Bool.equal empty.force_link t.force_link
     then []
     else [ prefix; "Force_link:"; string_of_bool t.force_link ])
  ; (if Bool.equal empty.effects_without_cps t.effects_without_cps
     then []
     else [ prefix; "Effects_without_cps:"; string_of_bool t.effects_without_cps ])
  ]
  |> List.filter_map ~f:(function
       | [] -> None
       | l -> Some (String.concat ~sep:" " l))
  |> String.concat ~sep:"\n"
  |> fun x -> x ^ "\n"

let parse_stringlist s =
  String.split_on_char ~sep:',' s
  |> List.filter_map ~f:(fun s ->
         match String.trim s with
         | "" -> None
         | s -> Some s)

let parse_stringset s = parse_stringlist s |> StringSet.of_list

let parse acc s =
  match String.drop_prefix ~prefix s with
  | None -> None
  | Some suffix -> (
      let suffix = String.trim suffix in
      match String.lsplit2 ~on:':' suffix with
      | None -> None
      | Some ("Provides", provides) ->
          Some
            { acc with
              provides = StringSet.union acc.provides (parse_stringset provides)
            }
      | Some ("Requires", requires) ->
          Some
            { acc with
              requires = StringSet.union acc.requires (parse_stringset requires)
            }
      | Some ("Primitives", primitives) ->
          Some { acc with primitives = acc.primitives @ parse_stringlist primitives }
      | Some ("Force_link", flink) ->
          Some
            { acc with force_link = bool_of_string (String.trim flink) || acc.force_link }
      | Some ("Effects_without_cps", b) ->
          Some { acc with effects_without_cps = bool_of_string (String.trim b) }
      | Some (_, _) -> None)
