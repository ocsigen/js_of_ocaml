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

type desc =
  | Top
  | Block of t list
  | Function of
      { arity : int
      ; pure : bool
      ; res : t
      }

and t =
  { id : int
  ; mutable desc : desc
  }

let next_id = ref 0

let fresh_id () =
  let id = !next_id in
  incr next_id;
  id

let make desc = { id = fresh_id (); desc }

let top = make Top

let block fields = make (Block fields)

let funct ~arity ~pure ~res = make (Function { arity; pure; res })

let rec merge (u : t) (v : t) =
  match u.desc, v.desc with
  | ( Function { arity = a1; pure = p1; res = r1 }
    , Function { arity = a2; pure = p2; res = r2 } ) ->
      if a1 = a2 then funct ~arity:a1 ~pure:(p1 && p2) ~res:(merge r1 r2) else top
  | Block b1, Block b2 ->
      if List.compare_lengths b1 b2 = 0 then block (List.map2 b1 b2 ~f:merge) else top
  | Top, _ | _, Top -> top
  | Function _, Block _ | Block _, Function _ -> top

let rec to_string (shape : t) =
  match shape.desc with
  | Top -> "N"
  | Block l -> "[" ^ String.concat ~sep:"," (List.map ~f:to_string l) ^ "]"
  | Function { arity; pure; res } ->
      Printf.sprintf
        "F(%d)%s%s"
        arity
        (if pure then "*" else "")
        (match res.desc with
        | Top -> ""
        | _ -> "->" ^ to_string res)

let of_string (s : string) =
  let pos = ref 0 in
  let current () = s.[!pos] in
  let next () = incr pos in
  let parse_char c =
    let c' = current () in
    if Char.equal c c' then next () else assert false
  in
  let parse_char_opt c =
    let c' = current () in
    if Char.equal c c'
    then (
      next ();
      true)
    else false
  in
  let rec parse_int acc =
    match current () with
    | '0' .. '9' as c ->
        let d = Char.code c - Char.code '0' in
        let acc = (acc * 10) + d in
        next ();
        parse_int acc
    | _ -> acc
  in
  let rec parse_shape () =
    match current () with
    | '[' ->
        next ();
        parse_block []
    | 'N' ->
        next ();
        top
    | 'F' ->
        next ();
        parse_fun ()
    | _ -> assert false
  and parse_block acc =
    match current () with
    | ']' ->
        next ();
        block (List.rev acc)
    | _ -> (
        let x = parse_shape () in
        match current () with
        | ',' ->
            next ();
            parse_block (x :: acc)
        | ']' ->
            next ();
            block (List.rev (x :: acc))
        | _ -> assert false)
  and parse_fun () =
    let () = parse_char '(' in
    let arity = parse_int 0 in
    let () = parse_char ')' in
    let pure = parse_char_opt '*' in
    match current () with
    | '-' ->
        next ();
        parse_char '>';
        let res = parse_shape () in
        funct ~arity ~pure ~res
    | _ -> funct ~arity ~pure ~res:top
  in
  parse_shape ()

module Store = struct
  let t = String.Hashtbl.create 17

  let set ~name shape = String.Hashtbl.replace t name shape

  let get ~name = String.Hashtbl.find_opt t name

  let load' fn =
    let l = file_lines_bin fn in
    List.iter l ~f:(fun s ->
        match String.drop_prefix ~prefix:"//# shape: " s with
        | None -> ()
        | Some name_n_shape -> (
            match String.lsplit2 name_n_shape ~on:':' with
            | None -> ()
            | Some (name, shape) -> set ~name (of_string shape)))

  let load ~name = if String.Hashtbl.mem t name then get ~name else None
end

module State = struct
  type nonrec t =
    { table : t Code.Var.Hashtbl.t
    ; cache : BitSet.t
    }

  let t : t = { table = Code.Var.Hashtbl.create 17; cache = BitSet.create () }

  let assign x shape =
    Code.Var.Hashtbl.replace t.table x shape;
    BitSet.set t.cache (Code.Var.idx x)

  let propagate x offset target =
    match Code.Var.Hashtbl.find_opt t.table x with
    | None -> ()
    | Some shape -> (
        match shape.desc with
        | Top | Function _ -> ()
        | Block fields -> assign target (List.nth fields offset))

  let mem x = BitSet.mem t.cache (Code.Var.idx x)

  let get x = if mem x then Code.Var.Hashtbl.find_opt t.table x else None

  let is_pure_fun x =
    match Code.Var.Hashtbl.find_opt t.table x with
    | None -> false
    | Some shape -> (
        match shape.desc with
        | Top | Block _ -> false
        | Function { pure; _ } -> pure)

  let reset () =
    Code.Var.Hashtbl.clear t.table;
    BitSet.clear t.cache
end
