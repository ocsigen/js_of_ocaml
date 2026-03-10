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

let proxy () = make Top

let top = proxy ()

let block fields = make (Block fields)

let funct ~arity ~pure ~res = make (Function { arity; pure; res })

module Set = Set.Make (struct
  type nonrec t = t

  let compare a b = Int.compare a.id b.id
end)

let to_string (shape : t) =
  let counts = Int.Hashtbl.create 17 in
  let rec count (s : t) =
    let n = try Int.Hashtbl.find counts s.id with Not_found -> 0 in
    Int.Hashtbl.replace counts s.id (n + 1);
    if n = 0
    then
      match s.desc with
      | Top -> ()
      | Function { res; _ } -> count res
      | Block fields -> List.iter ~f:count fields
  in
  count shape;
  let names = Int.Hashtbl.create 17 in
  let next_name = ref 0 in
  let buf = Buffer.create 64 in
  let rec emit (s : t) =
    let desc = s.desc in
    match desc with
    | Top -> Buffer.add_char buf 'N'
    | _ -> (
        let multi = Int.Hashtbl.find counts s.id > 1 in
        match if multi then Int.Hashtbl.find_opt names s.id else None with
        | Some name -> Buffer.add_string buf (Printf.sprintf "$%d" name)
        | None -> (
            if multi
            then begin
              let name = !next_name in
              incr next_name;
              Int.Hashtbl.replace names s.id name;
              Buffer.add_string buf (Printf.sprintf "#%d=" name)
            end;
            match desc with
            | Top -> assert false
            | Function { arity; pure; res } -> (
                Buffer.add_string buf (Printf.sprintf "F(%d)" arity);
                if pure then Buffer.add_char buf '*';
                match res.desc with
                | Top -> ()
                | _ ->
                    Buffer.add_string buf "->";
                    emit res)
            | Block fields ->
                Buffer.add_char buf '[';
                List.iteri
                  ~f:(fun i x ->
                    if i > 0 then Buffer.add_char buf ',';
                    emit x)
                  fields;
                Buffer.add_char buf ']'))
  in
  emit shape;
  Buffer.contents buf

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
  let ref_table = Int.Hashtbl.create 17 in
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
    | '#' ->
        next ();
        let parsed_id = parse_int 0 in
        parse_char '=';
        let shape_proxy = proxy () in
        Int.Hashtbl.replace ref_table parsed_id shape_proxy;
        let actual_shape = parse_shape () in
        shape_proxy.desc <- actual_shape.desc;
        shape_proxy
    | '$' ->
        next ();
        let parsed_id = parse_int 0 in
        Int.Hashtbl.find ref_table parsed_id
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
