(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

type value =
  | Bool of bool
  | String of string
  | Version of int * int * int

(* S-expressions, with the byte offsets of their first character and of the
   character following them *)
type sexp =
  | Atom of
      { text : string
      ; start : int
      ; stop : int
      }
  | List of
      { items : sexp list
      ; start : int
      ; stop : int
      }

exception Parse_error

let parse s =
  let len = String.length s in
  let is_at i c = i < len && Char.equal s.[i] c in
  let rec block_comment i depth =
    if i >= len
    then raise Parse_error
    else if is_at i ';' && is_at (i + 1) ')'
    then if depth = 1 then i + 2 else block_comment (i + 2) (depth - 1)
    else if is_at i '(' && is_at (i + 1) ';'
    then block_comment (i + 2) (depth + 1)
    else block_comment (i + 1) depth
  in
  let rec skip_blanks i =
    if i >= len
    then i
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> skip_blanks (i + 1)
      | ';' when is_at (i + 1) ';' -> (
          match String.index_from_opt s i '\n' with
          | None -> len
          | Some j -> skip_blanks (j + 1))
      | '(' when is_at (i + 1) ';' -> skip_blanks (block_comment (i + 2) 1)
      | _ -> i
  in
  let rec string_end i =
    if i >= len
    then raise Parse_error
    else
      match s.[i] with
      | '"' -> i + 1
      | '\\' -> string_end (i + 2)
      | _ -> string_end (i + 1)
  in
  let rec atom_end i =
    if i >= len
    then i
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' -> i
      | _ -> atom_end (i + 1)
  in
  let rec items i acc =
    let i = skip_blanks i in
    if i >= len || Char.equal s.[i] ')'
    then List.rev acc, i
    else
      match s.[i] with
      | '(' ->
          let l, j = items (i + 1) [] in
          if not (is_at j ')') then raise Parse_error;
          items (j + 1) (List { items = l; start = i; stop = j + 1 } :: acc)
      | '"' ->
          let j = string_end (i + 1) in
          items
            j
            (Atom { text = String.sub s ~pos:i ~len:(j - i); start = i; stop = j } :: acc)
      | _ ->
          let j = atom_end i in
          items
            j
            (Atom { text = String.sub s ~pos:i ~len:(j - i); start = i; stop = j } :: acc)
  in
  let l, i = items 0 [] in
  if i < len then raise Parse_error;
  l

(****)

(* The condition cannot be evaluated *)
exception Unsupported

let unescape s =
  (* [s] includes the quotes *)
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s - 1
    then
      match s.[i] with
      | '\\' when i + 1 < String.length s - 1 -> (
          match s.[i + 1] with
          | 'n' ->
              Buffer.add_char buf '\n';
              loop (i + 2)
          | 't' ->
              Buffer.add_char buf '\t';
              loop (i + 2)
          | ('"' | '\'' | '\\') as c ->
              Buffer.add_char buf c;
              loop (i + 2)
          | _ -> (
              match int_of_string_opt ("0x" ^ String.sub s ~pos:(i + 1) ~len:2) with
              | Some n when i + 3 < String.length s ->
                  Buffer.add_char buf (Char.chr n);
                  loop (i + 3)
              | _ -> raise Unsupported))
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 1;
  Buffer.contents buf

let is_unsigned_integer s =
  (not (String.is_empty s)) && String.for_all s ~f:(fun c -> Char.(c >= '0' && c <= '9'))

let lookup variables name =
  match List.string_assoc name variables with
  | Some v -> v
  | None -> (
      let name' = String.map name ~f:(fun c -> if Char.equal c '-' then '_' else c) in
      match List.string_assoc name' variables with
      | Some v -> v
      | None -> raise Unsupported)

let compare_values v v' =
  match v, v' with
  | Bool b, Bool b' -> Bool.compare b b'
  | String s, String s' -> String.compare s s'
  | Version (a, b, c), Version (a', b', c') -> Poly.compare (a, b, c) (a', b', c')
  | (Bool _ | String _ | Version _), _ -> raise Unsupported

let rec eval variables e =
  match e with
  | Atom { text; _ } when String.length text > 1 && Char.equal text.[0] '$' ->
      lookup variables (String.sub text ~pos:1 ~len:(String.length text - 1))
  | Atom { text; _ } when String.length text >= 2 && Char.equal text.[0] '"' ->
      String (unescape text)
  | List
      { items = [ Atom { text = a; _ }; Atom { text = b; _ }; Atom { text = c; _ } ]; _ }
    when is_unsigned_integer a && is_unsigned_integer b && is_unsigned_integer c ->
      Version (int_of_string a, int_of_string b, int_of_string c)
  | List { items = Atom { text = "and"; _ } :: l; _ } ->
      Bool (List.for_all ~f:(fun e -> eval_bool variables e) l)
  | List { items = Atom { text = "or"; _ } :: l; _ } ->
      Bool (List.exists ~f:(fun e -> eval_bool variables e) l)
  | List { items = [ Atom { text = "not"; _ }; e ]; _ } ->
      Bool (not (eval_bool variables e))
  | List
      { items = [ Atom { text = ("=" | "<>" | "<" | "<=" | ">" | ">=") as op; _ }; e; e' ]
      ; _
      } ->
      let c = compare_values (eval variables e) (eval variables e') in
      Bool
        (match op with
        | "=" -> c = 0
        | "<>" -> c <> 0
        | "<" -> c < 0
        | "<=" -> c <= 0
        | ">" -> c > 0
        | _ -> c >= 0)
  | Atom _ | List _ -> raise Unsupported

and eval_bool variables e =
  match eval variables e with
  | Bool b -> b
  | String _ | Version _ -> raise Unsupported

(****)

let specialize ~variables s =
  match parse s with
  | exception Parse_error -> s
  | l ->
      let buf = Buffer.create (String.length s) in
      let pos = ref 0 in
      let copy upto =
        Buffer.add_substring buf s !pos (upto - !pos);
        pos := upto
      in
      (* Blank the text, keeping the line breaks *)
      let skip upto =
        for i = !pos to upto - 1 do
          Buffer.add_char buf (if Char.equal s.[i] '\n' then '\n' else ' ')
        done;
        pos := upto
      in
      let branch e =
        match e with
        | List
            { items =
                Atom { text = ("@then" | "@else") as kind; stop = body_start; _ } :: body
            ; stop
            ; _
            } -> Some (kind, body_start, body, stop - 1)
        | Atom _ | List _ -> None
      in
      let rec walk e =
        match e with
        | Atom _ -> ()
        | List { items = Atom { text = "@if"; _ } :: cond :: branches; start; stop } -> (
            let selected =
              match List.map ~f:branch branches with
              | [ Some ("@then", _, _, _) ]
              | [ Some ("@then", _, _, _); Some ("@else", _, _, _) ] -> (
                  match eval_bool variables cond with
                  | true -> `Selected (Some (List.hd branches))
                  | false -> `Selected (List.nth_opt branches 1)
                  | exception Unsupported -> `Keep)
              | _ -> `Keep
            in
            match selected with
            | `Keep -> walk_children e
            | `Selected selected -> (
                copy start;
                match Option.bind selected ~f:branch with
                | None -> skip stop
                | Some (_, body_start, body, body_stop) ->
                    skip body_start;
                    List.iter ~f:walk body;
                    copy body_stop;
                    skip stop))
        | List _ -> walk_children e
      and walk_children e =
        match e with
        | List { items; _ } -> List.iter ~f:walk items
        | Atom _ -> ()
      in
      List.iter ~f:walk l;
      copy (String.length s);
      Buffer.contents buf
