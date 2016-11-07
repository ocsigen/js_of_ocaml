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

type pos = {
  mutable p_line : int;
  mutable p_col : int
}

type elt =
    Text of string
  | Break of string * int
  | Start_group of int
  | End_group
  | Set_pos of pos

type t =
  { mutable indent : int;
    mutable box_indent : int;
    mutable prev_indents : (int * int) list;
    mutable limit : int;
    mutable cur : int;

    mutable l : elt list;

    mutable n : int;
    mutable w : int;

    mutable compact : bool;

    mutable needed_space : (char -> char -> bool) option;
    mutable pending_space : string option;
    mutable last_char : char option;
    mutable line : int;
    mutable col : int;
    mutable total : int;
    output : string -> int -> int -> unit }

let spaces = String.make 80 ' '
let output st (s : string) l =
  (try
     let last = String.rindex_from s (l-1) '\n' + 1 in
     let line = ref 0 in
     for i = 0 to l-1 do
       if String.get s i = '\n' then incr line;
     done;
     st.line <- st.line + !line;
     st.col <- l - last
   with Not_found -> st.col <- l + st.col);
  st.total <- st.total + String.length s;
  st.output s 0 l


let rec output_spaces st n =
  output st spaces (min n 80);
  if n > 80 then output_spaces st (n - 80)

let output_newline st = output st "\n" 1

let rec flat_render st l =
  match l with
  | Text s :: r | Break (s, _) :: r ->
      output st s (String.length s); flat_render st r
  | Set_pos p :: r ->
    p.p_line <- st.line;
    p.p_col  <- st.col;
    flat_render st r
  | (Start_group _| End_group) :: r ->
     flat_render st r
  | [] ->
      ()

let rec push st e =
  if st.n = 0 then begin
    (* Vertical rendering *)
    match e with
      Text s ->
        output st s (String.length s);
        st.cur <- st.cur + String.length s
    | Set_pos p ->
      p.p_line <- st.line;
      p.p_col  <- st.col
    | Break (_, offs) ->
        output_newline st;
        let indent = st.box_indent + offs in
        st.indent <- indent;
        output_spaces st indent;
        st.limit <- max (indent + 60) 78;
        st.cur <- st.indent
    | Start_group n ->
        st.n <- 1;
        st.w <- st.limit - st.cur;
        st.prev_indents <- (st.box_indent, st.indent) :: st.prev_indents;
        st.indent <- st.indent + n;
        st.box_indent <- st.indent
    | End_group ->
        st.box_indent <- fst (List.hd st.prev_indents);
        st.indent <- snd (List.hd st.prev_indents);
        st.prev_indents <- List.tl st.prev_indents
  end else begin
    (* Fits? *)
    st.l <- e :: st.l;
    match e with
      Text s | Break (s, _) ->
        let w = st.w - String.length s in
        st.w <- w;
        if w < 0 then begin
          let l = List.rev st.l in
          st.l <- [];
          st.n <- 0;
          List.iter (fun e -> push st e) l
        end
    | Set_pos _ -> ()
    | Start_group _ ->
        st.n <- st.n + 1
    | End_group ->
        st.n <- st.n - 1;
        if st.n = 0 then begin
          flat_render st (List.rev st.l);
          st.box_indent <- fst (List.hd st.prev_indents);
          st.indent <- snd (List.hd st.prev_indents);
          st.prev_indents <- List.tl st.prev_indents;
          st.cur <- st.cur + st.w;
          st.l <- []
        end
  end

(****)

let string st (s : string) =
  if st.compact then (
    let len = (String.length s) in
    if len <> 0
    then begin
      (match st.pending_space with
        | None -> ()
        | Some sp ->
          begin
            st.pending_space <- None;
            match st.last_char,st.needed_space with
              | Some last,Some f ->
                if  f last s.[0]
                then output st sp 1
              | _, None -> output st sp 1
              | _ ->()
          end);

      output st s len;
      st.last_char <- Some (s.[len-1])
    end
  )
  else push st (Text s)

let genbreak st s n =
  if not st.compact then push st (Break (s, n))

let break_token = Break ("", 0)
let break st = if not st.compact then push st break_token
let break1 st = if not st.compact then push st (Break ("", 1))

let non_breaking_space_token = Text " "
let non_breaking_space st = if st.compact then st.pending_space <- Some " " else push st non_breaking_space_token
let space ?(indent=0) st = if st.compact then st.pending_space <- Some "\n" else push st (Break (" ", indent))

let start_group st n = if not st.compact then push st (Start_group n)
let end_group st = if not st.compact then push st End_group

(*

let render l =
  let st = { indent = 0; box_indent = 0; prev_indents = [];
             limit = 78; cur = 0; l = []; n = 0; w = 0;
             output = fun s i l -> output stdout s i l } in
  push st (Start_group 0);
  List.iter (fun e -> push st e) l;
  push st End_group;
  output_newline st

let rec tree n =
  if n = 0 then [Text "Leaf"] else
  [Start_group 10; Text "Node.... ("] @ tree (n - 1) @
  [Text ","; Break (" ", 0)] @ tree (n - 1) @ [Text ")"; End_group]

let _ =
for i = 1 to 10 do render (tree i) done

*)

let total t = t.total

let pos t =
  if t.compact
  then {
    p_line = t.line;
    p_col  = t.col
  }
  else
    let p = { p_line = -1 ; p_col = -1 } in
    push t (Set_pos p);
    p

let newline st =
  output_newline st;
  st.indent <- 0; st.box_indent <- 0; st.prev_indents <- [];
  st.cur <- 0; st.l <- []; st.n <- 0; st.w <- 0

(* hack on*)
let output_substring = Pervasives.output
(* for ocaml <  4.02, output_substring will be Pervasives.output (above)
   for ocaml >= 4.02, output_substring will be taken from the locally
                      open Pervasives module *)
let _ = output_substring
let output_substring =
  let open Pervasives in
  output_substring
(* hack off*)

let to_out_channel ch =
  { indent = 0; box_indent = 0; prev_indents = [];
    limit = 78; cur = 0; l = []; n = 0; w = 0;
    col = 0; line = 0; total = 0;
    compact = false; pending_space = None; last_char = None; needed_space = None;
    output = output_substring ch
  }

let to_buffer b =
  { indent = 0; box_indent = 0; prev_indents = [];
    limit = 78; cur = 0; l = []; n = 0; w = 0;
    col = 0; line = 0; total = 0;
    compact = false; pending_space = None; last_char = None; needed_space = None;
    output = fun s i l -> Buffer.add_substring b s i l }

let set_compact st v = st.compact <- v

let set_needed_space_function st f = st.needed_space <- Some f
