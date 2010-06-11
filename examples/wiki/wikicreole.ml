# 1 "wikicreole.mll"
 
(* Ocsimore
 * Copyright (C) 2008
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   Parser for Wikicreole
   @author Jérôme Vouillon
*)

type ('flow, 'inline, 'a_content) builder =
  { chars : string -> 'a_content;
    strong_elem : 'inline list -> 'a_content;
    em_elem : 'inline list -> 'a_content;
    br_elem : unit -> 'a_content;
    img_elem : string -> string -> 'a_content;
    tt_elem : 'inline list -> 'a_content;
    a_elem : string -> 'a_content list -> 'inline;
    p_elem : 'inline list -> 'flow;
    pre_elem : string list -> 'flow;
    h1_elem : 'inline list -> 'flow;
    h2_elem : 'inline list -> 'flow;
    h3_elem : 'inline list -> 'flow;
    h4_elem : 'inline list -> 'flow;
    h5_elem : 'inline list -> 'flow;
    h6_elem : 'inline list -> 'flow;
    ul_elem : ('inline list * 'flow option) list -> 'flow;
    ol_elem : ('inline list * 'flow option) list -> 'flow;
    hr_elem : unit -> 'flow;
    table_elem : (bool * 'inline list) list list -> 'flow;
    inline : 'a_content -> 'inline }

type style = Bold | Italic

type list_kind = Unordered | Ordered

type ('inline, 'flow) stack =
    Style of style * 'inline list * ('inline, 'flow) stack
  | Link of string * ('inline, 'flow) stack
      (* Not that we do not save anything in the case of links, as
         links cannot be nested *)
  | Paragraph
  | Heading of int
  | List_item of ('inline, 'flow) stack
  | List of
      list_kind * ('inline list * 'flow option) list * ('inline, 'flow) stack
  | Table of (bool * 'inline list) list list
  | Row of (bool * 'inline list) list * ('inline, 'flow) stack
  | Entry of bool * ('inline, 'flow) stack

type ('flow, 'inline, 'a_content) ctx =
  { build : ('flow, 'inline, 'a_content) builder;
    mutable italic : bool;
    mutable bold : bool;
    mutable heading : bool;
    mutable link : bool;
    mutable list_level : int;
    mutable inline_mix : 'inline list;
    mutable link_content : 'a_content list;
    mutable pre_content : string list;
    mutable list : ('inline list * 'flow option) list;
    mutable flow : 'flow list;
    mutable stack : ('inline, 'flow) stack }

let count c s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do if s.[i] = c then incr n done;
  !n

let push c v =
  match c.stack with
    Link _ -> c.link_content <- v :: c.link_content
  | _      -> c.inline_mix <- c.build.inline v :: c.inline_mix

let push_string c s = push c (c.build.chars s)

let push_chars c lexbuf = push_string c (Lexing.lexeme lexbuf)

let get_style c style =
  match style with Bold -> c.bold | Italic -> c.italic

let set_style c style v =
  match style with Bold -> c.bold <- v | Italic -> c.italic <- v

let pop_style c style inline stack =
  let elt =
    match style with
      Bold   -> c.build.strong_elem
    | Italic -> c.build.em_elem
  in
  let inline' = c.inline_mix in
  c.stack <- stack;
  c.inline_mix <- inline;
  push c (elt (List.rev inline'));
  set_style c style false

let style_change c style =
  if get_style c style then begin
    match c.stack with
      Style (s, inline, stack) when s = style ->
        pop_style c style inline stack
    | _ ->
        push_string c "**"
  end else begin
    c.stack <- Style (style, c.inline_mix, c.stack);
    c.inline_mix <- [];
    set_style c style true
  end

let pop_link c addr stack =
  c.stack <- stack;
  c.inline_mix <-
    c.build.a_elem addr (List.rev c.link_content) :: c.inline_mix;
  c.link_content <- [];
  c.link <- false

let close_entry c =
  match c.stack with
    Entry (heading, Row (entries, stack)) ->
      c.stack <- Row ((heading, List.rev c.inline_mix) :: entries, stack);
      c.inline_mix <- [];
      true
  | Row _ ->
      true
  | Table _ ->
      c.stack <- Row ([(false, List.rev c.inline_mix)], c.stack);
      c.inline_mix <- [];
      true
  | _ ->
      false

let close_row c =
  close_entry c &&
  match c.stack with
    Row (entries, Table rows) ->
      c.stack <- Table (List.rev entries :: rows);
      true
  | Table _ ->
      true
  | _ ->
      assert false

let rec end_paragraph c lev =
  match c.stack with
    Style (style, inline, stack) ->
      pop_style c style inline stack;
      end_paragraph c lev
  | Link (addr, stack) ->
      pop_link c addr stack;
      end_paragraph c lev
  | Paragraph ->
      if c.inline_mix <> [] then begin
        c.flow <- c.build.p_elem (List.rev c.inline_mix) :: c.flow;
        c.inline_mix <- []
      end
  | Heading l ->
      let f =
        match l with
          | 1 -> c.build.h1_elem
          | 2 -> c.build.h2_elem
          | 3 -> c.build.h3_elem
          | 4 -> c.build.h4_elem
          | 5 -> c.build.h5_elem
          | _ -> c.build.h6_elem
      in
      c.flow <- f (List.rev c.inline_mix) :: c.flow;
      c.inline_mix <- [];
      c.heading <- false;
      c.stack <- Paragraph
  | List_item stack ->
      c.list <- (List.rev c.inline_mix, None) :: c.list;
      c.stack <- stack;
      c.inline_mix <- [];
      end_paragraph c lev
  | List (kind, lst, stack) ->
      if lev < c.list_level then begin
        c.list_level <- c.list_level - 1;
        let elt =
          match kind with
            Unordered -> c.build.ul_elem
          | Ordered   -> c.build.ol_elem
        in
        let cur_lst = elt (List.rev c.list) in
        if c.list_level = 0 then
          c.flow <- cur_lst :: c.flow
        else begin
          match lst with
            (l, None) :: rem -> c.list <- (l, Some cur_lst) :: rem;
          | _                -> assert false
        end;
        c.stack <- stack;
        end_paragraph c lev
      end
  | Entry _ ->
      ignore (close_row c);
      end_paragraph c lev
  | Row _ ->
      assert false
  | Table rows ->
      c.flow <- c.build.table_elem (List.rev rows) :: c.flow;
      c.stack <- Paragraph

let rec correct_kind_rec stack kind n =
  match stack with
    List_item stack ->
      correct_kind_rec stack kind n
  | List (k, lst, stack) ->
      if n = 0 then k = kind else
      correct_kind_rec stack kind (n - 1)
  | Style (_, _, stack) ->
      correct_kind_rec stack kind n
  | Link _ | Heading _ | Paragraph | Entry _ | Row _ | Table _ ->
      assert false

let correct_kind c kind lev =
  lev = c.list_level + 1
    ||
  (lev <= c.list_level &&
   correct_kind_rec c.stack kind (c.list_level - lev))

let start_list_item c kind lev =
  let correct = correct_kind c kind lev in
  if lev = 1 || correct then begin
    (* If we have an item of a different kind at level 1, we close the
       previous list and start a new one of the right kind *)
    end_paragraph c (if correct then lev else 0);
    if lev = c.list_level then begin
      c.stack <- List_item c.stack
    end else (* if lev = c.list_level + 1 then *) begin
      c.list_level <- lev;
      c.stack <- List_item (List (kind, c.list, c.stack));
      c.list <- []
    end;
    true
  end else
    false

let start_table_row c heading =
  if not (close_row c) then begin
    end_paragraph c 0;
    c.stack <- Table []
  end;
  c.stack <- Entry (heading, Row ([], c.stack))


# 262 "wikicreole.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\001\000\002\000\001\000\001\000\001\000\002\000\005\000\
    \001\000\255\255\003\000\004\000\006\000\007\000\254\255\003\000\
    \004\000\006\000\251\255\002\000\003\000\007\000\250\255\008\000\
    \248\255\011\000\238\255\047\000\020\000\046\000\070\000\085\000\
    \108\000\155\000\193\000\208\000\008\001\025\001\077\001\012\000\
    \255\255\254\255\253\255\252\255\013\000\083\001\064\000\066\000\
    \074\000\250\255\120\000\251\255\249\255\130\001\170\001\186\001\
    \032\002\048\002\105\002\087\002\130\002\147\002\247\255\106\000\
    \031\000\080\000\097\000\135\000\246\255\173\000\182\000\011\000\
    \244\255\241\255\243\255\015\000\210\000\039\001\016\000\253\255\
    \171\000\254\255\204\000\113\001\215\000\226\000\239\000\255\255\
    \017\000\014\001\243\000\018\000";
  Lexing.lex_backtrk = 
   "\008\000\006\000\255\255\255\255\003\000\002\000\001\000\255\255\
    \000\000\255\255\001\000\001\000\001\000\001\000\255\255\255\255\
    \255\255\255\255\255\255\004\000\255\255\255\255\255\255\005\000\
    \255\255\255\255\255\255\015\000\013\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\000\000\
    \255\255\255\255\255\255\255\255\003\000\015\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\015\000\015\000\015\000\
    \015\000\007\000\007\000\255\255\015\000\015\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\010\000\010\000\
    \255\255\255\255\255\255\012\000\255\255\255\255\002\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\001\000";
  Lexing.lex_default = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \000\000\027\000\000\000\027\000\255\255\072\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\255\255\
    \000\000\000\000\000\000\000\000\255\255\027\000\047\000\047\000\
    \047\000\000\000\047\000\000\000\000\000\027\000\027\000\027\000\
    \058\000\057\000\058\000\057\000\027\000\027\000\000\000\065\000\
    \064\000\065\000\066\000\066\000\000\000\064\000\064\000\255\255\
    \000\000\000\000\000\000\255\255\255\255\080\000\255\255\000\000\
    \080\000\000\000\080\000\080\000\080\000\080\000\080\000\000\000\
    \255\255\080\000\080\000\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\009\000\009\000\018\000\008\000\007\000\017\000\
    \018\000\022\000\022\000\019\000\023\000\040\000\040\000\043\000\
    \039\000\074\000\079\000\087\000\081\000\076\000\074\000\000\000\
    \007\000\075\000\000\000\004\000\004\000\007\000\017\000\000\000\
    \004\000\255\255\005\000\005\000\255\255\003\000\015\000\005\000\
    \016\000\017\000\003\000\000\000\076\000\038\000\000\000\255\255\
    \255\255\255\255\037\000\255\255\255\255\006\000\024\000\010\000\
    \011\000\012\000\006\000\013\000\014\000\000\000\000\000\000\000\
    \036\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\073\000\000\000\255\255\255\255\000\000\000\000\255\255\
    \000\000\255\255\255\255\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\035\000\031\000\
    \034\000\000\000\000\000\255\255\255\255\000\000\255\255\000\000\
    \255\255\032\000\000\000\033\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\002\000\001\000\020\000\021\000\255\255\
    \002\000\001\000\255\255\255\255\255\255\255\255\030\000\028\000\
    \071\000\029\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\255\255\069\000\048\000\000\000\050\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\062\000\255\255\000\000\000\000\079\000\000\000\255\255\
    \078\000\000\000\255\255\255\255\255\255\255\255\049\000\000\000\
    \255\255\063\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\066\000\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\051\000\079\000\000\000\
    \255\255\078\000\255\255\076\000\074\000\255\255\067\000\075\000\
    \060\000\079\000\000\000\000\000\078\000\064\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\079\000\000\000\000\000\078\000\
    \255\255\000\000\076\000\000\000\255\255\000\000\255\255\255\255\
    \255\255\087\000\255\255\000\000\088\000\081\000\255\255\255\255\
    \091\000\255\255\000\000\255\255\068\000\000\000\018\000\022\000\
    \000\000\000\000\000\000\026\000\000\000\255\255\000\000\053\000\
    \000\000\045\000\043\000\000\000\074\000\044\000\255\255\255\255\
    \079\000\255\255\000\000\078\000\255\255\255\255\052\000\255\255\
    \000\000\000\000\000\000\255\255\000\000\000\000\255\255\255\255\
    \045\000\255\255\070\000\046\000\255\255\255\255\255\255\255\255\
    \083\000\079\000\255\255\071\000\078\000\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\036\000\255\255\083\000\
    \042\000\089\000\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\085\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\045\000\043\000\000\000\086\000\
    \044\000\255\255\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\045\000\255\255\255\255\255\255\041\000\
    \255\255\000\000\083\000\079\000\255\255\255\255\078\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \000\000\000\000\255\255\090\000\255\255\000\000\000\000\255\255\
    \255\255\083\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\082\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\079\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\000\000\000\000\000\000\255\255\
    \000\000\255\255\000\000\000\000\255\255\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\079\000\000\000\255\255\255\255\
    \255\255\255\255\074\000\000\000\255\255\000\000\000\000\079\000\
    \000\000\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\079\000\000\000\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\000\000\000\000\084\000\087\000\
    \000\000\000\000\000\000\081\000\056\000\000\000\054\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \043\000\000\000\000\000\000\000\000\000\000\000\079\000\000\000\
    \255\255\000\000\255\255\000\000\000\000\255\255\255\255\255\255\
    \000\000\255\255\055\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\255\255\000\000\000\000\255\255\255\255\081\000\
    \255\255\027\000\255\255\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\000\000\255\255\000\000\000\000\
    \027\000\056\000\056\000\000\000\000\000\000\000\000\000\056\000\
    \000\000\000\000\057\000\000\000\056\000\255\255\056\000\057\000\
    \255\255\059\000\059\000\043\000\000\000\000\000\000\000\059\000\
    \000\000\000\000\056\000\056\000\059\000\057\000\059\000\056\000\
    \255\255\255\255\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\059\000\059\000\000\000\000\000\000\000\059\000\
    \000\000\079\000\027\000\255\255\000\000\000\000\255\255\255\255\
    \059\000\059\000\000\000\057\000\057\000\057\000\059\000\000\000\
    \000\000\000\000\255\255\059\000\000\000\059\000\057\000\000\000\
    \057\000\027\000\056\000\056\000\255\255\000\000\000\000\255\255\
    \056\000\059\000\059\000\057\000\000\000\056\000\059\000\056\000\
    \057\000\000\000\000\000\057\000\057\000\255\255\057\000\000\000\
    \255\255\000\000\000\000\056\000\056\000\000\000\057\000\000\000\
    \056\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\255\255\000\000\057\000\057\000\057\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\056\000\000\000\057\000\
    \255\255\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\057\000\057\000\000\000\057\000\
    \255\255\000\000\255\255\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\061\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\008\000\019\000\000\000\007\000\017\000\
    \017\000\021\000\023\000\017\000\021\000\025\000\039\000\044\000\
    \025\000\075\000\078\000\088\000\091\000\028\000\028\000\255\255\
    \000\000\028\000\255\255\000\000\004\000\007\000\017\000\255\255\
    \007\000\064\000\000\000\005\000\064\000\000\000\003\000\007\000\
    \015\000\016\000\007\000\255\255\028\000\025\000\255\255\029\000\
    \029\000\027\000\025\000\029\000\027\000\000\000\001\000\006\000\
    \010\000\011\000\007\000\012\000\013\000\255\255\255\255\255\255\
    \025\000\255\255\046\000\255\255\047\000\046\000\029\000\047\000\
    \030\000\028\000\255\255\030\000\048\000\255\255\255\255\048\000\
    \255\255\027\000\065\000\255\255\255\255\065\000\027\000\031\000\
    \255\255\255\255\031\000\255\255\255\255\255\255\025\000\025\000\
    \025\000\255\255\255\255\066\000\027\000\255\255\066\000\255\255\
    \030\000\025\000\255\255\025\000\063\000\030\000\032\000\063\000\
    \255\255\032\000\255\255\000\000\000\000\002\000\020\000\031\000\
    \007\000\007\000\050\000\030\000\031\000\050\000\025\000\025\000\
    \071\000\025\000\027\000\027\000\027\000\255\255\255\255\255\255\
    \255\255\067\000\031\000\255\255\067\000\027\000\032\000\027\000\
    \255\255\255\255\255\255\032\000\064\000\046\000\255\255\047\000\
    \255\255\030\000\030\000\030\000\255\255\033\000\255\255\048\000\
    \033\000\032\000\027\000\027\000\030\000\027\000\030\000\255\255\
    \031\000\031\000\031\000\255\255\255\255\080\000\255\255\069\000\
    \080\000\255\255\069\000\031\000\046\000\031\000\047\000\255\255\
    \070\000\030\000\030\000\070\000\030\000\033\000\048\000\032\000\
    \032\000\032\000\033\000\034\000\065\000\255\255\034\000\255\255\
    \031\000\031\000\032\000\031\000\032\000\050\000\082\000\255\255\
    \033\000\082\000\035\000\076\000\076\000\035\000\066\000\076\000\
    \032\000\084\000\255\255\255\255\084\000\063\000\063\000\032\000\
    \032\000\255\255\032\000\034\000\085\000\255\255\255\255\085\000\
    \034\000\255\255\076\000\255\255\050\000\255\255\033\000\033\000\
    \033\000\086\000\035\000\255\255\086\000\090\000\034\000\035\000\
    \090\000\033\000\255\255\033\000\067\000\255\255\017\000\021\000\
    \255\255\255\255\255\255\025\000\255\255\035\000\255\255\033\000\
    \255\255\036\000\036\000\255\255\028\000\036\000\033\000\033\000\
    \089\000\033\000\255\255\089\000\034\000\034\000\034\000\064\000\
    \255\255\255\255\255\255\037\000\255\255\255\255\037\000\034\000\
    \036\000\034\000\069\000\035\000\035\000\035\000\029\000\027\000\
    \077\000\077\000\036\000\070\000\077\000\255\255\035\000\036\000\
    \035\000\255\255\255\255\255\255\034\000\034\000\255\255\034\000\
    \046\000\255\255\047\000\037\000\255\255\036\000\030\000\077\000\
    \037\000\082\000\048\000\035\000\035\000\255\255\035\000\255\255\
    \065\000\255\255\255\255\255\255\084\000\031\000\037\000\038\000\
    \255\255\255\255\038\000\255\255\045\000\045\000\255\255\085\000\
    \045\000\066\000\255\255\036\000\036\000\036\000\255\255\255\255\
    \255\255\255\255\063\000\255\255\032\000\255\255\036\000\255\255\
    \036\000\255\255\255\255\045\000\037\000\037\000\037\000\038\000\
    \050\000\255\255\083\000\083\000\038\000\045\000\083\000\037\000\
    \255\255\037\000\045\000\036\000\036\000\255\255\036\000\067\000\
    \255\255\255\255\038\000\089\000\053\000\255\255\255\255\053\000\
    \045\000\083\000\255\255\255\255\037\000\037\000\255\255\037\000\
    \255\255\255\255\255\255\033\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\077\000\255\255\255\255\255\255\
    \038\000\038\000\038\000\080\000\053\000\069\000\045\000\045\000\
    \045\000\053\000\255\255\038\000\054\000\038\000\070\000\054\000\
    \255\255\045\000\255\255\045\000\255\255\255\255\255\255\053\000\
    \255\255\034\000\255\255\255\255\055\000\255\255\255\255\055\000\
    \038\000\038\000\255\255\038\000\082\000\255\255\045\000\045\000\
    \035\000\045\000\076\000\255\255\054\000\255\255\255\255\084\000\
    \255\255\054\000\255\255\255\255\255\255\053\000\053\000\053\000\
    \255\255\255\255\085\000\255\255\055\000\255\255\255\255\054\000\
    \053\000\055\000\053\000\255\255\255\255\255\255\083\000\086\000\
    \255\255\255\255\255\255\090\000\055\000\255\255\053\000\055\000\
    \255\255\255\255\255\255\255\255\255\255\053\000\053\000\255\255\
    \053\000\255\255\255\255\255\255\255\255\054\000\054\000\054\000\
    \036\000\255\255\255\255\255\255\255\255\255\255\089\000\255\255\
    \054\000\255\255\054\000\255\255\255\255\055\000\055\000\055\000\
    \255\255\037\000\054\000\255\255\255\255\255\255\255\255\255\255\
    \055\000\255\255\055\000\255\255\255\255\054\000\054\000\077\000\
    \054\000\056\000\056\000\255\255\255\255\056\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\055\000\055\000\255\255\
    \055\000\057\000\057\000\255\255\255\255\057\000\255\255\255\255\
    \056\000\056\000\056\000\255\255\255\255\255\255\255\255\056\000\
    \255\255\255\255\056\000\255\255\056\000\038\000\056\000\056\000\
    \057\000\057\000\057\000\045\000\255\255\255\255\255\255\057\000\
    \255\255\255\255\056\000\056\000\057\000\056\000\057\000\056\000\
    \059\000\059\000\255\255\255\255\059\000\255\255\255\255\255\255\
    \255\255\255\255\057\000\057\000\255\255\255\255\255\255\057\000\
    \255\255\083\000\058\000\058\000\255\255\255\255\058\000\059\000\
    \059\000\059\000\255\255\056\000\056\000\056\000\059\000\255\255\
    \255\255\255\255\053\000\059\000\255\255\059\000\056\000\255\255\
    \056\000\058\000\058\000\058\000\060\000\255\255\255\255\060\000\
    \058\000\059\000\059\000\058\000\255\255\058\000\059\000\058\000\
    \058\000\255\255\255\255\056\000\056\000\061\000\056\000\255\255\
    \061\000\255\255\255\255\058\000\058\000\255\255\058\000\255\255\
    \058\000\255\255\054\000\255\255\060\000\255\255\255\255\255\255\
    \255\255\060\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\055\000\255\255\255\255\061\000\255\255\060\000\
    \255\255\255\255\061\000\255\255\058\000\058\000\058\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\061\000\255\255\058\000\
    \061\000\058\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\060\000\060\000\060\000\
    \255\255\255\255\255\255\255\255\058\000\058\000\255\255\058\000\
    \060\000\255\255\060\000\255\255\255\255\255\255\061\000\061\000\
    \061\000\255\255\060\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\061\000\255\255\061\000\255\255\060\000\060\000\255\255\
    \060\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\061\000\061\000\
    \255\255\061\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \056\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \057\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\059\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\058\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\060\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\061\000";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec parse_bol c lexbuf =
    __ocaml_lex_parse_bol_rec c lexbuf 0
and __ocaml_lex_parse_bol_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 275 "wikicreole.mll"
               (
      end_paragraph c 0;
      parse_bol c lexbuf
    )
# 559 "wikicreole.ml"

  | 1 ->
# 279 "wikicreole.mll"
                                                                     (
      end_paragraph c 0;
      assert (c.stack = Paragraph);
      c.stack <- Heading (count '=' (Lexing.lexeme lexbuf));
      c.heading <- true;
      parse_rem c lexbuf
    )
# 570 "wikicreole.ml"

  | 2 ->
# 286 "wikicreole.mll"
                        (
      let lev = count '*' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Unordered lev) then begin
        let s = Lexing.lexeme lexbuf in
        let l = String.length s - lev in
        if l > 0 then push_string c (String.sub s 0 l);
        for i = 1 to lev / 2 do
          style_change c Bold
        done;
        if lev land 1 = 1 then push_string c "*"
      end;
      parse_rem c lexbuf
    )
# 587 "wikicreole.ml"

  | 3 ->
# 299 "wikicreole.mll"
                        (
      let lev = count '#' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Ordered lev) then
        push_chars c lexbuf;
      parse_rem c lexbuf
    )
# 597 "wikicreole.ml"

  | 4 ->
# 305 "wikicreole.mll"
                                                          (
      end_paragraph c 0;
      c.flow <- c.build.hr_elem () :: c.flow;
      parse_bol c lexbuf
    )
# 606 "wikicreole.ml"

  | 5 ->
# 310 "wikicreole.mll"
                                           (
      parse_nowiki c lexbuf
    )
# 613 "wikicreole.ml"

  | 6 ->
# 313 "wikicreole.mll"
                      (
      start_table_row c false;
      parse_rem c lexbuf
    )
# 621 "wikicreole.ml"

  | 7 ->
# 317 "wikicreole.mll"
                       (
      start_table_row c true;
      parse_rem c lexbuf
    )
# 629 "wikicreole.ml"

  | 8 ->
# 321 "wikicreole.mll"
       (
      parse_rem c lexbuf
    )
# 636 "wikicreole.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_bol_rec c lexbuf __ocaml_lex_state

and parse_rem c lexbuf =
    __ocaml_lex_parse_rem_rec c lexbuf 25
and __ocaml_lex_parse_rem_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 327 "wikicreole.mll"
               (
      (* Headings are single lines *)
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    )
# 654 "wikicreole.ml"

  | 1 ->
# 335 "wikicreole.mll"
         (
      style_change c Bold;
      parse_rem c lexbuf
    )
# 662 "wikicreole.ml"

  | 2 ->
# 339 "wikicreole.mll"
         (
      style_change c Italic;
      parse_rem c lexbuf
    )
# 670 "wikicreole.ml"

  | 3 ->
# 343 "wikicreole.mll"
                                           (
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    )
# 681 "wikicreole.ml"

  | 4 ->
# 350 "wikicreole.mll"
                                                       (
      if c.link then
        push_chars c lexbuf
      else
        let s = Lexing.lexeme lexbuf in
        let addr = String.sub s 2 (String.length s - 4) in
        c.inline_mix <-
         c.build.a_elem addr [c.build.chars addr] :: c.inline_mix;
      parse_rem c lexbuf
  )
# 695 "wikicreole.ml"

  | 5 ->
# 360 "wikicreole.mll"
                                                      (
      if c.link then
        push_chars c lexbuf
      else begin
        let s = Lexing.lexeme lexbuf in
        let addr = String.sub s 2 (String.length s - 3) in
        c.stack <- Link (addr, c.stack);
        c.link <- true
      end;
      parse_rem c lexbuf
  )
# 710 "wikicreole.ml"

  | 6 ->
# 371 "wikicreole.mll"
         (
      begin match c.stack with
        Link (addr, stack) ->
          pop_link c addr stack
      | _ ->
          push_chars c lexbuf
      end;
      parse_rem c lexbuf
    )
# 723 "wikicreole.ml"

  | 7 ->
# 381 "wikicreole.mll"
                                                 (
      if c.link then
        push_chars c lexbuf
      else
        let addr = Lexing.lexeme lexbuf in
        c.inline_mix <-
          c.build.a_elem addr [c.build.chars addr] :: c.inline_mix;
      parse_rem c lexbuf
  )
# 736 "wikicreole.ml"

  | 8 ->
# 390 "wikicreole.mll"
           (
      push c (c.build.br_elem ());
      parse_rem c lexbuf
    )
# 744 "wikicreole.ml"

  | 9 ->
# 395 "wikicreole.mll"
                                               (
      let s = Lexing.lexeme lexbuf in
      let i = String.index s '|' in
      let url = String.sub s 2 (i - 2) in
      let alt = String.sub s (i + 1) (String.length s - i - 3) in
      push c (c.build.img_elem url alt);
      parse_rem c lexbuf
    )
# 756 "wikicreole.ml"

  | 10 ->
# 403 "wikicreole.mll"
                                                            (
      let s = Lexing.lexeme lexbuf in
      let txt = String.sub s 3 (String.length s - 6) in
      push c (c.build.tt_elem [c.build.inline (c.build.chars txt)]);
      parse_rem c lexbuf
    )
# 766 "wikicreole.ml"

  | 11 ->
# 409 "wikicreole.mll"
                                       (
      let s = Lexing.lexeme lexbuf in
      (* It amounts to the same to quote a UTF-8 char or its first byte *)
      push_string c (String.sub s 1 1);
      parse_rem c lexbuf
    )
# 776 "wikicreole.ml"

  | 12 ->
# 415 "wikicreole.mll"
                                        (
      if not (close_row c) then
        push_chars c lexbuf;
      parse_bol c lexbuf
    )
# 785 "wikicreole.ml"

  | 13 ->
# 420 "wikicreole.mll"
        (
      if close_entry c then
        c.stack <- Entry (false, c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    )
# 796 "wikicreole.ml"

  | 14 ->
# 427 "wikicreole.mll"
         (
      if close_entry c then
        c.stack <- Entry (true, c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    )
# 807 "wikicreole.ml"

  | 15 ->
# 434 "wikicreole.mll"
                                  (
      push_chars c lexbuf;
      parse_rem c lexbuf
    )
# 815 "wikicreole.ml"

  | 16 ->
# 438 "wikicreole.mll"
      (
     Format.eprintf "Unrecognized char '%s'@." (Lexing.lexeme lexbuf);
     parse_rem c lexbuf
  )
# 823 "wikicreole.ml"

  | 17 ->
# 442 "wikicreole.mll"
        (
      end_paragraph c 0
    )
# 830 "wikicreole.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_rem_rec c lexbuf __ocaml_lex_state

and parse_nowiki c lexbuf =
    __ocaml_lex_parse_nowiki_rec c lexbuf 77
and __ocaml_lex_parse_nowiki_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 448 "wikicreole.mll"
                                           (
      let s = Lexing.lexeme lexbuf in
      c.pre_content <- String.sub s 1 (String.length s - 1) :: c.pre_content;
      parse_nowiki c lexbuf
    )
# 845 "wikicreole.ml"

  | 1 ->
# 453 "wikicreole.mll"
                                     (
      c.flow <- c.build.pre_elem (List.rev c.pre_content) :: c.flow;
      c.pre_content <- [];
      parse_bol c lexbuf
    )
# 854 "wikicreole.ml"

  | 2 ->
# 458 "wikicreole.mll"
                                        (
      c.pre_content <- Lexing.lexeme lexbuf :: c.pre_content;
      parse_nowiki c lexbuf
    )
# 862 "wikicreole.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_nowiki_rec c lexbuf __ocaml_lex_state

;;

# 463 "wikicreole.mll"
 

let context b =
  { build = b; italic = false; bold = false;
    heading = false; link = false; list_level = 0;
    inline_mix = []; link_content = []; pre_content = []; list = []; flow = [];
    stack = Paragraph }

let from_lexbuf b lexbuf =
  let c = context b in
  parse_bol c lexbuf;
  List.rev c.flow

let from_channel b ch = from_lexbuf b (Lexing.from_channel ch)

let from_string b s = from_lexbuf b (Lexing.from_string s)


# 887 "wikicreole.ml"
