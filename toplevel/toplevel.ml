(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
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

(*
TODO
- syntax highlighting?
*)

open Lwt
open Compiler
module Html = Dom_html


module H = struct
  type 'a t = {
    data : 'a array;
    mutable start : int;
    mutable size : int;
  }

  let m x m =
    let r = x mod m in
    if r < 0 then r + m else r

  let make len (init : 'a) : 'a t = {
    data = Array.make len init;
    start = 0;
    size = 0;
  }

  let clear t = t.start <- 0; t.size <- 0

  let add x t =
    let len' = Array.length t.data in
    t.data.(m (t.start + t.size) len') <- x;
    if t.size < len'
    then t.size <- t.size + 1
    else t.start <- m (t.start + 1) len'
  let get t i =
    if i >= t.size || i < 0 then raise Not_found;
    t.data.(m (t.start + i) (Array.length t.data))

  let to_array t =
    if t.size = 0 then [||]
    else
      let arr = Array.make t.size t.data.(0) in
      for i = 0 to t.size - 1 do
        arr.(i) <- get t i;
      done;
      arr

  let size t = t.size


  let get_storage () =
    match Js.Optdef.to_option Html.window##localStorage with
        None -> raise Not_found
      | Some t -> t


  let load_or_create n : Js.js_string Js.t t =
    try
      let s = get_storage () in
      match Js.Opt.to_option (s##getItem(Js.string "history")) with
        | None -> raise Not_found
        | Some s ->
          let a = Json.unsafe_input s in
          if Array.length a.data = n
          then a
          else
            let a' = make n (Js.string "") in
            for i = 0 to a.size - 1 do
              add (get a i) a';
            done;
            a'
    with Not_found -> make n (Js.string "")

  let save t =
    try
      let s = get_storage () in
      let str = Json.output t in
      s##setItem(Js.string "history", str)
    with Not_found -> ()

end

module Top : sig
  val setup : (string -> unit) -> unit
  val exec : Format.formatter -> string -> unit
  val initialize : unit -> unit
end = struct
  let split_primitives p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len then []
      else if p.[cur] = '\000' then
        String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
      else
        split beg (cur + 1) in
    Array.of_list(split 0 0)

  class type global_data = object
    method compile : (string -> string) Js.writeonly_prop
  end

  external global_data : unit -> global_data Js.t = "caml_get_global_data"

  let g = global_data ()

  let setup output =
    Hashtbl.add Toploop.directive_table "enable" (Toploop.Directive_string Option.Optim.enable);
    Hashtbl.add Toploop.directive_table "disable" (Toploop.Directive_string Option.Optim.disable);
    Hashtbl.add Toploop.directive_table "debug_on" (Toploop.Directive_string Option.Debug.enable);
    Hashtbl.add Toploop.directive_table "debug_off" (Toploop.Directive_string Option.Debug.disable);
    Hashtbl.add Toploop.directive_table "debug_off" (Toploop.Directive_string Option.Debug.disable);
    Hashtbl.add Toploop.directive_table "tailcall" (Toploop.Directive_string (fun s ->
      let x = Option.Tailcall.of_string s in
      Option.Tailcall.set x));
    let initial_primitive_count =
      Array.length (split_primitives (Symtable.data_primitive_names ())) in

    let compile s =
      let prims =
        split_primitives (Symtable.data_primitive_names ()) in
      let unbound_primitive p =
        try ignore (Js.Unsafe.eval_string p); false with _ -> true in
      let stubs = ref [] in
      Array.iteri
        (fun i p ->
           if i >= initial_primitive_count && unbound_primitive p then
             stubs :=
               Format.sprintf
                 "function %s(){caml_failwith(\"%s not implemented\")}" p p
               :: !stubs)
        prims;
      let output_program = Driver.from_string prims s in
      let b = Buffer.create 100 in
      output_program (Pretty_print.to_buffer b);
      let res = Buffer.contents b in
      let res = String.concat "" !stubs ^ res in
      output res;
      res
    in
    g##compile <- compile (*XXX HACK!*)


  let refill_lexbuf s p ppf buffer len =
    if !p = String.length s
    then 0
    else
      let len',nl =
        try String.index_from s !p '\n' - !p + 1,false
        with _ -> String.length s - !p,true in
      let len'' = min len len' in
      String.blit s !p buffer 0 len'';
      Format.fprintf ppf "%s" (String.sub buffer 0 len'');
      if nl then Format.pp_print_newline ppf ();
      Format.pp_print_flush ppf ();
      p:=!p + len'';
      len''

  let exec' s =
    let lb = Lexing.from_string s in
    try
      List.iter
        (fun phr ->
           if not (Toploop.execute_phrase false Format.std_formatter phr) then raise Exit)
        (!Toploop.parse_use_file lb)
    with
    | Exit -> ()
    | x    -> Errors.report_error Format.err_formatter x

  let exec sharpf s =
    let lb = Lexing.from_function (refill_lexbuf s (ref 0) sharpf) in
    try
      while true do
        try
          let phr = !Toploop.parse_toplevel_phrase lb in
          ignore(Toploop.execute_phrase true Format.std_formatter phr)
        with
          End_of_file ->
          raise End_of_file
        | x ->
          Errors.report_error Format.err_formatter x
      done
    with End_of_file ->
      flush_all ()


  let initialize () =
    Toploop.initialize_toplevel_env ();
    Toploop.input_name := "//toplevel//";
    let header = "        Objective Caml version %s@.@." in
    exec' (Printf.sprintf "Format.printf \"%s\" Sys.ocaml_version;;" header);
    exec' ("#enable \"pretty\";;");
    exec' ("#enable \"shortvar\";;")
end

let trim s =
  let ws c = c = ' ' || c = '\t' || c = '\n' in
  let len = String.length s in
  let start = ref 0 in
  let stop = ref (len - 1) in
  while !start < len && (ws s.[!start])
  do incr start done;
  while !stop > !start && (ws s.[!stop])
  do decr stop done;
  String.sub s !start (!stop - !start + 1)

let by_id s =
  Js.Opt.get (Html.document##getElementById(Js.string s)) (fun () -> failwith (Printf.sprintf "cannot find dom id %S\n%!" s))

let by_id_coerce s f  =
  Js.Opt.get (Js.Opt.bind (Html.document##getElementById(Js.string s)) f)
    (fun () -> failwith (Printf.sprintf "cannot find dom id %S\n%!" s))

let do_by_id s f =
  Js.Opt.case (Html.document##getElementById(Js.string s))
    (fun () -> ())
    (fun d -> f d)

let examples =
  let r = Regexp.regexp "^\\(\\*+(.*)\\*+\\)$" in
  let l = ref [] and name = ref "" in
  let content = ref [] in
  try
    begin
      let ic = open_in "examples.ml" in
      try
        while true do
          let line = input_line ic in
          match Regexp.string_match r line 0 with
          | Some res ->
            if !l <> []
            then begin
              content:=(!name,List.rev !l)::!content;
              l:=[]
            end;
            (name := match Regexp.matched_group res 1 with Some s -> s | None -> assert false)
          | None -> l:= line::!l
        done;
        assert false
      with End_of_file ->
        if !l <> []
        then begin
          content:=(!name,List.rev !l)::!content;
          l:=[]
        end;
        List.rev_map (fun (name,content) -> name,trim (String.concat "\n" content)) !content
    end
  with  _ -> []

let indent_caml s =
  let output = {
    IndentPrinter.debug = false;
    config = IndentConfig.default;
    in_lines = (fun _ -> true);
    indent_empty = true;
    adaptive = true;
    kind = IndentPrinter.Print (fun s acc -> acc ^ s)
  }
  in
  let stream = Nstream.of_string s in
  IndentPrinter.proceed output stream IndentBlock.empty ""


let indent_textarea textbox =
  let rec loop s acc (i,pos') =
    try
      let pos = String.index_from s pos' '\n' in
      loop s ((i,(pos',pos))::acc) (succ i,succ pos)
    with _ -> List.rev ((i,(pos',String.length s)) :: acc) in
  let rec find (l : (int * (int * int)) list ) c =
    match l with
    | [] -> assert false
    | (i,(lo,up))::_ when up >= c -> c,i,lo,up
    | (_,(lo,up))::rem -> find rem c in
  let v = textbox##value in
  let pos =
    let c1 = (Obj.magic textbox)##selectionStart
    and c2 = (Obj.magic textbox)##selectionEnd in
    if Js.Opt.test (Obj.magic c1) && Js.Opt.test (Obj.magic c2)
    then begin
      let l = loop (Js.to_string v) [] (0,0) in
      Some (find l c1,find l c2)
    end
    else None in
  let v = indent_caml (Js.to_string v) in
  textbox##value<-Js.string v;
  begin match pos with
    | Some ((c1,line1,lo1,up1),(c2,line2,lo2,up2)) ->
      let l = loop v [] (0,0) in
      let (lo1'',up1'') = List.assoc line1 l in
      let (lo2'',up2'') = List.assoc line2 l in
      let n1 = max (c1 + up1'' - up1) lo1'' in
      let n2 = max (c2 + up2'' - up2) lo2'' in
      (Obj.magic textbox)##setSelectionRange(n1,n2);
      textbox##focus();
      ()
    | None -> () end

let run _ =
  let container = by_id "toplevel-container" in
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Html.CoerceTo.textarea in
  let example_container = by_id "toplevel-examples" in

  let sharp_chan = open_out "sharp" in
  let sharp_ppf = Format.formatter_of_out_channel sharp_chan in
  let resize () =
    Lwt.pause () >>= fun () ->
    textbox##style##height <- Js.string "auto";
    textbox##style##height <- Js.string (Printf.sprintf "%dpx" (max 18 textbox##scrollHeight));
    container##scrollTop <- container##scrollHeight;
    Lwt.return () in


  let hist = H.load_or_create 100 in
  let hist_idx = ref (H.size hist) in
  let cur = ref (Js.string "") in
  let execute () =
    let content' = Js.to_string textbox##value in
    let content = trim content' in
    let len = String.length content in
    let content =
      if content = ""
      || (len > 2
          && content.[len - 1] = ';'
          && content.[len - 2] = ';')
      then content'
      else content' ^ ";;" in
    H.add (Js.string content') hist;
    H.save hist;
    cur:=Js.string "";
    hist_idx:=H.size hist;
    Top.exec sharp_ppf content;
    textbox##value <- Js.string "";
    resize () >>= fun () ->
    container##scrollTop <- container##scrollHeight;
    textbox##focus();
    Lwt.return_unit in

  List.iter (fun (name,content) ->
      let a = Html.createA Html.document in
      a##className <- Js.string "list-group-item";
      a##innerHTML <- Js.string name;
      a##onclick <- Html.handler (fun _ ->
          textbox##value <- Js.string content;
          Lwt.async(fun () ->
              resize () >>= fun () ->
              container##scrollTop <- container##scrollHeight;
              textbox##focus();
              Lwt.return_unit);
          Js._true
        );
      Dom.appendChild example_container a
    ) examples;

  begin (* setup handlers *)
    container##onclick <- Html.handler (fun _ -> textbox##focus(); Js._false);
    do_by_id "btn-execute"
      (fun e -> e##onclick <- Html.handler (fun _ -> Lwt.async execute; Js._false));
    do_by_id "btn-clear"
      (fun e -> e##onclick <- Html.handler (fun _ -> output##innerHTML <- Js.string ""; Js._false));
    do_by_id "btn-reset"
      (fun e -> e##onclick <- Html.handler (fun _ -> output##innerHTML <- Js.string "";
                                             Top.initialize (); Js._false));

    textbox##onkeydown <- Html.handler (fun e ->
        match e##keyCode with
        | 13 when not (Js.to_bool e##ctrlKey  ||
                       Js.to_bool e##shiftKey ||
                       Js.to_bool e##altKey   ||
                       Js.to_bool e##metaKey) ->
          Lwt.async execute;
          Js._false
        | 13 ->
          Lwt.async resize;Js._true
        | 09 ->
          indent_textarea textbox;
          Js._false
        | 38 (* when not (Js.to_bool e##ctrlKey) *) ->
          (try
             if !hist_idx = H.size hist
             then cur := textbox##value;
             let idx = !hist_idx - 1 in
             let s=H.get hist idx in
             hist_idx:=idx;
             textbox##value <- s;
             Js._false
          with _ -> Js._false)
        | 40 (* when not (Js.to_bool e##ctrlKey) *) ->
          (try
             let idx = !hist_idx + 1 in
             if idx = H.size hist
             then begin
               incr hist_idx;
               textbox##value <- !cur;
             end else begin
               let s=H.get hist idx in
               hist_idx:=idx;
               textbox##value <- s
             end;
             Js._false
           with _ -> Js._false)
        | 76 when (Js.to_bool e##ctrlKey  ||
                   Js.to_bool e##shiftKey ||
                   Js.to_bool e##altKey   ||
                   Js.to_bool e##metaKey) ->
          output##innerHTML <- Js.string "";
          Js._true
        | 75 when (Js.to_bool e##ctrlKey  ||
                   Js.to_bool e##shiftKey ||
                   Js.to_bool e##altKey   ||
                   Js.to_bool e##metaKey) ->
          Top.initialize ();
          Js._false

        | _ -> Js._true
      );
    textbox##onkeyup <- Html.handler (fun e ->
        Lwt.async resize;Js._true);
    textbox##onchange <- Html.handler (fun _ -> Lwt.async resize; Js._true)
  end;

  let append_string cl s =
    let span = Html.createSpan Html.document in
    span##className <- Js.string cl;
    Dom.appendChild span (Html.document##createTextNode(Js.string s ));
    Dom.appendChild output span in
  Sys_js.set_channel_flusher sharp_chan (append_string "sharp");
  Sys_js.set_channel_flusher stdout (append_string "stdout");
  Sys_js.set_channel_flusher stderr (append_string "stderr");

  Lwt.async (fun () ->
      resize () >>= fun () ->
      textbox##focus ();
      Lwt.return_unit
    );
  let ph = by_id "last-js" in

  Top.setup (fun s -> ph##innerHTML <- Js.string s);
  Top.initialize ()

let _ = Html.window##onload <- Html.handler (fun _ -> run (); Js._false)
