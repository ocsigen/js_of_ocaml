#! /usr/bin/ocaml unix.cma
(* Js_of_ocaml benchmarks
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#use "lib/common.ml"

(****)

let reference = ref None
let nreference = ref (-1)
let maximum = ref (-1.)
let gnuplot = ref true
let table = ref false
let omitted = ref []
let appended = ref []
let errors = ref false
let script = ref false
let conf = ref "report.config"
let svg = ref false
let svgfontsize = ref 7
let svgwidth = ref 500
let svgheight = ref 150
let edgecaption = ref false
let ylabel = ref ""

(****)

let str_split s c =
  let i = ref (String.length s) in
  let r = ref [] in
  begin try
    while true do
      let j = String.rindex_from s (!i - 1) c in
      r := String.sub s (j + 1) (!i - j - 1) :: !r;
      i := j
    done
  with Not_found -> () end;
  String.sub s 0 !i :: !r

(****)

let rec merge f l1 l2 =
  match l1, l2 with
    [], _ | _, [] ->
      []
  | (n1, v1) :: r1, (n2, v2) :: r2 ->
      if n1 < n2 then
        merge f r1 l2
      else if n1 > n2 then
        merge f l1 r2
      else
        (n1, f v1 v2) :: merge f r1 r2

let merge_blank l2 =
  List.map (fun (n2, v2) -> (n2, (0.0, 0.0) :: v2)) l2

let read_column ?title ?color meas spec refe =
  let l =
    List.map
      (fun nm ->
        let l = read_measures meas spec nm in
        let a = Array.of_list l in
        let (m, i) = mean_with_confidence a in
        (nm, [(m, i)]))
      (benchs meas (no_ext spec))
  in
  let nm =
    match title with
      | Some nm -> nm
      | None    -> dir meas (no_ext spec)
  in
  if refe then reference := Some l;
  Some ([Some (nm, color)], l)

let read_blank_column () = None

let rec list_create n a =
  if n = 0
  then []
  else a::list_create (n-1) a

let merge_columns l old_table =
  let rec aux = function
    | [] | [None] -> ([], [])
    | [Some c] -> c
    | Some (h, c) :: r ->
      let (h', t) = aux r in
      (h @ h', merge (fun v1 v2 -> v1 @ v2) c t)
    | None :: r ->
      let (h', t) = aux r in
(*VVV utile ? *)
      (None::h', merge_blank t)
  in
  let rec remove_head_blank = function
    | None :: l -> let (n, ll) = remove_head_blank l in (n+1, ll)
    | l -> (0, l)
  in
  let rec add_blanks n (h, t) =
    if n = 0
    then (h, t)
    else
      let zeros = list_create n (0.0, 0.0) in

(*VVV utile ? *)
      let nodisplays = list_create n None in

      (h @ nodisplays , List.map (fun (a, l) -> (a, l @ zeros)) t)
  in
  (* if there was an old table, we keep only the lines corresponding
     to entries in that table *)
  let l = match l, old_table with
    | [], _ -> []
    | _, None -> l
    | (Some (h, c))::ll, Some o ->
      (Some (h, (merge (fun v1 v2 -> v1) c o)))::ll
    | None::ll, Some o -> 
      (Some ([None], (List.map (fun (nm, _) -> (nm, [0.0, 0.0])) o)))::ll
  in
  let (nb_blanks, l) = remove_head_blank (List.rev l) in
  let l = List.rev l in
  add_blanks nb_blanks (aux l)

let normalize (h, t) =
  match !reference with
    | None -> (h, t)
    | Some rr ->
      (h,
       List.map
         (fun (nm, l) ->
           let (r, _) = List.hd (List.assoc nm rr) in
           if r <> r then begin
             Format.eprintf "No reference available for '%s'@." nm;
             exit 1
           end;
           (nm, List.map (fun (v, i) -> (v /. r, i /. r)) l))
         t)

let stats (h, t) =
  for i = 0 to List.length h - 1 do
    match List.nth h i with
      | Some (nm, _) ->
        let l = List.map (fun (_, l) -> fst (List.nth l i)) t in
        let a = Array.of_list l in
        Array.sort compare a;
        let p = List.fold_right (fun x p -> x *. p) l 1. in
        Format.eprintf "%s:@.  %f %f@." nm
          (p ** (1. /. float (List.length l)))
          a.(Array.length a / 2)
      | None -> ()
  done

let text_output _no_header (h, t) =
  Format.printf "-";
  List.iter (fun v ->
    let nm = match v with
      | Some (nm, _) -> nm
      | None -> ""
    in
    Format.printf " - \"%s\"" nm) h;
  Format.printf "@.";
  List.iter
    (fun (nm, l) ->
       Format.printf "%s" nm;
       List.iter (fun (m, i) -> Format.printf " %f %f" m i) l;
       Format.printf "@.")
    t

let gnuplot_output ch no_header (h, t) =
  let n = List.length (snd (List.hd t)) in
  if not no_header
  then begin
    if !svg
    then Printf.fprintf ch "set terminal svg fsize %d size %d %d\n" !svgfontsize !svgwidth !svgheight;
    if !edgecaption
    then Printf.fprintf ch "set key tmargin horizontal Left left reverse\n";
    Printf.fprintf ch "\
      set multiplot\n\
      set style data histograms\n\
      set style fill solid 1 border rgb 'black'\n\
      set style histogram errorbars gap 1%s\n\
      set xtics border in scale 0,0 nomirror rotate by -30  \
                offset character 0, 0, 0\n"
      (if !errors then " lw 1" else "");
    if !ylabel <> "" then Printf.fprintf ch "set ylabel \"%s\"\n" !ylabel;
    if !maximum > 0. then
      Printf.fprintf ch "set yrange [0:%f]\n" !maximum
    else
      Printf.fprintf ch "set yrange [0:]\n";
  end;

  (* labels *)
  for i = 0 to n - 1 do
    let nn = ref 0. in
    List.iter
      (fun (nm, l) ->
         let (v, ii) = List.nth l i in
         if !maximum > 0. && v > !maximum
         then Printf.fprintf ch "set label font \",5\" \"%.2f\" at %f,%f center\n"
           v (!nn +. float i /. float n -. 0.5 (* why? *))
           (!maximum *. 1.04 +. 0.1);
         nn := !nn +. 1.)
      t;
  done;

  Printf.fprintf ch "plot";
  for i = 0 to n - 1 do
    match List.nth h i with
      | Some (_, col) ->
        if i > 0
        then Printf.fprintf ch ", \"-\" using 2:3 title columnhead lw 0"
        else Printf.fprintf ch " \"-\" using 2:3:xtic(1) title columnhead lw 0";
        (match col with
          | Some c -> Printf.fprintf ch " lc rgb '%s'" c
          | None   -> ());
      | None ->
        if i > 0
        then Printf.fprintf ch ", \"-\" using 2:3 title columnhead lw 0"
        else Printf.fprintf ch " \"-\" using 2:3:xtic(1) title columnhead lw 0";
(* notitle does not work ... I don't know why ... *)
  done;
  Printf.fprintf ch "\n";
  for i = 0 to n - 1 do
    let nm = match List.nth h i with
      | Some (nm, _) -> nm
      | None -> ""
    in
    Printf.fprintf ch "- - \"%s\"\n" nm;
    List.iter
      (fun (nm, l) ->
         let (v, ii) = List.nth l i in
         Printf.fprintf ch "\"%s\" %f %f\n" nm v (if ii <> ii then 0. else ii))
      t;
    Printf.fprintf ch "e\n"
  done

let filter (h, t) =
  let l1 = List.filter
    (fun (nm, _) -> 
      not ((List.mem nm !appended) || (List.mem nm !omitted)))
    t
  in
  let app =
    List.fold_left 
      (fun beg nm -> try (nm, List.assoc nm t)::beg with Not_found -> beg)
      [] !appended
  in 
  (h, l1 @ app)

let output_table =
  let old_table = ref None in
  fun r (l:  ((string * 'a option) option list * _) option list) f ->
    let t = merge_columns l !old_table in
    old_table := Some (snd t);
    let t = filter t in
    let t = normalize t in
    stats t;
    f t

let output_tables r conf =
  let output_function, close =
    if !table
    then text_output, fun () -> ()
    else if !script
    then gnuplot_output stdout, fun () -> ()
    else begin
      let ch = Unix.open_process_out "gnuplot -persist" in
      (gnuplot_output ch,
       fun () -> close_out ch)
    end
  in
  let no_header = ref false in
  List.iter
    (fun conf ->
      output_table r
        (List.map
           (function
             | None -> read_blank_column ()
             | Some (dir1, dir2, color, title, refe) -> 
               read_column ~title ~color dir1 (dir2, "") refe)
           conf)
        (output_function !no_header);
      no_header := true;
    )
    conf;
  close ()


(*
let f _ =
  let c1 = read_column (times ^ "/v8") js_of_ocaml in
  let c2 = read_column (times ^ "/v8") ocamljs in
  output_table 1 [c1; c2]
*)

(*
let f _ =
  let c1 = read_column times opt in
  let c2 = read_column times byte in
  let c3 = read_column (times ^ "/v8") js_of_ocaml in
  output_table 1 [c3; c2; c1]
*)

(*
let f _ =
  let c1 = read_column (times ^ "/v8") js_of_ocaml in
  let c2 = read_column (times ^ "/v8") js_of_ocaml_unsafe in
  output_table 1 [c1; c2]

let f _ =
  let o = read_column ~title:"ocamlopt" ~color:"#729fcf" times opt in
  let b = read_column ~title:"ocamlc" ~color:"#204a87" times byte in
  let c0 = read_column ~title:"old V8 (august?)" ~color:"#fbaf4f" (times ^ "/oldv8") js_of_ocaml in
  let c1 = read_column ~title:"V8" ~color:"#d98e2d" (times ^ "/v8") js_of_ocaml in
  let c2 = read_column ~title:"Nitro" ~color:"#a75f0c" (times ^ "/nitro") js_of_ocaml in
  let c3 = read_column ~title:"TraceMonkey" ~color:"#a40000" (times ^ "/tm") js_of_ocaml in
  output_table 2 [o; b; c0; c1; c2; c3]
*)

(*

let f _ =
  let o = read_column ~title:"ocamlopt" ~color:"#729fcf" times opt in
  let b = read_column ~title:"ocamlc" ~color:"#326bbe" times byte in
  let c1 = read_column ~title:"V8" ~color:"#d98e2d" (times ^ "/v8") js_of_ocaml in
  let c2 = read_column ~title:"Nitro" ~color:"#a75f0c" (times ^ "/nitro") js_of_ocaml in
  output_table 2 [o; b; c1; c2]
*)

(*
let f _ =
  let engine = "v8" in
  let c1 = read_column (times ^ "/" ^ engine) js in
  let c2 = read_column (times ^ "/" ^ engine) js_of_ocaml in
  output_table 2 [c1; c2]
*)

(*
let f _ =
  let c1 = read_column sizes ml in
  let c2 = read_column sizes byte in
  let c3 = read_column sizes (sub_spec js_of_ocaml "full") in
  let c4 = read_column sizes (sub_spec js_of_ocaml "generated") in
  let c5 = read_column sizes ocamljs in
  output_table 3 [c1; c2; c3; c4; c5]
*)

(*
let f _ =
  let c1 = read_column sizes (sub_spec js_of_ocaml "generated") in
  let c2 = read_column sizes js_of_ocaml_compact in
  let c3 = read_column sizes js_of_ocaml_inline in
  let c4 = read_column sizes js_of_ocaml_deadcode in
  output_table 1 [c1; c2; c3; c4]
*)

(*
let f _ =
  let c2 = read_column ~title:"bytecode" ~color:"#326bbe" sizes byte in
  let c3 = read_column ~title:"Javascript" ~color:"#a75f0c" sizes (sub_spec js_of_ocaml "full") in
  output_table 1 [c2; c3]
*)

(****)

let read_config () =
  let f = !conf in
  if not (Sys.file_exists f) then begin
    Format.eprintf "Configuration file '%s' not found!@." f;
    exit 1
  end;
  let fullinfo = ref [] in
  let info = ref [] in
  let i = ref 0 in
  let reference = ref false in
  let ch = open_in f in
  let split_at_space l =
    try
      let i = String.index l ' ' in
      (String.sub l 0 i, String.sub l (i + 1) (String.length l - i - 1))
    with Not_found -> (l, "")
  in
  let get_info dir0 rem refe =
    let (dir1, rem) = split_at_space rem in
    let (dir2, rem) = split_at_space rem in
    let (color, title) = split_at_space rem in
    let dir1 = if dir1 = "\"\"" then dir0 else dir0^"/"^dir1 in
    info := Some (dir1, dir2, color, title, refe) :: !info
  in
  begin try
    while true do
      let l = input_line ch in
      if String.length l = 0
      then
        (if !info <> []
         then (fullinfo := (List.rev !info)::!fullinfo ; info := []; i:=0))
      else
        if l.[0] <> '#'
        then begin
          incr i;
          reference := !nreference = !i;
          let (kind, rem) = split_at_space l in
          let (kind2, rem) = split_at_space rem in
          (match kind with
            | "histogram" -> ()
            | "histogramref" ->
              if !nreference = -1 then reference := true
            | _ ->
              Format.eprintf "Unknown config options '%s'@." kind;
              exit 1);
          (match kind2 with
            | "blank" -> info := None :: !info
            | "times" -> get_info times rem !reference
            | "compiletimes" -> get_info compiletimes rem !reference
            | "sizes" -> get_info sizes rem !reference
            | _ ->
              Format.eprintf "Unknown config options '%s'@." kind2;
              exit 1);
        end
    done
  with End_of_file -> () end;
  close_in ch;
  if !info <> [] then fullinfo := (List.rev !info)::!fullinfo;
  (!reference, List.rev !fullinfo)

let _ =
  let options =
    [("-ref", Arg.Set_int nreference, "<col> use column <col> as the baseline");
     ("-max", Arg.Set_float maximum, "<m> truncate graph at level <max>");
     ("-table", Arg.Set table, " output a text table");
     ("-omit", Arg.String (fun s -> omitted := str_split s ',' @ !omitted),
      " omit the given benchmark");
     ("-append", Arg.String (fun s -> appended := str_split s ',' @ !appended),
      " append the given benchmark at the end");
     ("-errors", Arg.Set errors, " display error bars");
     ("-config", Arg.Set_string conf, "<file> use <file> as a config file");
     ("-script", Arg.Set script, " output gnuplot script");
     ("-svg", Arg.Tuple [Arg.Set svg;
                         Arg.Set_int svgfontsize;
                         Arg.Set_int svgwidth;
                         Arg.Set_int svgheight],
      "<fontsize> <width> <height> svg output");
     ("-edgecaption", Arg.Set edgecaption, " display caption outside the diagram");
     ("-ylabel", Arg.Set_string ylabel, " Y axis label");
    ]
  in
  Arg.parse (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));

  let r, conf = read_config () in

  output_tables r conf

(* f () *)

(*
http://hacks.mozilla.org/2009/07/tracemonkey-overview/
http://weblogs.mozillazine.org/bz/archives/020732.html
*)

(*
./report.ml -max 4 -omit hamming
*)
