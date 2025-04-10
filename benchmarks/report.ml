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

open StdLabels
open Common

let reference = ref None

let maximum = ref (-1.)

let minimum = ref 0.

let omitted = ref []

let appended = ref []

let errors = ref false

let svg = ref false

let svgfontsize = ref 7

let svgwidth = ref 500

let svgheight = ref 150

let edgecaption = ref false

let ylabel = ref ""

let rec merge f l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> []
  | (n1, v1) :: r1, (n2, v2) :: r2 ->
      if n1 < n2
      then merge f r1 l2
      else if n1 > n2
      then merge f l1 r2
      else (n1, f v1 v2) :: merge f r1 r2

let merge_blank = List.map ~f:(fun (n2, v2) -> n2, (0.0, 0.0) :: v2)

let read_column ~measure path spec refe :
    (Measure.t option list * (string * (float * float) list) list) option =
  let l =
    List.map
      (Spec.find_names ~root:path (Spec.no_ext spec))
      ~f:(fun nm ->
        let l = read_measures path spec nm in
        let a = Array.of_list l in
        let m, i = mean_with_confidence a in
        nm, [ m, i ])
  in
  if refe then reference := Some l;
  Some ([ Some measure ], l)

let read_blank_column () = None

let rec list_create n a = if n = 0 then [] else a :: list_create (n - 1) a

let merge_columns l old_table =
  let rec aux = function
    | [] | [ None ] -> [], []
    | [ Some c ] -> c
    | Some (h, c) :: r ->
        let h', t = aux r in
        h @ h', merge (fun v1 v2 -> v1 @ v2) c t
    | None :: r ->
        let h', t = aux r in
        (*VVV utile ? *)
        None :: h', merge_blank t
  in
  let rec remove_head_blank = function
    | None :: l ->
        let n, ll = remove_head_blank l in
        n + 1, ll
    | l -> 0, l
  in
  let add_blanks n (h, t) =
    if n = 0
    then h, t
    else
      let zeros = list_create n (0.0, 0.0) in
      (*VVV utile ? *)
      let nodisplays = list_create n None in
      h @ nodisplays, List.map t ~f:(fun (a, l) -> a, l @ zeros)
  in
  (* if there was an old table, we keep only the lines corresponding
     to entries in that table *)
  let l =
    match l, old_table with
    | [], _ -> []
    | _, None -> l
    | Some (h, c) :: ll, Some o -> Some (h, merge (fun v1 _v2 -> v1) c o) :: ll
    | None :: ll, Some o ->
        Some ([ None ], List.map o ~f:(fun (nm, _) -> nm, [ 0.0, 0.0 ])) :: ll
  in
  let nb_blanks, l = remove_head_blank (List.rev l) in
  let l = List.rev l in
  add_blanks nb_blanks (aux l)

let normalize (h, t) =
  match !reference with
  | None -> h, t
  | Some rr ->
      ( h
      , List.map t ~f:(fun (nm, l) ->
            let r, _ = List.hd (List.assoc nm rr) in
            if r <> r
            then (
              Format.eprintf "No reference available for '%s'@." nm;
              exit 1);
            nm, List.map l ~f:(fun (v, i) -> v /. r, i /. r)) )

let stats (h, t) =
  for i = 0 to List.length h - 1 do
    match List.nth h i with
    | Some Measure.{ name = nm; _ } ->
        let l = List.map t ~f:(fun (_, l) -> fst (List.nth l i)) in
        let a = Array.of_list l in
        Array.sort a ~cmp:compare;
        let p = List.fold_right l ~f:(fun x p -> x *. p) ~init:1. in
        Format.eprintf
          "%s:@.  %f %f@."
          nm
          (p ** (1. /. float (List.length l)))
          a.(Array.length a / 2)
    | None -> ()
  done

let escape_name_for_gnuplot s =
  let b = Buffer.create (String.length s) in
  String.iter s ~f:(function
    | '_' -> Buffer.add_string b {|\\\_|}
    | c -> Buffer.add_char b c);
  Buffer.contents b

let text_output _no_header (h, t) =
  Format.printf "-";
  List.iter h ~f:(fun v ->
      let nm =
        match v with
        | Some Measure.{ name; _ } -> name
        | None -> ""
      in
      Format.printf " - \"%s\"" nm);
  Format.printf "@.";
  List.iter t ~f:(fun (nm, l) ->
      Format.printf "%s" nm;
      List.iter l ~f:(fun (m, i) -> Format.printf " %f %f" m i);
      Format.printf "@.")

let gnuplot_output ch no_header (h, t) =
  let n = List.length (snd (List.hd t)) in
  if not no_header
  then (
    if !svg
    then
      Printf.fprintf
        ch
        "set terminal svg size %d %d font 'Arial,%d'\n"
        !svgwidth
        !svgheight
        !svgfontsize;
    if !edgecaption
    then Printf.fprintf ch "set key tmargin horizontal Left left reverse\n";
    Printf.fprintf
      ch
      "set multiplot\n\
       set style data histograms\n\
       set style fill solid 1 border rgb 'black'\n\
       set style histogram errorbars gap 1%s\n\
       set xtics border in scale 0,0 nomirror rotate by -30  offset character 0, 0, 0\n"
      (if !errors then " lw 1" else "");
    if !ylabel <> "" then Printf.fprintf ch "set ylabel \"%s\"\n" !ylabel;
    if !maximum > 0.
    then Printf.fprintf ch "set yrange [%f:%f]\n" !minimum !maximum
    else Printf.fprintf ch "set yrange [0:]\n");
  (* labels *)
  for i = 0 to n - 1 do
    let nn = ref 0. in
    List.iter t ~f:(fun (_nm, l) ->
        let v, _ii = List.nth l i in
        if !maximum > 0. && v > !maximum
        then
          Printf.fprintf
            ch
            "set label font \",5\" \"%.2f\" at %f,%f center\n"
            v
            (!nn +. (float i /. float n) -. 0.5) (* why? *)
            ((!maximum *. 1.04) +. 0.01);
        nn := !nn +. 1.)
  done;
  Printf.fprintf ch "plot";
  for i = 0 to n - 1 do
    match List.nth h i with
    | Some Measure.{ color; _ } -> (
        if i > 0
        then Printf.fprintf ch ", \"-\" using 2:3 title columnhead lw 0"
        else Printf.fprintf ch " \"-\" using 2:3:xtic(1) title columnhead lw 0";
        match color with
        | Some c -> Printf.fprintf ch " lc rgb '%s'" c
        | None -> ())
    | None ->
        if i > 0
        then Printf.fprintf ch ", \"-\" using 2:3 title columnhead lw 0"
        else Printf.fprintf ch " \"-\" using 2:3:xtic(1) title columnhead lw 0"
    (* notitle does not work ... I don't know why ... *)
  done;
  Printf.fprintf ch "\n";
  for i = 0 to n - 1 do
    let nm =
      match List.nth h i with
      | Some Measure.{ name; _ } -> name
      | None -> ""
    in
    Printf.fprintf ch "- \"%s\"\n" (escape_name_for_gnuplot nm);
    List.iter t ~f:(fun (nm, l) ->
        let v, ii = List.nth l i in
        Printf.fprintf
          ch
          "\"%s\" %f %f\n"
          (escape_name_for_gnuplot nm)
          v
          (if ii <> ii then 0. else ii));
    Printf.fprintf ch "e\n"
  done

let filter (h, t) =
  let l1 =
    List.filter t ~f:(fun (nm, _) ->
        not (List.mem nm ~set:!appended || List.mem nm ~set:!omitted))
  in
  let app =
    List.fold_left !appended ~init:[] ~f:(fun beg nm ->
        try (nm, List.assoc nm t) :: beg with Not_found -> beg)
  in
  h, l1 @ app

let output_table =
  let old_table = ref None in
  fun (l : (Measure.t option list * _) option list) f ->
    let t = merge_columns l !old_table in
    old_table := Some (snd t);
    let t = filter t in
    let t = normalize t in
    stats t;
    f t

let rec transpose (l : (string * 'a list) list) : (string * 'a) list list =
  match l with
  | (_, _ :: _) :: _ ->
      List.map ~f:(fun (label, data) -> label, List.hd data) l
      :: transpose (List.map ~f:(fun (label, data) -> label, List.tl data) l)
  | (_, []) :: _ | [] -> []

let geometric_mean l =
  exp
    (List.fold_left ~f:( +. ) ~init:0. (List.map l ~f:(fun (_, (v, _)) -> log v))
    /. float (List.length l))

let current_bench_output
    (ch : out_channel)
    (_no_header : bool)
    ((header : Measure.t option list), (t : (string * (float * float) list) list)) =
  let suite_name = !ylabel in
  let measure_descs =
    (* Filter out blank columns *)
    List.filter_map header ~f:Fun.id
  in
  let measures = transpose t in
  assert (List.length measures = List.length measure_descs);
  let summary =
    let metrics =
      List.map2 measure_descs measures ~f:(fun desc measures ->
          let description =
            Option.value desc.Measure.description ~default:desc.Measure.name
          in
          let mean = geometric_mean measures in
          `Assoc
            [ "name", `String description
            ; "value", `Float mean
            ; "units", `String desc.Measure.units
            ])
    in
    `Assoc
      [ "name", `String "Microbenchmarks - summary (geometric means)"
      ; "metrics", `List metrics
      ]
  in
  let results =
    List.map2 measure_descs measures ~f:(fun desc measures ->
        let description =
          Option.value desc.Measure.description ~default:desc.Measure.name
        in
        let metrics : Yojson.Basic.t list =
          List.map measures ~f:(fun (test_name, (m, _confidence_itvl)) ->
              `Assoc
                [ "name", `String test_name
                ; "value", `Float m
                ; "units", `String desc.Measure.units
                ])
        in
        `Assoc
          [ "name", `String (String.concat ~sep:" - " [ "Microbenchmarks"; description ])
          ; "metrics", `List metrics
          ])
  in
  let json =
    `Assoc [ "name", `String suite_name; "results", `List (summary :: results) ]
  in
  Yojson.Basic.to_channel ch json;
  output_char ch '\n'

let output ~format conf =
  let output_function, close =
    match format with
    | `Text -> text_output, fun () -> ()
    | `Gnuplot_script -> gnuplot_output stdout, fun () -> ()
    | `Gnuplot ->
        let ch = Unix.open_process_out "gnuplot -persist" in
        gnuplot_output ch, fun () -> close_out ch
    | `Current_bench -> current_bench_output stdout, fun () -> ()
  in
  let no_header = ref false in
  List.iter conf ~f:(fun conf ->
      output_table
        (List.map conf ~f:(function
          | None -> read_blank_column ()
          | Some (dir1, dir2, measure, refe) ->
              read_column ~measure dir1 (Spec.create dir2 "") refe))
        (output_function !no_header);
      no_header := true);
  close ()

let _ =
  let conf = ref "report.config" in
  let format : [ `Gnuplot | `Gnuplot_script | `Text | `Current_bench ] ref =
    ref `Gnuplot
  in
  let script = ref false in
  let nreference = ref None in
  let options =
    [ ( "-ref"
      , Arg.Int (fun i -> nreference := Some i)
      , "<col> use column <col> as the baseline. Overrides histogramref." )
    ; "-max", Arg.Set_float maximum, "<m> truncate graph at level <max>"
    ; "-min", Arg.Set_float minimum, "<m> truncate graph below level <min>"
    ; ( "-format"
      , Arg.Symbol
          ( [ "gnuplot"; "table"; "current-bench" ]
          , function
            | "gnuplot" -> format := `Gnuplot
            | "table" -> format := `Text
            | "current-bench" -> format := `Current_bench
            | _ -> assert false )
      , " output format: a Gnuplot graph, a text table, or a JSON object for use by \
         current-bench (default gnuplot)" )
    ; ( "-omit"
      , Arg.String (fun s -> omitted := split_on_char s ~sep:',' @ !omitted)
      , " omit the given benchmark" )
    ; ( "-append"
      , Arg.String (fun s -> appended := split_on_char s ~sep:',' @ !appended)
      , " append the given benchmark at the end" )
    ; "-errors", Arg.Set errors, " display error bars"
    ; "-config", Arg.Set_string conf, "<file> use <file> as a config file"
    ; "-script", Arg.Set script, " output gnuplot script"
    ; ( "-svg"
      , Arg.Tuple
          [ Arg.Set svg
          ; Arg.Set_int svgfontsize
          ; Arg.Set_int svgwidth
          ; Arg.Set_int svgheight
          ]
      , "<fontsize> <width> <height> svg output" )
    ; "-edgecaption", Arg.Set edgecaption, " display caption outside the diagram"
    ; "-ylabel", Arg.Set_string ylabel, " Y axis label"
    ]
  in
  Arg.parse
    (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  let format =
    match !format, !script with
    | `Gnuplot, true -> `Gnuplot_script
    | `Gnuplot, false -> `Gnuplot
    | _, true ->
        Printf.eprintf "-script only work with gnuplot format";
        exit 2
    | ((`Gnuplot_script | `Text | `Current_bench) as x), false -> x
  in
  let conf = read_report_config ?column_ref:!nreference !conf in
  output ~format conf

(*
http://hacks.mozilla.org/2009/07/tracemonkey-overview/
*)
