#! /usr/bin/ocaml unix.cma

#use "lib/common.ml"

(****)

let reference = ref (-1)
let maximum = ref (-1.)
let gnuplot = ref true
let table = ref false

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

let read_column meas spec =
  let l =
  List.map
    (fun nm ->
       let l = read_measures meas spec nm in
       let a = Array.of_list l in
       let (m, i) = mean_with_confidence a in
       (nm, [(m, i)]))
    (benchs meas (no_ext spec))
  in
  ([dir meas (no_ext spec)], l)

let rec merge_columns l =
  match l with
    [] ->
      ([], [])
  | [c] ->
      c
  | (h, c) :: r ->
      let (h', t) = merge_columns r in
      (h @ h', merge (fun v1 v2 -> v1 @ v2) c t)

let normalize n (h, t) =
  (h,
   List.map
     (fun (nm, l) ->
        let (r, _) = List.nth l n in
        if r <> r then begin
          Format.eprintf "No reference available for '%s'@." nm;
          exit 1
        end;
        (nm, List.map (fun (v, i) -> (v /. r, i /. r)) l))
     t)

let stats (h, t) =
  for i = 0 to List.length h - 1 do
    let nm = List.nth h i in
    let l = List.map (fun (_, l) -> fst (List.nth l i)) t in
    let a = Array.of_list l in
    Array.sort compare a;
    let p = List.fold_right (fun x p -> x *. p) l 1. in
    Format.eprintf "%s:@.  %f %f@." nm
      (p ** (1. /. float (List.length l)))
      a.(Array.length a / 2)
  done

let text_output (h, t) =
  Format.printf "-";
  List.iter (fun s -> Format.printf " - \"%s\"" s) h;
  Format.printf "@.";
  List.iter
    (fun (nm, l) ->
       Format.printf "%s" nm;
       List.iter (fun (m, i) -> Format.printf " %f %f" m i) l;
       Format.printf "@.")
    t

let gnuplot_output ch (h, t) =
  let n = List.length (snd (List.hd t)) in
  Printf.fprintf ch "\
    set style data histograms\n\
    set style fill solid 1.00 border -1\n\
    set style histogram errorbars gap 1 lw 1\n\
    set xtics border in scale 0,0 nomirror rotate by -30  \
              offset character 0, 0, 0\n";
 if !maximum > 0. then
   Printf.fprintf ch "set yrange [0:%f]\n" !maximum
 else
   Printf.fprintf ch "set yrange [0:]\n";
 Printf.fprintf ch "plot";
  for i = 0 to n - 1 do
    if i > 0 then Printf.fprintf ch ", \"-\" using 2:3 title columnhead"
    else Printf.fprintf ch " \"-\" using 2:3:xtic(1) title columnhead"
  done;
  Printf.fprintf ch "\n";
  for i = 0 to n - 1 do
    Printf.fprintf ch "- - \"%s\"\n" (List.nth h i);
    List.iter
      (fun (nm, l) ->
         let (v, i) = List.nth l i in
         Printf.fprintf ch "\"%s\" %f %f\n" nm v (if i <> i then 0. else i))
      t;
    Printf.fprintf ch "e\n"
  done

let output_table r l =
  let t = merge_columns l in
  let t = normalize (if !reference <= 0 then r - 1 else !reference - 1) t in
  stats t;
  if !table then
    text_output t
  else begin
    let ch = Unix.open_process_out "gnuplot -persist" in
    gnuplot_output ch t;
    close_out ch
  end

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
*)

let f _ =
  let o = read_column times opt in
  let b = read_column times byte in
  let c1 = read_column (times ^ "/v8") js_of_ocaml in
  let c2 = read_column (times ^ "/nitro") js_of_ocaml in
  let c3 = read_column (times ^ "/tm") js_of_ocaml in
  output_table 3 [o; b; c1; c2; c3]

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

let f _ =
  let c1 = read_column sizes (sub_spec js_of_ocaml "generated") in
  let c2 = read_column sizes js_of_ocaml_compact in
  let c3 = read_column sizes js_of_ocaml_inline in
  let c4 = read_column sizes js_of_ocaml_deadcode in
  output_table 1 [c1; c2; c3; c4]

(****)

let _ =
  let options =
    [("-ref", Arg.Set_int reference, "<col> use column <col> as the baseline");
     ("-max", Arg.Set_float maximum, "<m> truncate graph at level <max>");
     ("-table", Arg.Set table, " output a text table")]
  in
  Arg.parse (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  f ()
