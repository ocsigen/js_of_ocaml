#! /usr/bin/ocaml unix.cma

#use "lib/common.ml"

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

let column_stats l =
  let l = List.map (fun (x, l) -> fst (List.hd l)) l in
  let a = Array.of_list l in
  Array.sort compare a;
  let p = List.fold_right (fun x p -> x *. p) l 1. in
  Format.eprintf "%f %f@." (p ** (1. /. float (List.length l)))
    a.(Array.length a / 2)

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
  column_stats l;
  l

let rec merge_columns l =
  match l with
    [] -> []
  | [c] -> c
  | c :: r -> merge (fun v1 v2 -> v1 @ v2) c (merge_columns r)

let normalize n t =
  List.map
    (fun (nm, l) ->
       let (r, _) = List.nth l n in
       if r <> r then begin
         Format.eprintf "No reference available for '%s'@." nm;
         exit 1
       end;
       (nm, List.map (fun (v, i) -> (v /. r, i /. r)) l))
    t

let text_output t =
  List.iter
    (fun (nm, l) ->
       Format.printf "%s" nm;
       List.iter (fun (m, i) -> Format.printf " %f %f" m i) l;
       Format.printf "@.")
    t

let gnuplot_output t =
  let n = List.length (snd (List.hd t)) in
  Format.printf "\
    set style data histograms@.
    set style fill   solid 1.00 border -1@.
    set style histogram errorbars gap 1 lw 1@.
    set xtics border in scale 1,0.5 nomirror rotate by -45  \
              offset character 0, 0, 0@.";
  Format.printf "plot";
  for i = 0 to n - 1 do
    if i > 0 then Format.printf ",";
    Format.printf " \"-\" using 2:3:xtic(1)";
  done;
  Format.printf "@.";
  for i = 0 to n - 1 do
    List.iter
      (fun (nm, l) ->
         let (v, i) = List.nth l i in
         Format.printf "\"%s\" %f %f@." nm v i)
      t;
    Format.printf "e@."
  done

let output_table t = gnuplot_output t

(*
let _ =
  let c1 = read_column (meas ^ "/v8") js_of_ocaml in
  let c2 = read_column (meas ^ "/v8") ocamljs in
  let t = merge (fun v1 v2 -> v1 @ v2) c1 c2 in
  output_table t
*)

(*
let _ =
  let c1 = read_column meas opt in
  let c2 = read_column meas byte in
  let c3 = read_column (meas ^ "/v8") js_of_ocaml in
  let t = merge_columns [c3; c2; c1] in
  let t = normalize 0 t in
  output_table t
*)

(*
let _ =
  let c1 = read_column (meas ^ "/v8") js_of_ocaml in
  let c2 = read_column (meas ^ "/v8") js_of_ocaml_unsafe in
  let t = merge (fun v1 v2 -> v1 @ v2) c1 c2 in
  output_table t
*)

let _ =
  let c1 = read_column (meas ^ "/v8") js_of_ocaml in
  let c2 = read_column (meas ^ "/nitro") js_of_ocaml in
  let c3 = read_column (meas ^ "/tm") js_of_ocaml in
  let t = merge_columns [c1; c2; c3] in
  let t = normalize 0 t in
  output_table t

(*
let _ =
  let engine = "tm" in
  let c1 = read_column (meas ^ "/" ^ engine) js in
  let c2 = read_column (meas ^ "/" ^ engine) js_of_ocaml in
  let t = merge (fun v1 v2 -> v1 @ v2) c1 c2 in
  let t = normalize 1 t in
  output_table t
*)

(*
- provide column legend
  ==> title columnhead
- normalize to a given column
- automate plotting
- report average and median improvement

======================

set style data histograms
set style fill   solid 1.00 border -1
set style histogram errorbars gap 1 lw 1
set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0
plot "/tmp/t.txt" using 2:3:xtic(1), "" using 4:5




--ref n
*)
