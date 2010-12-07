#! /usr/bin/ocaml unix.cma

#use "lib/common.ml"

(****)

let reference = ref (-1)
let maximum = ref (-1.)
let gnuplot = ref true
let table = ref false
let omitted = ref []
let errors = ref false
let script = ref false

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

let read_column ?title ?color meas spec =
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
      Some nm -> nm
    | None    -> dir meas (no_ext spec)
  in
  ([(nm, color)], l)

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
    let (nm, _) = List.nth h i in
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
  List.iter (fun (s, _) -> Format.printf " - \"%s\"" s) h;
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
    set style fill solid 1 border rgb 'black'\n\
    set style histogram errorbars gap 1%s\n\
    set xtics border in scale 0,0 nomirror rotate by -30  \
              offset character 0, 0, 0\n"
    (if !errors then " lw 1" else "");
 if !maximum > 0. then
   Printf.fprintf ch "set yrange [0:%f]\n" !maximum
 else
   Printf.fprintf ch "set yrange [0:]\n";
 Printf.fprintf ch "plot";
  for i = 0 to n - 1 do
    if i > 0 then Printf.fprintf ch ", \"-\" using 2:3 title columnhead lw 0"
    else Printf.fprintf ch " \"-\" using 2:3:xtic(1) title columnhead lw 0";
    match snd (List.nth h i) with
      Some c -> Printf.fprintf ch "lc rgb '%s'" c
    | None   -> ()
  done;
  Printf.fprintf ch "\n";
  for i = 0 to n - 1 do
    Printf.fprintf ch "- - \"%s\"\n" (fst (List.nth h i));
    List.iter
      (fun (nm, l) ->
         let (v, i) = List.nth l i in
         Printf.fprintf ch "\"%s\" %f %f\n" nm v (if i <> i then 0. else i))
      t;
    Printf.fprintf ch "e\n"
  done

let filter (h, t) =
  (h, List.filter (fun (nm, _) -> not (List.mem nm !omitted)) t)

let output_table r (l:  ((string * 'a option) list * _) list)=
  let t = merge_columns l in
  let t = filter t in
  let t = normalize (if !reference <= 0 then r - 1 else !reference - 1) t in
  stats t;
  if !table then
    text_output t
  else if !script then
    gnuplot_output stdout t
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

let f _ =
  let c1 = read_column (times ^ "/v8") js_of_ocaml in
  let c2 = read_column (times ^ "/v8") js_of_ocaml_unsafe in
  output_table 1 [c1; c2]

let f _ =
  let o = read_column ~color:"#729fcf" times opt in
  let b = read_column ~color:"#204a87" times byte in
  let c1 = read_column ~color:"#d98e2d" (times ^ "/v8") js_of_ocaml in
  let c2 = read_column ~color:"#a75f0c" (times ^ "/nitro") js_of_ocaml in
  let c3 = read_column ~color:"#a40000" (times ^ "/tm") js_of_ocaml in
  output_table 2 [o; b; c1; c2; c3]
(*
*)
let f _ =
  let o = read_column ~title:"ocamlopt" ~color:"#729fcf" times opt in
  let b = read_column ~title:"ocamlc" ~color:"#326bbe" times byte in
  let c1 = read_column ~title:"V8" ~color:"#d98e2d" (times ^ "/v8") js_of_ocaml in
  let c2 = read_column ~title:"Nitro" ~color:"#a75f0c" (times ^ "/nitro") js_of_ocaml in
  output_table 2 [o; b; c1; c2]


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

let _ =
  let options =
    [("-ref", Arg.Set_int reference, "<col> use column <col> as the baseline");
     ("-max", Arg.Set_float maximum, "<m> truncate graph at level <max>");
     ("-table", Arg.Set table, " output a text table");
     ("-omit", Arg.String (fun s -> omitted := str_split s ',' @ !omitted),
      " omit the given benchmarks");
     ("-errors", Arg.Set errors, " display error bars");
     ("-script", Arg.Set script, " output gnuplot script")]
  in
  Arg.parse (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  f ()

(*
http://hacks.mozilla.org/2009/07/tracemonkey-overview/
http://weblogs.mozillazine.org/bz/archives/020732.html
*)

(*
./report.ml -max 4 -omit hamming
*)
