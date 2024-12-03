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

let split_on_char ~sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep
    then (
      r := String.sub s ~pos:(i + 1) ~len:(!j - i - 1) :: !r;
      j := i)
  done;
  String.sub s ~pos:0 ~len:!j :: !r

let mean a =
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    s := !s +. a.(i)
  done;
  !s /. float (Array.length a)

let mean_variance a =
  let m = mean a in
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    let d = a.(i) -. m in
    s := !s +. (d *. d)
  done;
  (* https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
     https://en.wikipedia.org/wiki/Bessel%27s_correction *)
  m, !s /. float (Array.length a - 1)

(*        90%    95%    98%    99%    99.5%  99.8%  99.9%*)
let tinv_table =
  [| 1, [| 6.314; 12.71; 31.82; 63.66; 127.3; 318.3; 636.6 |]
   ; 2, [| 2.920; 4.303; 6.965; 9.925; 14.09; 22.33; 31.60 |]
   ; 3, [| 2.353; 3.182; 4.541; 5.841; 7.453; 10.21; 12.92 |]
   ; 4, [| 2.132; 2.776; 3.747; 4.604; 5.598; 7.173; 8.610 |]
   ; 5, [| 2.015; 2.571; 3.365; 4.032; 4.773; 5.893; 6.869 |]
   ; 6, [| 1.943; 2.447; 3.143; 3.707; 4.317; 5.208; 5.959 |]
   ; 7, [| 1.895; 2.365; 2.998; 3.499; 4.029; 4.785; 5.408 |]
   ; 8, [| 1.860; 2.306; 2.896; 3.355; 3.833; 4.501; 5.041 |]
   ; 9, [| 1.833; 2.262; 2.821; 3.250; 3.690; 4.297; 4.781 |]
   ; 10, [| 1.812; 2.228; 2.764; 3.169; 3.581; 4.144; 4.587 |]
   ; 11, [| 1.796; 2.201; 2.718; 3.106; 3.497; 4.025; 4.437 |]
   ; 12, [| 1.782; 2.179; 2.681; 3.055; 3.428; 3.930; 4.318 |]
   ; 13, [| 1.771; 2.160; 2.650; 3.012; 3.372; 3.852; 4.221 |]
   ; 14, [| 1.761; 2.145; 2.624; 2.977; 3.326; 3.787; 4.140 |]
   ; 15, [| 1.753; 2.131; 2.602; 2.947; 3.286; 3.733; 4.073 |]
   ; 16, [| 1.746; 2.120; 2.583; 2.921; 3.252; 3.686; 4.015 |]
   ; 17, [| 1.740; 2.110; 2.567; 2.898; 3.222; 3.646; 3.965 |]
   ; 18, [| 1.734; 2.101; 2.552; 2.878; 3.197; 3.610; 3.922 |]
   ; 19, [| 1.729; 2.093; 2.539; 2.861; 3.174; 3.579; 3.883 |]
   ; 20, [| 1.725; 2.086; 2.528; 2.845; 3.153; 3.552; 3.850 |]
   ; 21, [| 1.721; 2.080; 2.518; 2.831; 3.135; 3.527; 3.819 |]
   ; 22, [| 1.717; 2.074; 2.508; 2.819; 3.119; 3.505; 3.792 |]
   ; 23, [| 1.714; 2.069; 2.500; 2.807; 3.104; 3.485; 3.767 |]
   ; 24, [| 1.711; 2.064; 2.492; 2.797; 3.091; 3.467; 3.745 |]
   ; 25, [| 1.708; 2.060; 2.485; 2.787; 3.078; 3.450; 3.725 |]
   ; 26, [| 1.706; 2.056; 2.479; 2.779; 3.067; 3.435; 3.707 |]
   ; 27, [| 1.703; 2.052; 2.473; 2.771; 3.057; 3.421; 3.690 |]
   ; 28, [| 1.701; 2.048; 2.467; 2.763; 3.047; 3.408; 3.674 |]
   ; 29, [| 1.699; 2.045; 2.462; 2.756; 3.038; 3.396; 3.659 |]
   ; 30, [| 1.697; 2.042; 2.457; 2.750; 3.030; 3.385; 3.646 |]
   ; 40, [| 1.684; 2.021; 2.423; 2.704; 2.971; 3.307; 3.551 |]
   ; 50, [| 1.676; 2.009; 2.403; 2.678; 2.937; 3.261; 3.496 |]
   ; 60, [| 1.671; 2.000; 2.390; 2.660; 2.915; 3.232; 3.460 |]
   ; 80, [| 1.664; 1.990; 2.374; 2.639; 2.887; 3.195; 3.416 |]
   ; 100, [| 1.660; 1.984; 2.364; 2.626; 2.871; 3.174; 3.390 |]
   ; 120, [| 1.658; 1.980; 2.358; 2.617; 2.860; 3.160; 3.373 |]
  |]

let tinv_row n =
  let i = ref 1 in
  let l = Array.length tinv_table in
  while !i < l && fst tinv_table.(!i) <= n do
    incr i
  done;
  snd tinv_table.(!i - 1)

let tinv95 n = (tinv_row n).(1)

let tinv98 n = (tinv_row n).(2)

let tinv99 n = (tinv_row n).(3)

let mean_with_confidence a =
  let m, v = mean_variance a in
  let l = Array.length a in
  m, sqrt v /. sqrt (float l) *. tinv98 (l - 1)

let src = "sources"

let code = "build"

let hostname = Unix.gethostname ()

let times = Filename.concat "results/times" hostname

let sizes = "results/sizes"

let compiletimes = Filename.concat "results/compiletimes" hostname

module Spec : sig
  type t

  val create : string -> string -> t

  val dir : root:string -> t -> string

  val file : root:string -> t -> string -> string

  val no_ext : t -> t

  val sub_spec : t -> string -> t

  val find_names : root:string -> t -> string list

  val ml : t

  val js : t

  val byte : t

  val opt : t

  val js_of_ocaml : t

  val js_of_ocaml_o3 : t

  val js_of_ocaml_js_string : t

  val byte_unsafe : t

  val opt_unsafe : t

  val js_of_ocaml_unsafe : t

  val js_of_ocaml_inline : t

  val js_of_ocaml_deadcode : t

  val js_of_ocaml_compact : t

  val js_of_ocaml_call : t

  val js_of_ocaml_effects : t
end = struct
  type t =
    { dir : string
    ; ext : string
    }

  let create dir ext = { dir; ext }

  let no_ext { dir; _ } = { dir; ext = "" }

  let file ~root { dir; ext } nm = Format.sprintf "%s/%s/%s%s" root dir nm ext

  let dir ~root { dir; _ } = Format.sprintf "%s/%s" root dir

  let sub_spec { dir; ext } loc = { dir = Format.sprintf "%s/%s" dir loc; ext }

  let find_names ~root spec =
    let dir = dir ~root spec in
    Sys.readdir dir
    |> Array.to_list
    |> List.filter ~f:(fun nm ->
           let open Unix in
           match stat (dir ^ "/" ^ nm) with
           | { st_kind = S_REG | S_LNK; _ } -> true
           | _ -> false)
    |> (if spec.ext = ""
        then fun x -> x
        else
          fun x ->
            x
            |> List.filter ~f:(fun nm -> Filename.check_suffix nm spec.ext)
            |> List.map ~f:Filename.chop_extension)
    |> List.sort ~cmp:compare

  let ml = create "ml" ".ml"

  let js = create "js" ".js"

  let byte = create "byte" ""

  let opt = create "opt" ""

  let js_of_ocaml = create "js_of_ocaml" ".js"

  let js_of_ocaml_o3 = create "o3" ".js"

  let js_of_ocaml_js_string = create "jsstring" ".js"

  let byte_unsafe = create "unsafe/byte" ""

  let opt_unsafe = create "unsafe/opt" ""

  let js_of_ocaml_unsafe = create "unsafe/js_of_ocaml" ".js"

  let js_of_ocaml_inline = create "noinline" ".js"

  let js_of_ocaml_deadcode = create "nodeadcode" ".js"

  let js_of_ocaml_compact = create "notcompact" ".js"

  let js_of_ocaml_call = create "nooptcall" ".js"

  let js_of_ocaml_effects = create "effects" ".js"
end

let rec mkdir d =
  if not (Sys.file_exists d)
  then (
    mkdir (Filename.dirname d);
    Unix.mkdir d 0o777)

let need_update src dst =
  try
    let d = Unix.stat dst in
    d.Unix.st_kind <> Unix.S_REG
    ||
    let s = Unix.stat src in
    d.Unix.st_mtime < s.Unix.st_mtime
  with Unix.Unix_error (Unix.ENOENT, _, _) -> true

let measures_need_update code meas spec nm =
  let p = Spec.file ~root:code spec nm in
  let m = Spec.file ~root:meas (Spec.no_ext spec) nm in
  need_update p m

let read_measures meas spec nm =
  let m = Spec.file ~root:meas (Spec.no_ext spec) nm in
  let l = ref [] in
  if Sys.file_exists m
  then (
    let ch = open_in m in
    (try
       while true do
         l := float_of_string (input_line ch) :: !l
       done
     with End_of_file -> ());
    close_in ch;
    !l)
  else []

let write_measures meas spec nm l =
  let m = Spec.file ~root:meas (Spec.no_ext spec) nm in
  let tmp = Spec.file ~root:meas (Spec.no_ext spec) "_tmp_" in
  mkdir (Spec.dir ~root:meas spec);
  let ch = open_out tmp in
  List.iter ~f:(fun t -> Printf.fprintf ch "%f\n" t) (List.rev l);
  close_out ch;
  Sys.rename tmp m
