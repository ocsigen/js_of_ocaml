(* Js_of_ocaml compiler
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

let series = ref None

let stop_profiling () =
  match !series with
  | Some _x ->
      (* Spacetime.Series.save_and_close x; *)
      series := None
  | None -> ()

let start_profiling name =
  let path = name ^ ".spacetime" in
  stop_profiling ();
  Format.eprintf "Start profiling %s\n%!" path;
  (* series := Some (Spacetime.Series.create ~path); *)
  ()

let take_snapshot () =
  match !series with
  | None -> ()
  | Some _series ->
      Gc.minor ();
      (* Spacetime.Snapshot.take series; *)
      ()

let debugs : (string * bool ref) list ref = ref []

let available () = List.map !debugs ~f:fst

let find ?(even_if_quiet = false) s =
  let state =
    match List.string_assoc s !debugs with
    | Some s -> s
    | None ->
        let state = ref false in
        debugs := (s, state) :: !debugs;
        state
  in
  fun () ->
    if String.equal s "times" then take_snapshot ();
    (even_if_quiet || not !quiet) && !state

let enable s =
  match List.string_assoc s !debugs with
  | Some s -> s := true
  | None -> failwith (Printf.sprintf "The debug named %S doesn't exist" s)

let disable s =
  match List.string_assoc s !debugs with
  | Some s -> s := false
  | None -> failwith (Printf.sprintf "The debug named %S doesn't exist" s)
