open Js_of_ocaml_compiler
open Stdlib

let normalize_path s =
  let s =
    String.map
      ~f:(function
        | '\\' -> '/' (* Normalize windows path for the tests *)
        | x -> x)
      s
  in
  Filename.basename s

let read_file f =
  let ic = open_in_bin f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_uleb contents pos =
  let rec go shift acc =
    let b = Char.code contents.[!pos] in
    incr pos;
    let acc = acc lor ((b land 0x7f) lsl shift) in
    if b land 0x80 <> 0 then go (shift + 7) acc else acc
  in
  go 0 0

(* Ranges (as [(start, end)] offsets) of the entries of the code section of a
   Wasm module. Each entry corresponds to a function. *)
let function_ranges contents =
  let pos =
    ref 8
    (* magic number and version *)
  in
  let len = String.length contents in
  let ranges = ref [] in
  while !pos < len do
    let id = Char.code contents.[!pos] in
    incr pos;
    let size = read_uleb contents pos in
    let section_end = !pos + size in
    if id = 10 (* code section *)
    then begin
      let count = read_uleb contents pos in
      for _ = 1 to count do
        let sz = read_uleb contents pos in
        let start = !pos in
        ranges := (start, start + sz) :: !ranges;
        pos := start + sz
      done
    end;
    pos := section_end
  done;
  List.rev !ranges

let interesting_file f =
  match f with
  | "a.ml" | "b.ml" | "c.ml" | "d.ml" | "test_bc.ml" -> true
  | _ -> false

let dump wasm_file (sm : Source_map.Standard.t) =
  let sources = Array.of_list sm.sources in
  let mappings = Source_map.Mappings.decode_exn sm.mappings in
  let entries =
    List.filter_map mappings ~f:(fun (m : Source_map.map) ->
        match m with
        | Gen _ -> None
        | Gen_Ori { gen_col; ori_line; ori_col; ori_source; _ }
        | Gen_Ori_Name { gen_col; ori_line; ori_col; ori_source; _ } ->
            Some (gen_col, normalize_path sources.(ori_source), ori_line, ori_col))
  in
  let pos_to_string (_, src, line, col) = Printf.sprintf "%s:%d:%d" src line col in
  List.iter
    (function_ranges (read_file wasm_file))
    ~f:(fun (start, end_) ->
      let entries =
        List.filter entries ~f:(fun (off, _, _, _) -> off >= start && off < end_)
      in
      let interesting =
        List.exists entries ~f:(fun (_, src, _, _) -> interesting_file src)
      in
      match entries with
      | ((first_offset, _, _, _) as first) :: rem when interesting ->
          let last = List.fold_left rem ~init:first ~f:(fun _ e -> e) in
          (* The Chrome profiler attributes the samples within a function to
             its start offset, so report whether there is a mapping there. *)
          Printf.printf
            "function%s: %s -- %s\n"
            (if first_offset = start then "" else " (no mapping at start)")
            (pos_to_string first)
            (pos_to_string last)
      | _ -> ())

let () =
  let dir = Sys.argv.(1) in
  let files = Sys.readdir dir |> Array.to_list |> List.sort ~cmp:String.compare in
  List.iter files ~f:(fun f ->
      if Filename.check_suffix f ".map"
      then begin
        (* Remove the hash from the file name *)
        let name =
          match String.index_opt f '-' with
          | Some i -> String.sub f ~pos:0 ~len:i ^ ".wasm.map"
          | None -> f
        in
        Printf.printf "sourcemap for %s\n" name;
        let wasm_file = Filename.chop_suffix (Filename.concat dir f) ".map" in
        match Source_map.of_file (Filename.concat dir f) with
        | Standard sm -> dump wasm_file sm
        | Index _ -> assert false
      end)
