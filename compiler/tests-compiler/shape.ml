open Util

(* Regression test: without memoization in the_shape_of, computing the
   shape of this function causes exponential blowup (2^n recursive
   calls for n levels of sharing). With memoization this is O(n). *)
let%expect_test "shape exponential blowup" =
  with_temp_dir ~f:(fun () ->
      let ocaml_prog =
        {|
let f (b : bool) =
  let f () = () in
  let g () = () in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  let g b () = if b then f' else g' in
  let f' b () = if b then f else g in
  let g' b () = if b then f else g in
  let f b () = if b then f' else g' in
  f
|}
      in
      let ocaml_file =
        ocaml_prog
        |> Filetype.ocaml_text_of_string
        |> Filetype.write_ocaml ~name:"test.ml"
      in
      let js_file =
        ocaml_file
        |> compile_ocaml_to_cmo
        |> compile_cmo_to_javascript ~pretty:false ~sourcemap:false
      in
      let content = js_file |> Filetype.read_js |> Filetype.string_of_js_text in
      let lines = String.split_on_char '\n' content in
      List.iter
        (fun line ->
          match String.trim line with
          | s when String.length s > 10 && String.sub s 0 10 = "//# shape:" ->
              print_endline s
          | _ -> ())
        lines);
  [%expect
    {| //# shape: Test:[F(1)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(2)*->F(1)*] |}]
