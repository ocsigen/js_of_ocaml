(* Differential check: parse each file with the recursive-descent parser
   and the Menhir parser, and compare the printed programs. *)
open Js_of_ocaml_compiler
open Stdlib

let print p =
  let buffer = Buffer.create 1024 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  let _ = Js_output.program pp p in
  Buffer.contents buffer

let read_file f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let dummy_report _ = ()

let only = ref `Both

let verbose = ref false

let run kind file =
  let src = read_file file in
  let old_res =
    match !only with
    | `Rd -> Error (0, 0)
    | `Both | `Menhir -> (
        match
          Parse_js_menhir.parse
            kind
            (Parse_js_menhir.Lexer.of_string
               ~report_error:dummy_report
               ~filename:file
               src)
        with
        | p -> Ok (print p)
        | exception Parse_js_menhir.Parsing_error pi -> Error (pi.Parse_info.line, pi.col)
        )
  in
  let new_res =
    match !only with
    | `Menhir -> Error (0, 0)
    | `Both | `Rd -> (
        match
          Parse_js.parse
            kind
            (Parse_js.Lexer.of_string ~report_error:dummy_report ~filename:file src)
        with
        | p -> Ok (print p)
        | exception Parse_js.Parsing_error pi -> Error (pi.Parse_info.line, pi.col)
        | exception e -> Error (-1, Hashtbl.hash (Printexc.to_string e)))
  in
  match old_res, new_res with
  | Ok a, Ok b when String.equal a b -> `Same
  | Error a, Error b when Poly.equal a b -> `Same_error
  | Ok a, Ok b ->
      if !verbose
      then (
        let write f s =
          let oc = open_out_bin f in
          output_string oc s;
          close_out oc
        in
        write "/tmp/parser_diff_menhir.js" a;
        write "/tmp/parser_diff_rd.js" b;
        ignore (Sys.command "diff -u /tmp/parser_diff_menhir.js /tmp/parser_diff_rd.js"));
      `Diff
  | Error (l, c), Ok _ -> `Old_error (l, c)
  | Ok _, Error (l, c) -> `New_error (l, c)
  | Error (l, c), Error (l', c') -> `Diff_error (l, c, l', c')

(* [--bench N]: parse every file N times with each parser and report CPU time *)
let bench n files =
  Config.Flag.enable "debuginfo";
  let srcs =
    List.filter_map files ~f:(fun file ->
        let src = read_file file in
        let ok_menhir =
          try
            ignore
              (Parse_js_menhir.parse
                 `Script
                 (Parse_js_menhir.Lexer.of_string ~report_error:dummy_report src));
            true
          with _ -> false
        in
        let ok_rd =
          try
            ignore
              (Parse_js.parse
                 `Script
                 (Parse_js.Lexer.of_string ~report_error:dummy_report src));
            let env = Flow_lexer.Lex_env.create (Sedlexing.Utf8.from_string src) in
            let rec loop env =
              match Flow_lexer.lex env with
              | Js_token.T_EOF, _ -> ()
              | _ -> loop env
            in
            loop env;
            true
          with _ -> false
        in
        if ok_menhir && ok_rd then Some (file, src) else None)
  in
  let time f =
    Gc.full_major ();
    let t0 = Sys.time () in
    for _ = 1 to n do
      f ()
    done;
    Sys.time () -. t0
  in
  let total_bytes =
    List.fold_left srcs ~init:0 ~f:(fun acc (_, s) -> acc + String.length s)
  in
  let menhir () =
    List.iter srcs ~f:(fun (_, src) ->
        ignore
          (Parse_js_menhir.parse
             `Script
             (Parse_js_menhir.Lexer.of_string ~report_error:dummy_report src)))
  in
  let rd () =
    List.iter srcs ~f:(fun (_, src) ->
        ignore
          (Parse_js.parse
             `Script
             (Parse_js.Lexer.of_string ~report_error:dummy_report src)))
  in
  let lex () =
    List.iter srcs ~f:(fun (_, src) ->
        let env = Flow_lexer.Lex_env.create (Sedlexing.Utf8.from_string src) in
        let rec loop env =
          let tok, _ = Flow_lexer.lex env in
          match tok with
          | Js_token.T_EOF -> ()
          | _ -> loop env
        in
        loop env)
  in
  (* warm up *)
  menhir ();
  rd ();
  let t_lex = time lex in
  let t_menhir = time menhir in
  let t_rd = time rd in
  let t_menhir' = time menhir in
  let t_rd' = time rd in
  Printf.printf
    "%d files, %d bytes, %d iterations\n\
     lexer only : %.3fs\n\
     menhir     : %.3fs / %.3fs\n\
     rd         : %.3fs / %.3fs\n\
     speedup (rd vs menhir, best of 2): %.2fx\n"
    (List.length srcs)
    total_bytes
    n
    t_lex
    t_menhir
    t_menhir'
    t_rd
    t_rd'
    (Float.min t_menhir t_menhir' /. Float.min t_rd t_rd')

(* Decode a UTF-8 string straight into the sedlex buffer, without the
   per-character generator/channel machinery of [Sedlexing.Utf8.from_string]. *)
let lexbuf_of_string s =
  let n = String.length s in
  let i = ref 0 in
  let refill buf pos len =
    let k = ref 0 in
    while !k < len && !i < n do
      let c = Char.code (String.unsafe_get s !i) in
      (if c < 0x80
       then (
         Array.unsafe_set buf (pos + !k) (Uchar.unsafe_of_int c);
         incr i)
       else
         let module H = Sedlexing.Utf8.Helper in
         let w = H.width s.[!i] in
         if !i + w > n then raise Sedlexing.MalFormed;
         let b j = Char.code s.[!i + j] in
         let u =
           match w with
           | 2 -> H.check_two c (b 1)
           | 3 -> H.check_three c (b 1) (b 2)
           | _ -> H.check_four c (b 1) (b 2) (b 3)
         in
         Array.unsafe_set buf (pos + !k) (Uchar.unsafe_of_int u);
         i := !i + w);
      incr k
    done;
    !k
  in
  let bytes_per_char u =
    let c = Uchar.to_int u in
    if c < 0x80 then 1 else if c < 0x800 then 2 else if c < 0x10000 then 3 else 4
  in
  Sedlexing.create ~bytes_per_char refill

(* [--gc FILE]: time and allocation of each phase on one file *)
let gc_stats file =
  Config.Flag.enable "debuginfo";
  let src = read_file file in
  let phase name f =
    let best = ref infinity and stats = ref (0., 0., 0.) in
    for _ = 1 to 3 do
      Gc.full_major ();
      let (minor0, promoted0, major0), t0 = Gc.counters (), Sys.time () in
      f ();
      let t1 = Sys.time () in
      let minor1, promoted1, major1 = Gc.counters () in
      best := Float.min !best (t1 -. t0);
      stats := minor1 -. minor0, promoted1 -. promoted0, major1 -. major0
    done;
    let minor, promoted, major = !stats in
    Printf.printf
      "%-14s %6.3fs  minor %7.1f MB  promoted %7.1f MB  major %7.1f MB\n%!"
      name
      !best
      (minor *. 8. /. 1e6)
      (promoted *. 8. /. 1e6)
      (major *. 8. /. 1e6)
  in
  (* Per-character floor of sedlex: consume the input the way a generated
     lexer does, marking the start of a lexeme every 8 characters *)
  let scan lb =
    let rec loop () =
      Sedlexing.start lb;
      let rec chars n =
        if n = 0
        then true
        else if Sedlexing.__private__next_int lb < 0
        then false
        else chars (n - 1)
      in
      if chars 8 then loop ()
    in
    loop ()
  in
  phase "decode" (fun () -> scan (Sedlexing.Utf8.from_string src));
  phase "decode(direct)" (fun () -> scan (lexbuf_of_string src));
  phase "lexer" (fun () ->
      let env = Flow_lexer.Lex_env.create (Sedlexing.Utf8.from_string src) in
      let rec loop env =
        match Flow_lexer.lex env with
        | Js_token.T_EOF, _ -> ()
        | _ -> loop env
      in
      loop env);
  phase "lexer(direct)" (fun () ->
      let env = Flow_lexer.Lex_env.create (lexbuf_of_string src) in
      let rec loop env =
        match Flow_lexer.lex env with
        | Js_token.T_EOF, _ -> ()
        | _ -> loop env
      in
      loop env);
  phase "lexer+array" (fun () ->
      let env = Flow_lexer.Lex_env.create (Sedlexing.Utf8.from_string src) in
      let a = ref (Array.make 64 (Js_token.T_EOF, Obj.magic 0)) and n = ref 0 in
      let rec loop env =
        let ((tok, _) as res) = Flow_lexer.lex env in
        if !n = Array.length !a
        then (
          let a' = Array.make (2 * !n) (Js_token.T_EOF, Obj.magic 0) in
          Array.blit ~src:!a ~src_pos:0 ~dst:a' ~dst_pos:0 ~len:!n;
          a := a');
        !a.(!n) <- res;
        incr n;
        match tok with
        | Js_token.T_EOF -> ()
        | _ -> loop env
      in
      loop env);
  phase "rd" (fun () ->
      ignore
        (Parse_js.parse `Script (Parse_js.Lexer.of_string ~report_error:dummy_report src)));
  phase "menhir" (fun () ->
      ignore
        (Parse_js_menhir.parse
           `Script
           (Parse_js_menhir.Lexer.of_string ~report_error:dummy_report src)))

let () =
  match Array.to_list Sys.argv with
  | [ _; "--gc"; file ] ->
      gc_stats file;
      exit 0
  | _ -> ()

let () =
  match Array.to_list Sys.argv with
  | _ :: "--bench" :: n :: files ->
      bench (int_of_string n) files;
      exit 0
  | _ -> ()

let () =
  Config.Flag.enable "debuginfo";
  let same = ref 0 and total = ref 0 in
  let files =
    List.filter
      (List.tl (Array.to_list Sys.argv))
      ~f:(function
        | "--only-rd" ->
            only := `Rd;
            false
        | "--only-menhir" ->
            only := `Menhir;
            false
        | "-v" ->
            verbose := true;
            false
        | _ -> true)
  in
  List.iter files ~f:(fun file ->
      incr total;
      let report kind = function
        | `Same | `Same_error ->
            incr same;
            true
        | `Diff ->
            Printf.printf "%s [%s]: DIFF\n%!" file kind;
            false
        | `Old_error (l, c) ->
            Printf.printf "%s [%s]: only menhir fails at %d:%d\n%!" file kind l c;
            false
        | `New_error (l, c) ->
            Printf.printf "%s [%s]: only rd fails at %d:%d\n%!" file kind l c;
            false
        | `Diff_error (l, c, l', c') ->
            Printf.printf
              "%s [%s]: both fail, menhir at %d:%d, rd at %d:%d\n%!"
              file
              kind
              l
              c
              l'
              c';
            false
      in
      let r =
        try report "script" (run `Script file)
        with Stack_overflow ->
          Printf.printf "%s: stack overflow\n%!" file;
          false
      in
      if not r then ignore (report "module" (run `Module file)));
  Printf.printf "%d/%d files identical\n" !same !total
