open Js_of_ocaml_compiler

let files = Sys.argv |> Array.to_list |> List.tl

let col (l1 : Lexing.position) = l1.pos_cnum - l1.pos_bol

let loc_equal (l1 : Lexing.position) (l2 : Lexing.position) =
  l1.pos_lnum = l2.pos_lnum && col l1 = col l2

let loc { Lexing.pos_lnum; pos_cnum; pos_bol; pos_fname } =
  Printf.sprintf "%s:%d:%d" pos_fname pos_lnum (pos_cnum - pos_bol)

let () =
  List.iter
    (fun f ->
      let c1 = open_in_bin f in
      let c2 = open_in_bin f in
      let l1 = Lexing.from_channel c1 in
      Lexing.set_position
        l1
        { Lexing.pos_fname = f; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      if true then Lexing.set_filename l1 f;
      let l2 = Sedlexing.Utf8.from_channel c2 in
      if true then Sedlexing.set_filename l2 f;
      Sedlexing.set_position
        l2
        { Lexing.pos_fname = f; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      Sedlexing.start l2;
      let f1 () =
        let t = Js_lexer.main l1 in
        t, l1.lex_start_p, l1.lex_curr_p
      in
      let drop1 () = Js_lexer.drop_line l1 in
      let f2, drop2 =
        let env = ref (Flow_lexer.Lex_env.create l2) in
        let f () =
          let nenv, res = Flow_lexer.token !env in
          env := nenv;
          let p = Flow_lexer.Lex_result.loc res in
          Flow_lexer.Lex_result.token res, fst p, snd p
        in
        let drop () = Flow_lexer.drop_line !env in
        f, drop
      in

      try
        while true do
          let t1, p1, p1' = f1 () in
          let t2, p2, p2' = f2 () in
          if false
          then
            Printf.eprintf
              "%s: %s\n%s: %s\n"
              (loc p2)
              (Js_token.to_string_extra t1)
              (loc p2)
              (Js_token.to_string_extra t2);
          if true
          then
            if t1 <> t2
            then (
              Printf.eprintf
                "Token mismatch: %s:%s <> %s\n"
                (loc p1)
                (Js_token.to_string_extra t1)
                (Js_token.to_string_extra t2);
              drop1 ();
              drop2 ())
            else if true && ((not (loc_equal p1 p2)) || not (loc_equal p1' p2'))
            then
              Printf.eprintf
                "Location mismatch for %s: %s-%s <> %s-%s\n"
                (Js_token.to_string_extra t1)
                (loc p1)
                (loc p1')
                (loc p2)
                (loc p2');
          match t1, t2 with
          | T_EOF, T_EOF -> raise Exit
          | _ -> ()
        done
      with Exit -> ())
    files
