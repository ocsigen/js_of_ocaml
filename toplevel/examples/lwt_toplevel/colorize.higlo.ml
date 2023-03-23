open Js_of_ocaml
open Js_of_ocaml_tyxml

let text ~a_class:cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])

let ocaml ~a_class:cl s =
  let tks = Higlo.Lang.parse ~lang:"ocaml" s in
  let span' cl (s, _) = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ]) in
  let make_span = function
    | Higlo.Lang.Bcomment s -> span' "comment" s
    | Higlo.Lang.Constant s -> span' "constant" s
    | Higlo.Lang.Directive s -> span' "directive" s
    | Higlo.Lang.Escape s -> span' "escape" s
    | Higlo.Lang.Id s -> span' "id" s
    | Higlo.Lang.Keyword (level, s) -> span' (Printf.sprintf "kw%d" level) s
    | Higlo.Lang.Lcomment s -> span' "comment" s
    | Higlo.Lang.Numeric s -> span' "numeric" s
    | Higlo.Lang.String s -> span' "string" s
    | Higlo.Lang.Symbol (level, s) -> span' (Printf.sprintf "sym%d" level) s
    | Higlo.Lang.Text s -> span' "text" s
    | Higlo.Lang.Title (_, s) -> span' "text" s
  in
  Tyxml_js.Html.(div ~a:[ a_class [ cl ] ] (List.map make_span tks))

let highlight (`Pos from_) to_ e =
  let _ =
    List.fold_left
      (fun pos e ->
        match Js.Opt.to_option (Dom_html.CoerceTo.element e) with
        | None -> pos
        | Some e ->
            let size = Js.Opt.case e##.textContent (fun () -> 0) (fun t -> t##.length) in
            if pos + size > from_ && (to_ = `Last || `Pos pos < to_)
            then e##.classList##add (Js.string "errorloc");
            pos + size)
      0
      (Dom.list_of_nodeList e##.childNodes)
  in
  ()
