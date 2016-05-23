
let text ~a_class:cl s =
  Tyxml_js.Html.(span ~a:[a_class [cl]] [pcdata s])

#ifdef higlo
let ocaml ~a_class:cl s =
  let tks = Higlo.parse ~lang:"ocaml" s in
  let span' cl s = Tyxml_js.Html.(span ~a:[a_class [cl]] [pcdata s]) in
  let make_span = function
    | Higlo.Bcomment s -> span' "comment" s
    | Higlo.Constant s -> span' "constant" s
    | Higlo.Directive s -> span' "directive" s
    | Higlo.Escape s -> span' "escape" s
    | Higlo.Id s -> span' "id" s
    | Higlo.Keyword (level,s) -> span' (Printf.sprintf "kw%d" level) s
    | Higlo.Lcomment s -> span' "comment" s
    | Higlo.Numeric s -> span' "numeric" s
    | Higlo.String s -> span' "string" s
    | Higlo.Symbol (level,s) -> span' (Printf.sprintf "sym%d" level) s
    | Higlo.Text s -> span' "text" s in
  Tyxml_js.Html.(div ~a:[a_class [cl]] (List.map make_span tks))
#else
let ocaml = text
#endif


#ifdef higlo
let highlight (`Pos from_) to_ e =
  let _ =
    List.fold_left (fun pos e ->
      match Js.Opt.to_option (Dom_html.CoerceTo.element e) with
      | None -> pos
      | Some e ->
	 let size = Js.Opt.case (e##textContent) (fun () -> 0) (fun t -> t##length) in
	 if pos + size > from_ && (to_ = `Last || `Pos pos < to_)
	 then e##classList##add(Js.string "errorloc");
	 pos + size) 0 (Dom.list_of_nodeList (e##childNodes)) in
  ();;
#else
let highlight from_ to_ e =
  match Js.Opt.to_option e##textContent with
  | None -> assert false
  | Some x ->
     let x = Js.to_string x in
     let `Pos from_  = from_ in
     let to_ = match to_ with `Pos n -> n | `Last -> String.length x - 1 in
     e##innerHTML <- Js.string "";
     let span kind s =
       if s <> ""
       then
         let span = Tyxml_js.Html.(span ~a:[a_class [kind]] [pcdata s]) in
         Dom.appendChild e (Tyxml_js.To_dom.of_element span) in
     span "normal"   (String.sub x 0 from_);
     span "errorloc" (String.sub x from_ (to_ - from_));
     span "normal"   (String.sub x to_ (String.length x - to_))
#endif
