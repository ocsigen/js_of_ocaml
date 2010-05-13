open Dom
let js = JsString.of_string

let int_input name value =
  let res = HTML.document##createDocumentFragment() in
  Dom.appendChild res (HTML.document##createTextNode(js name));
  let input = HTML.document##createElement(js"input") in
  input##setAttribute(js"type", js"text") ;
  input##setAttribute(js"value", JsString.of_int !value) ;
  input##onchange <-
    (fun _ ->
      (value :=
         try
           int_of_string
             (JsString.to_string (input##getAttribute(js"value")))
         with Invalid_argument _ ->
           !value);
      input##setAttribute(js"value", JsString.of_int !value);
      Js._false);
  Dom.appendChild res input;
  res

let button name callback =
  let res = HTML.document##createDocumentFragment() in
  let input = HTML.document##createElement(js"input") in
  input##setAttribute(js"type", js"submit");
  input##setAttribute(js"value", js name);
  input##onclick <- callback;
  Dom.appendChild res input;
  res

let div id =
  let div = HTML.document##createElement(js"div") in
  div##setAttribute(js"id", js id);
  div

let uid = let uid = ref 0 in fun () -> incr uid ; "caml__" ^ string_of_int  !uid

let onload _ =
  let main =
    match Nullable.maybe HTML.document##getElementById(js"main") with
      Some div -> div
    | None     -> assert false
  in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
  Dom.appendChild main (int_input "Number of columns" nbr);
  Dom.appendChild main (HTML.document##createElement(js"br"));
  Dom.appendChild main (int_input "Number of rows" nbc);
  Dom.appendChild main (HTML.document##createElement(js"br"));
  Dom.appendChild main (int_input "Number of mines" nbm);
  Dom.appendChild main (HTML.document##createElement(js"br"));
  Dom.appendChild main
             (button "nouvelle partie"
                 (fun _ ->
                   let id = uid () in
                   ignore (Dom.appendChild main (div id));
                   Minesweeper.run
                     id
                     (string_of_int !nbc)
                     (string_of_int !nbr)
                     (string_of_int !nbm);
                   Js._false))
;;

HTML.window##onload <- onload
