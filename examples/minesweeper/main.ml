open Dom
let js = JsString.of_string
let document = HTML.document

let int_input name value =
  let res = document##createDocumentFragment() in
  Dom.appendChild res (HTML.document##createTextNode(js name));
  let input = HTML.createInputElement document in
  input##_type <- js"text";
  input##value <- JsString.of_int !value;
  input##onchange <- Nullable.some
    (fun _ ->
       begin try
         value := int_of_string (JsString.to_string (input##value))
       with Invalid_argument _ ->
         ()
       end;
       input##value <- JsString.of_int !value;
       Js._false);
  Dom.appendChild res input;
  res

let button name callback =
  let res = HTML.document##createDocumentFragment() in
  let input = HTML.createInputElement document in
  input##_type <- js"submit";
  input##value <- js name;
  input##onclick <- Nullable.some callback;
  Dom.appendChild res input;
  res

let onload _ =
  let main =
    match Nullable.maybe HTML.document##getElementById(js"main") with
      Some div -> div
    | None     -> assert false
  in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
  Dom.appendChild main (int_input "Number of columns" nbr);
  Dom.appendChild main (HTML.createBrElement document);
  Dom.appendChild main (int_input "Number of rows" nbc);
  Dom.appendChild main (HTML.createBrElement document);
  Dom.appendChild main (int_input "Number of mines" nbm);
  Dom.appendChild main (HTML.createBrElement document);
  Dom.appendChild main
    (button "nouvelle partie"
       (fun _ ->
          let div = HTML.createDivElement HTML.document in
          Dom.appendChild main div;
          Minesweeper.run div !nbc !nbr !nbm;
          Js._false))

let _ = HTML.window##onload <- onload
