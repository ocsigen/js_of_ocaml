(* Js_of_ocaml examples
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2008 Benjamin Canou
 *
 *           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
 *  TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
 *
 *)
open Js_of_ocaml
module Html = Dom_html

let js = Js.string

let document = Html.window##.document

let int_input name value =
  let lab = Html.createLabel document in
  Dom.appendChild lab (document##createTextNode (js name));
  let input = Html.createInput ~_type:(js "text") document in
  input##.value := js (string_of_int !value);
  input##.onchange :=
    Html.handler (fun _ ->
        (try value := int_of_string (Js.to_string input##.value)
         with Invalid_argument _ -> ());
        input##.value := js (string_of_int !value);
        Js._false);
  Dom.appendChild lab input;
  lab

let () =
  let main = Js.Opt.get (document##getElementById (js "main")) (fun () -> assert false) in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
  let ctrl = Html.createDiv document in
  ctrl##.className := js "controls";
  let fields = Html.createDiv document in
  fields##.className := js "controls-fields";
  Dom.appendChild fields (int_input "Columns" nbr);
  Dom.appendChild fields (int_input "Rows" nbc);
  Dom.appendChild fields (int_input "Mines" nbm);
  Dom.appendChild ctrl fields;
  let btn = Html.createInput ~_type:(js "submit") document in
  btn##.value := js "New Game";
  let board_div = Html.createDiv document in
  let new_game () =
    Js.Opt.iter board_div##.firstChild (fun c -> Dom.removeChild board_div c);
    let div = Html.createDiv document in
    Dom.appendChild board_div div;
    Minesweeper.run div !nbc !nbr !nbm
  in
  btn##.onclick :=
    Html.handler (fun _ ->
        new_game ();
        Js._false);
  Dom.appendChild ctrl btn;
  Dom.appendChild main ctrl;
  Dom.appendChild main board_div;
  new_game ()
