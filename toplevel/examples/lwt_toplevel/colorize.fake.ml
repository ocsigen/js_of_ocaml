open Js_of_ocaml
open Js_of_ocaml_tyxml

let text ~a_class:cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])

let ocaml = text

let highlight from_ to_ e =
  match Js.Opt.to_option e##.textContent with
  | None -> assert false
  | Some x ->
      let x = Js.to_string x in
      let (`Pos from_) = from_ in
      let to_ =
        match to_ with
        | `Pos n -> n
        | `Last -> String.length x - 1
      in
      e##.innerHTML := Js.string "";
      let span kind s =
        if s <> ""
        then
          let span = Tyxml_js.Html.(span ~a:[ a_class [ kind ] ] [ txt s ]) in
          Dom.appendChild e (Tyxml_js.To_dom.of_element span)
      in
      span "normal" (String.sub x 0 from_);
      span "errorloc" (String.sub x from_ (to_ - from_));
      span "normal" (String.sub x to_ (String.length x - to_))
