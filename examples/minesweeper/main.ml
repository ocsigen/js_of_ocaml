open Dom
let js = JsString.of_string

let int_input name value =
  let res = Fragment.create () in
  Fragment.append res (Node.text (js name)) ;
  let input = Node.element (js"input") in
  Node.set_attribute input (js"type") (js"text") ;
  Node.set_attribute input (js"value") (JsString.of_int !value) ;
  Node.register_event input "onchange"
    (fun _ ->
      (value :=
         try
           int_of_string
             (JsString.to_string (Node.get_attribute input "value"))
         with Invalid_argument _ ->
           !value);
      Node.set_attribute input (js"value") (JsString.of_int !value);
      Js._false);
  Fragment.append res input;
  res

let button name callback =
  let res = Fragment.create () in
  let input = Node.element (js"input") in
  Node.set_attribute input (js"type") (js"submit") ;
  Node.set_attribute input (js"value") (js name) ;
  Node.register_event input "onclick" callback;
  Fragment.append res input;
  res

let div id =
  let div = Node.element (js"div") in
  Node.set_attribute div (js"id") (js id);
  div

let uid = let uid = ref 0 in fun () -> incr uid ; "caml__" ^ string_of_int  !uid

let onload _ =
  let main = Node.get_element_by_id Node.document (js"main") in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
  Fragment.flush main (int_input "Number of columns" nbr);
  Node.append main (Node.element (js"br"));
  Fragment.flush main  (int_input "Number of rows" nbc);
  Node.append main (Node.element (js"br"));
  Fragment.flush main  (int_input "Number of mines" nbm);
  Node.append main (Node.element (js"br"));
  Fragment.flush main
             (button "nouvelle partie"
                 (fun _ ->
                   let id = uid () in
                   Node.append main (div id);
                   Minesweeper.run
                     id
                     (string_of_int !nbc)
                     (string_of_int !nbr)
                     (string_of_int !nbm);
                   Js._false));
  Js._false
;;

Node.register_event window "onload" onload
