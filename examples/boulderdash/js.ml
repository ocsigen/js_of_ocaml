(***********************************************************************)
(*                                                                     *)
(*                              O'Browser                              *)
(*                                                                     *)
(*                           Benjamin Canou                            *)
(*                                                                     *)
(*  Copyright 2008 Benjamin Canou.         This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

external js_external : string -> int -> ('a -> 'b) option = "caml_js_external"

external http_get_with_status : string -> (int * string) = "caml_js_http_get_with_status"
let http_get url = snd (http_get_with_status url)

(* url -> content type -> data -> (status * result) *)
external http_post : string -> string -> string -> (int * string) = "caml_js_http_post"

external dom_of_xml : string -> JSOO.obj = "caml_js_dom_of_xml"
external pretty_xml_of_dom : JSOO.obj -> string = "caml_js_pretty_xml_of_dom"
external xml_of_dom : JSOO.obj -> string = "caml_js_xml_of_dom"

external alert : string -> unit = "caml_js_alert"
external params : unit -> string array = "caml_js_params"
let params = params ()
external exec : string -> string array -> unit = "caml_js_exec"
external enable_utf8 : bool -> unit = "caml_js_enable_utf8"
external utf8_enabled : unit -> bool = "caml_js_utf8_enabled"

module Node = struct
  open JSOO
  type t = obj
  let document = eval "document"    
  let text content =
    document >>> call_method "createTextNode" [| string content |]
  let element tag =
    document >>> call_method "createElement" [| string tag |]
  let get_attribute node name =
    try node >>> get name >>> as_string with _ -> failwith "get_attribute"
  let set_attribute node name value =
    node >>> call_method "setAttribute" [| string name ; string value |] >>> ignore
  let remove_attribute node name =
    try node >>> set name (inject Nil) with _ -> failwith "remove"
  let get_element_by_id root id =
    root >>> call_method "getElementById" [| string id |]
  let register_event node name fn arg =
    node >>> set name (wrap_event (fun _ -> ignore (fn arg)))
  let clear_event node name =
    node >>> set name (inject Nil)
  let append node child =
    node >>> call_method "appendChild" [| child |] >>> ignore
  let remove node child =
    node >>> call_method "removeChild" [| child |] >>> ignore
  external children : t -> t list = "caml_js_node_children"
  external n_children : t -> int = "caml_js_node_n_children"
  external child : t -> int -> t = "caml_js_node_child"
  let iter f n =
    for i = 0 to n_children n - 1 do
      f (child n i)
    done
  let fold_left f s n =
    let m = n_children n in
    let rec fold i r = if i >= m then r else fold (i + 1) (f r (child n i)) in
      fold 0 s
  let empty n = List.iter (remove n) (children n)
  let replace_all n c = empty n ; append n c
end

module Fragment = struct
  type t
  external create : unit -> t = "caml_js_fragment_create"
  external append : t -> Node.t -> unit = "caml_js_fragment_append"
  external flush : Node.t -> t -> unit = "caml_js_fragment_flush"
end

let get_element_by_id = Node.get_element_by_id Node.document

module Html = struct
  let rec set_attrs m attrs =
    match attrs with 
      | [] -> ()
      | (n, v) :: attrs -> Node.set_attribute m n v ; set_attrs m attrs

  let set_attr_opt m n v =
    match v with 
      | None -> ()
      | Some v -> Node.set_attribute m n v

  let register_event_opt m n v =
    match v with 
      | None -> ()
      | Some f -> Node.register_event m n f ()
	  
  let create n ?(attrs = []) () =
    let m = Node.element n in
      set_attrs m attrs ;
      m

  let img ?src ?alt ?style ?(attrs = []) () =
    let m = create "img" ~attrs:attrs () in
      set_attr_opt m "src" src ;
      set_attr_opt m "alt" alt ;
      set_attr_opt m "style" style ;
      m

  let div ?style ?(attrs = []) children =
    let m = create "div" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let li ?style ?(attrs = []) children =
    let m = create "li" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let ol ?style ?(attrs = []) children =
    let m = create "ol" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let ul ?style ?(attrs = []) children =
    let m = create "ul" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let span ?style ?(attrs = []) children =
    let m = create "span" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let table ?style ?(attrs = []) children =
    let m = create "table" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let tr ?style ?(attrs = []) children =
    let m = create "tr" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let td ?style ?(attrs = []) children =
    let m = create "td" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let map_table ?style ?(attrs = []) ?tr_style ?(tr_attrs = []) ?td_style ?(td_attrs = []) f t =
    let m = table ?style ~attrs:attrs [] in
      set_attr_opt m "style" style ;
      for y = 0 to Array.length t - 1 do
	let tr = tr ?style:tr_style ~attrs:tr_attrs [] in
	  for x = 0 to Array.length t.(y) - 1 do
	    let td = td ?style:td_style ~attrs:td_attrs [f y x t.(y).(x)]in
	      Node.append tr td ;
	  done ;
	  Node.append m tr ;
      done ;
      m
	    
  let h1 ?style ?(attrs = []) children =
    let m = create "h1" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      List.iter (Node.append m) children ;
      m

  let select ?style ?onchange ?(attrs = []) children =
    let m = create "select" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      register_event_opt m "onchange" onchange ;
      List.iter (Node.append m) children ;
      m

  let option ?style ?onclick ?(attrs = []) children =
    let m = create "option" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      register_event_opt m "onclick" onclick ;
      List.iter (Node.append m) children ;
      m

  let a ?style ?onclick ?href ?name ?(attrs = []) children =
    let m = create "a" ~attrs:attrs () in
      set_attr_opt m "style" style ;
      (match name with
	 | Some s -> set_attr_opt m "name" name ; set_attr_opt m "href" href
	 | None ->
	     set_attr_opt m "href"
	       (match href with Some "#" | None -> Some "javascript:;" | _ -> href)) ;
      register_event_opt m "onclick" onclick ;
      List.iter (Node.append m) children ;
      m

  type 'a input = {
    get: unit -> 'a;
    set: 'a -> unit;
    editable: bool -> unit;
    node: Node.t;
    mutable callback: 'a input -> unit;
  }
  let input format parse value size editable callback =
    let node = create "input"
      ~attrs:["type", "text"; "value", format value;
	      "size", string_of_int size ] () in
    let value = ref value in
    let input = {
	get = (fun () -> !value);
	set = (fun x -> value := x ; Node.set_attribute node "value" (format x));
	editable = (fun b -> Node.set_attribute node "disabled" (if b then "false" else  "true"));
	node = node;
	callback = callback;
    } in
      if not editable then Node.set_attribute node "disabled" "disabled" ;
      Node.register_event node "onchange"
	(fun () ->
	   (try value := parse (Node.get_attribute node "value") with _ -> input.set !value) ;
	   input.callback input (* do not simplify *)) () ;
      input

  let int_input ?(editable = true) ?(size = 8) ?(value = 0) ?(callback = fun _ -> ()) () =
    input string_of_int int_of_string value size editable callback
    
  let string s = Node.text s

  let int i = Node.text (string_of_int i)

  let br = create "br"

end

let unquote_id_item =
  let t = Hashtbl.create 200 in
    Hashtbl.add t "nbsp" " " ;
    Hashtbl.add t "sp" " " ;
    Hashtbl.add t "amp" "&" ;
    Hashtbl.add t "sl" "/" ;
    (fun x -> try Hashtbl.find t x with _ -> x)

let decode_id s  =
  let sz = String.length s in
  let rec split p i acc =
    if i >= sz then
      [String.sub s p (i - p) :: acc]
    else
      if i + 1 >= sz then
	[String.sub s p (i - p + 1) :: acc]
      else
	if s.[i] = ':' then (
	  if s.[i + 1] = ':' then (
	    (String.sub s p (i - p) :: acc) :: split (i + 2) (i + 2) []
	  ) else ( (* quote *)
	    let rec quoted i =
	      if i >= sz then i else
		if s.[i] = ':' then i else quoted (i + 1)
	    in let i' = quoted (i + 1) in
	      split (i' + 1) (i' + 1) (unquote_id_item (String.sub s (i + 1) (i' - i - 1)) :: String.sub s p (i - p) :: acc)
	  )
	) else
	  split p (succ i) acc
  in List.map (List.fold_left (fun r s -> s ^ r) "") (split 0 0 [])
;;

