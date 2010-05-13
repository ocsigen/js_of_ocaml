
let window = Js.variable "window"

let alert msg = ignore (Js.meth_call window "alert" [| Js.string msg |])

external http_get_with_status : string -> (int * string) = "caml_js_http_get_with_status"
let http_get url = snd (http_get_with_status url)

module Node = struct
  type t = Js.t
  type nodeList

  let document = Js.variable "document"

  let get_element_by_id root id =
    Js.meth_call root "getElementById" [| Js.string id |]

  let text content =
    Js.meth_call document "createTextNode" [| Js.string content |]

  let element tag =
    Js.meth_call document "createElement" [| Js.string tag |]

  let get_attribute node name : JsString.t =
    Js.extract (Js.get node name)

  let set_attribute node name value =
    ignore (Js.meth_call node "setAttribute"
              [| Js.string name ; Js.string value |])

  let register_event node name fn =
    ignore (Js.set node name (Js.inject (fn : Js.t -> Js.bool)))

  let clear_event node name =
    Js.set node name Js.null

  let append node child =
    ignore (Js.meth_call node "appendChild" [| child |])
  let remove node child =
    ignore (Js.meth_call node "removeChild" [| child |])

  let children node : nodeList = Js.extract (Js.get node "childNodes")
  let first_child node : Js.t Nullable.t = Js.extract (Js.get node "firstChild")

  let rec empty n =
    match Nullable.maybe (first_child n) with
      Some c -> remove n c; empty n
    | None   -> ()

  let replace_all n c = empty n ; append n c
end

module Fragment = struct
  let create () = Js.meth_call Node.document "createDocumentFragment" [||]
  let append fr n = ignore (Js.meth_call fr "appendChild" [| n |])
  let flush n fr = ignore (Js.meth_call n "appendChild" [| fr |])
end

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
      | Some f -> Node.register_event m n f
	  
  let create n ?(attrs = []) () =
    let m = Node.element n in
      set_attrs m attrs ;
      m

  let div_s = JsString.of_string "div"
  let style_s = JsString.of_string "style"
  let img_s = JsString.of_string "img"
  let src_s = JsString.of_string "src"
  let alt_s = JsString.of_string "alt"
  let table_s = JsString.of_string "table"
  let tr_s = JsString.of_string "tr"
  let td_s = JsString.of_string "td"
  let h1_s = JsString.of_string "h1"
  let select_s = JsString.of_string "select"
  let option_s = JsString.of_string "option"
  let br_s = JsString.of_string "br"

  let img ?src ?alt ?style ?(attrs = []) () =
    let m = create img_s ~attrs:attrs () in
      set_attr_opt m src_s src ;
      set_attr_opt m alt_s alt ;
      set_attr_opt m style_s style ;
      m

  let div ?style ?(attrs = []) children =
    let m = create div_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      List.iter (Node.append m) children ;
      m

  let table ?style ?(attrs = []) children =
    let m = create table_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      List.iter (Node.append m) children ;
      m

  let tr ?style ?(attrs = []) children =
    let m = create tr_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      List.iter (Node.append m) children ;
      m

  let td ?style ?(attrs = []) children =
    let m = create td_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      List.iter (Node.append m) children ;
      m

  let map_table ?style ?(attrs = []) ?tr_style ?(tr_attrs = []) ?td_style ?(td_attrs = []) f t =
    let m = table ?style ~attrs:attrs [] in
      set_attr_opt m style_s style ;
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
    let m = create h1_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      List.iter (Node.append m) children ;
      m

  let select ?style ?onchange ?(attrs = []) children =
    let m = create select_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      register_event_opt m "onchange" onchange ;
      List.iter (Node.append m) children ;
      m

  let option ?style ?onclick ?(attrs = []) children =
    let m = create option_s ~attrs:attrs () in
      set_attr_opt m style_s style ;
      register_event_opt m "onclick" onclick ;
      List.iter (Node.append m) children ;
      m

  let br = create br_s

  let string = Node.text

  let int i = Node.text (JsString.of_int i)

end

module Dom = struct
  open Js.Obj

  class type ['node] nodeList = object
    method item : int -> 'node t meth
    method length : int readonly_prop
  end

  class type node = object
    method nodeName : Js.string readonly_prop
    method nodeValue : Js.string Nullable.t readonly_prop
    method nodeType : int readonly_prop
    method parentNode : node t Nullable.t prop
    method childNodes : node nodeList t prop
    method firstChild : node t Nullable.t prop
    method lastChild : node t Nullable.t prop
    method previousSibling : node t Nullable.t prop
    method nextSibling : node t Nullable.t prop

    method insertBefore : node t -> node t Nullable.t -> node t meth
    method replaceChild : node t -> node t -> node t meth
    method removeChild : node t -> node t meth
    method appendChild : node t -> node t meth
    method hasChildNodes : Js.bool meth
    method cloneNode : Js.bool -> node t meth
  end

  let appendChild (p : #node t) (n : #node t) =
    ignore (p##appendChild ((n :> node t)))

  class type ['element] element = object
    inherit node
    method tagName : Js.string readonly_prop
    method getAttribute : Js.string -> Js.string meth
    method setAttribute : Js.string -> Js.string -> unit meth
    method removeAttribute : Js.string -> unit meth
    method hasAttribyte : Js.string -> Js.bool meth
    method getElementsByTagName : Js.string -> 'element nodeList t meth
  end

  class type characterData =
  object
    inherit node
    method data : Js.string prop
    method length : int readonly_prop
    method substringData : int -> int -> Js.string meth
    method appendData : Js.string -> unit meth
    method insertData : int -> Js.string -> unit meth
    method deleteData : int -> int -> unit meth
    method replaceData : int -> int -> Js.string meth
  end

  class type text = object
    inherit characterData
  end

  class type documentFragment = object
    inherit node
  end

  class type ['element] document = object
    inherit ['element] element
    method documentElement : 'element t readonly_prop
    method createDocumentFragment : documentFragment t meth
    method createElement : Js.string -> 'element t meth
    method createTextNode : Js.string -> text t meth
    method getElementById : Js.string -> 'element t Nullable.t meth
    method getElementsByTagName : Js.string -> 'element nodeList t meth
  end
end

module HTML = struct
  open Js.Obj

  class type mouseEvent = object
  end

  class type element = object
    inherit [element] Dom.element
    method id : Js.string prop
    method title : Js.string prop
    method lang : Js.string prop
    method dir : Js.string prop
    method className : Js.string prop

    (* FIX: not portable! *)
    method onclick : (mouseEvent t Nullable.t -> Js.bool) prop
    (* FIX: should be on some specific elements *)
    method onchange : (unit -> Js.bool) prop
  end

  class type document = object
    inherit [element] Dom.document
    method title : Js.string prop
    method referrer : Js.string readonly_prop
    method domain : Js.string readonly_prop
(*URL?*)
    method body : element prop
    method cookie : Js.string prop
  end

  type interval_id
  type timeout_id

  class type location = object
    method hash : Js.string prop
    method host : Js.string prop
    method hostname : Js.string prop
    method href : Js.string prop
    method pathname : Js.string prop
    method protocol : Js.string prop
    method search : Js.string prop

    method reload : Js.bool -> unit meth
    method replace : Js.string -> unit meth
  end

  class type window = object
    method onload : (unit -> unit) prop
    method onbeforeunload : (unit -> Js.string) prop
    method location : location t prop

    method alert : Js.string -> unit meth
    method confirm : Js.string -> Js.bool meth

    method setInterval : (unit -> unit) -> float -> interval_id meth
    method clearInterval : interval_id -> unit meth

    method setTimeout : (unit -> unit) -> float -> timeout_id meth
    method clearTimeout : timeout_id -> unit meth
  end
(*XXX Creation functions a la lablgtk... *)

  let window : window t = Js.extract (Js.variable "window")
  let document : document t = Js.extract (Js.variable "document")

end
