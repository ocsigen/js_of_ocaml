
open Js

type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

class type xmlHttpRequest = object
  method onreadystatechange : (unit -> unit) opt prop
  method readyState : readyState readonly_prop
  method _open :
    js_string t -> js_string t -> bool t -> unit meth
  method _open_full :
    js_string t -> js_string t -> bool t ->
    js_string t opt -> js_string t opt -> unit meth
  method setRequestHeader : js_string t -> js_string t -> unit meth
  method send : js_string t opt -> unit meth
  method send_document : #Dom.element #Dom.document -> unit meth
  method abort : unit meth
  method status : int readonly_prop
  method statusText : js_string t readonly_prop
  method getResponseHeader : js_string t -> js_string t opt meth
  method getAllResponseHeaders : js_string t meth
  method responseText : js_string t readonly_prop
  method responseXML : Dom.element Dom.document t opt readonly_prop
end

external create : unit -> xmlHttpRequest t = "createXMLHTTPObject"

let send_request url callback postData =
  let req = create () in
  let meth = Js.string (if postData = null then "GET" else "POST") in
  req##_open (meth, url, Js._true);
  req##setRequestHeader (Js.string "User-Agent", Js.string "XMLHTTP/1.0");
  Opt.iter postData
    (fun d -> req##setRequestHeader
                (Js.string "Content-type",
                 Js.string "application/x-www-form-urlencoded"));
  req##onreadystatechange <- Js.some
    (fun () ->
       if
         req##readyState = DONE && (req##status = 200 || req##status = 304)
       then
         callback req);
  if req##readyState <> DONE then req##send (postData)
