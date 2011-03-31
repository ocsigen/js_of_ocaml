open Js
open Dom_html

class type blob = object
  method size : int readonly_prop
  method _type : js_string t readonly_prop
  method slice : int -> int -> blob meth
  method slice_withContentType : int -> int -> js_string t -> blob meth
end

class type file = object
  inherit blob
  method name : js_string t readonly_prop
  method lastModifiedDate : js_string t readonly_prop
end

type file_any = < > t

module CoerceTo = struct
  let string (e : file_any) =
    if typeof e = string "string"
    then Js.some (Unsafe.coerce e:js_string t)
    else Js.null
end

class type fileList = object
  inherit [file] Dom.nodeList
end

class type fileError = object
  method code : int readonly_prop
end

type readyState = EMPTY | LOADING | DONE

class type fileReader = object ('self)

  method readAsArrayBuffer : blob t -> unit meth
  method readAsBinaryString : blob t -> unit meth
  method readAsText : blob t -> unit meth
  method readAsText_withEncoding : blob t -> js_string t -> unit meth
  method readAsDataURL : blob t -> unit meth

  method abort : unit meth

  method readyState : readyState readonly_prop

  method result : file_any readonly_prop
  method error : fileError t readonly_prop

  method onloadstart : ('self t, event t) event_listener writeonly_prop
  method onprogress : ('self t, event t) event_listener writeonly_prop
  method onload : ('self t, event t) event_listener writeonly_prop
  method onabort : ('self t, event t) event_listener writeonly_prop
  method onerror : ('self t, event t) event_listener writeonly_prop
  method onloadend : ('self t, event t) event_listener writeonly_prop

  inherit Dom_html.eventTarget
end

let fileReader () : fileReader t constr = Unsafe.variable "FileReader"

let reader kind file =
  let reader = jsnew (fileReader ()) () in
  let (res, w) = Lwt.task () in
  reader##onloadend <- handler
    (fun _ ->
      if reader##readyState = DONE then
        Lwt.wakeup w 
	  (match Opt.to_option (CoerceTo.string (reader##result)) with
	    | None -> assert false (* can't happen: called with good readAs_ *)
	    | Some s -> s)
      else (); (* CCC TODO: handle errors *)
      Js._false);
  Lwt.on_cancel res (fun () -> reader##abort ());
  (match kind with
    | `BinaryString -> reader##readAsBinaryString(file)
    | `Text -> reader##readAsText(file)
    | `Text_withEncoding e -> reader##readAsText_withEncoding(file,e)
    | `DataURL -> reader##readAsDataURL(file));
  res

let readAsBinaryString file =
  reader `BinaryString file

let readAsText file =
  reader `Text file

let readAsText_withEncoding file e =
  reader (`Text_withEncoding e) file

let readAsDataURL file =
  reader `DataURL file
