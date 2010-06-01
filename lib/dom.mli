
open Js

class type ['node] nodeList = object
  method item : int -> 'node t meth
  method length : int readonly_prop
end

class type node = object
  method nodeName : js_string t readonly_prop
  method nodeValue : js_string t opt readonly_prop
  method nodeType : int readonly_prop
  method parentNode : node t opt prop
  method childNodes : node nodeList t prop
  method firstChild : node t opt prop
  method lastChild : node t opt prop
  method previousSibling : node t opt prop
  method nextSibling : node t opt prop

  method insertBefore : node t -> node t opt -> node t meth
  method replaceChild : node t -> node t -> node t meth
  method removeChild : node t -> node t meth
  method appendChild : node t -> node t meth
  method hasChildNodes : bool t meth
  method cloneNode : bool t -> node t meth
end

val insertBefore : #node t -> #node t -> #node t opt -> unit
val replaceChild : #node t -> #node t -> #node t -> unit
val removeChild : #node t -> #node t -> unit
val appendChild : #node t -> #node t -> unit

class type element = object
  inherit node
  method tagName : js_string t readonly_prop
  method getAttribute : js_string t -> js_string t meth
  method setAttribute : js_string t -> js_string t -> unit meth
  method removeAttribute : js_string t -> unit meth
  method hasAttribyte : js_string t -> bool t meth
end

class type characterData = object
  inherit node
  method data : js_string t prop
  method length : int readonly_prop
  method subjs_stringData : int -> int -> js_string t meth
  method appendData : js_string t -> unit meth
  method insertData : int -> js_string t -> unit meth
  method deleteData : int -> int -> unit meth
  method replaceData : int -> int -> js_string t meth
end

class type text = characterData

class type documentFragment = node

class type ['element] document = object
  inherit element
  method documentElement : 'element t readonly_prop
  method createDocumentFragment : documentFragment t meth
  method createElement : js_string t -> 'element t meth
  method createTextNode : js_string t -> text t meth
  method getElementById : js_string t -> 'element t opt meth
end
