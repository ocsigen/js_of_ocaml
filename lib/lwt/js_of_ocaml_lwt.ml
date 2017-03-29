
module XmlHttpRequest = struct
  include Js_of_ocaml.XmlHttpRequest
  include Lwt_xmlHttpRequest
end

module File = struct
  include Js_of_ocaml.File
  include Lwt_file
end

module Jsonp = Lwt_jsonp
