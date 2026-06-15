
# Module `Js_of_ocaml_lwt.XmlHttpRequest`

```ocaml
type readyState = Js_of_ocaml.XmlHttpRequest.readyState = 
  | UNSENT
  | OPENED
  | HEADERS_RECEIVED
  | LOADING
  | DONE
```
```ocaml
type _ response = _ Js_of_ocaml.XmlHttpRequest.response = 
  | ArrayBuffer : Js_of_ocaml.Typed_array.arrayBuffer Js_of_ocaml.Js.t
                  Js_of_ocaml.Js.Opt.t
                  response
  | Blob : Js_of_ocaml.File.blob Js_of_ocaml.Js.t Js_of_ocaml.Js.Opt.t response
  | Document : Js_of_ocaml.Dom.element Js_of_ocaml.Dom.document Js_of_ocaml.Js.t
               Js_of_ocaml.Js.Opt.t
               response
  | JSON : 'a Js_of_ocaml.Js.Opt.t response
  | Text : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t response
  | Default : string response
```
```ocaml
class type  xmlHttpRequest = object ... end
```
```ocaml
class type  xmlHttpRequestUpload = object ... end
```
```ocaml
val create : unit -> xmlHttpRequest Js_of_ocaml.Js.t
```
The next part of this module allow one to use Ocaml with no need for Javascript documentation.

```ocaml
module Event = Js_of_ocaml.XmlHttpRequest.Event
```
```ocaml
type 'response generic_http_frame = {
  url : string;
  code : int;
  headers : string -> string option;
  content : 'response;
  content_xml : unit ->
  Js_of_ocaml.Dom.element Js_of_ocaml.Dom.document Js_of_ocaml.Js.t option;
}
```
The type for XHR results. The code field is the http status code of the answer. The headers field is a function associating values to any header name.

```ocaml
type http_frame = string generic_http_frame
```
```ocaml
exception Wrong_headers of int * (string -> string option)
```
The exception raise by perform functions when the check\_headers parameter returned false. The parameter of the exception is a function is like the `headers` function of `http_frame`

```ocaml
val perform_raw : 
  ?headers:(string * string) list ->
  ?content_type:string ->
  ?get_args:(string * string) list ->
  ?check_headers:(int -> (string -> string option) -> bool) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?contents:
    [< `POST_form of (string * Js_of_ocaml.Form.form_elt) list
    | `Form_contents of Js_of_ocaml.Form.form_contents
    | `String of string
    | `Blob of Js_of_ocaml.File.blob Js_of_ocaml.Js.t ] ->
  ?override_mime_type:string ->
  ?override_method:
    [< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ] ->
  ?with_credentials:bool ->
  response_type:'a Js_of_ocaml.XmlHttpRequest.response ->
  string ->
  'a generic_http_frame Lwt.t
```
`perform_raw` is the same as [`perform_raw_url`](./#val-perform_raw_url) except that an additional response\_type argument can be given to set the XMLHttpRequest responseType, and hence return different types of data for GET requests.

```ocaml
val perform_raw_url : 
  ?headers:(string * string) list ->
  ?content_type:string ->
  ?get_args:(string * string) list ->
  ?check_headers:(int -> (string -> string option) -> bool) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?contents:
    [< `POST_form of (string * Js_of_ocaml.Form.form_elt) list
    | `Form_contents of Js_of_ocaml.Form.form_contents
    | `String of string
    | `Blob of Js_of_ocaml.File.blob Js_of_ocaml.Js.t ] ->
  ?override_mime_type:string ->
  ?override_method:
    [< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ] ->
  ?with_credentials:bool ->
  string ->
  http_frame Lwt.t
```
`perform_raw_url` makes an asynchronous request to the specified `url` with specified options. The result is a cancelable thread returning an HTTP frame. By default, if `post_args` and `form_arg` are `None`, a GET request is used. If `post_args` or `form_arg` is `Some _` (even `Some []`) then a POST request is made. But if `override_method` is set, the request method is forced, no matter the `post_args` or `form_arg` value. For example, with `override_method` set to ``PUT` and `form_arg` set to `Some _` a PUT request including the form data will be made. The `check_headers` argument is run as soon as the answer code and headers are available. If it returns false, the request is canceled and the functions raise the `Wrong_headers` exception

```ocaml
val perform : 
  ?headers:(string * string) list ->
  ?content_type:string ->
  ?get_args:(string * string) list ->
  ?check_headers:(int -> (string -> string option) -> bool) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?contents:
    [< `POST_form of (string * Js_of_ocaml.Form.form_elt) list
    | `Form_contents of Js_of_ocaml.Form.form_contents
    | `String of string
    | `Blob of Js_of_ocaml.File.blob Js_of_ocaml.Js.t ] ->
  ?override_mime_type:string ->
  ?override_method:
    [< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ] ->
  ?with_credentials:bool ->
  Js_of_ocaml.Url.url ->
  http_frame Lwt.t
```
`perform` is the same as [`perform_raw_url`](./#val-perform_raw_url) except that the Url argument has type [`Js_of_ocaml.Url.url`](./../js_of_ocaml/Js_of_ocaml-Url.md#type-url).

```ocaml
val get : string -> http_frame Lwt.t
```
`get url` makes an asynchronous request to the specified `url`
