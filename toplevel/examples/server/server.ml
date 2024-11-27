open Lwt
open Cohttp
open Cohttp_lwt_unix
open Re

let _ = Findlib.init ()

let filesys = ref (Findlib.default_location ())

let server () =
  let re_filesys =
    compile (seq [ str "/filesys/"; group (seq [ str !filesys; rep any ]); eos ])
  in
  let header typ =
    let h = Header.init () in
    let h = Header.add h "Content-Type" typ in
    let h = Header.add h "Server" "iocaml" in
    h
  in
  let header_html = header "text/html; charset=UTF-8" in
  let header_js = header "application/javascript; charset=UTF-8" in
  let header_css = header "text/css; charset=UTF-8" in
  let header_plain_user_charset = header "text/plain; charset=x-user-defined" in
  let callback _conn_id req _body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    try
      (* send binary file *)
      let fname = Group.get (exec re_filesys path) 1 in
      Lwt_io.eprintf "filesys: %s\n" fname
      >>= fun () -> Server.respond_file ~headers:header_plain_user_charset ~fname ()
    with _ ->
      (* send static file *)
      let fname = Path.resolve_local_file ~docroot:"." ~uri in
      Lwt_io.eprintf "static: %s\n" fname
      >>= fun () ->
      let headers =
        if Filename.check_suffix fname ".css"
        then header_css
        else if Filename.check_suffix fname ".js"
        then header_js
        else header_html
      in
      Server.respond_file ~headers ~fname ()
  in
  let server = Server.make ~callback () in
  Server.create server

let () = Lwt_main.run (server ())
