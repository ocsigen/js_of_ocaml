open Common

let log_stop = log_start "Url test suite"
let url_string_url u = Url.url_of_string (Url.string_of_url u)
let () = match Url.Current.get () with
  | None -> log_failure "can't parse current url"
  | Some u -> match url_string_url u with
    | None -> log_failure "can't parse pretty-printed url"
    | Some v ->
       if u = v then
         log_success ()
       else
         log_failure "no fixpoint"
let () =
  let t1 = Url.urlencode "/toto+ blah&tutu" in
  let t2 = Url.urlencode ~with_plus:false "/toto+ blah&tutu" in
  if t1 = "/toto%2B%20blah%26tutu" && t2 = "/toto+%20blah%26tutu" then
    log_success ()
  else
    log_failure "escaping error"

let () = log_stop ()
