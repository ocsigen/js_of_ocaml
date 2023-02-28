module RE = Runtime_events

type RE.User.tag += MyTag

let ev = RE.User.register "my_event" MyTag RE.Type.unit

let () = RE.User.write ev ()
