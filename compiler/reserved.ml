


let keyword =
  ["break"; "case"; "catch"; "do"; "else"; "for"; "if"; "in"; "new";
   "this"; "throw"; "try"; "var"; "void"; "while"; "with"; "class";
   "enum"; "super"; "const"; "yield"; "let" ]


let provided = [
  "ActiveXObject";
  "Array";
  "Date";
  "Math";
  "JSON";
  "Object";
  "RegExp";
  "String";
  "XMLHttpRequest";
  "decodeURI";
  "decodeURIComponent";
  "encodeURI";
  "encodeURIComponent";
  "escape";
  "event";
  "isNaN";
  "parseFloat";
  "parseInt";
  "location";
  "window";
  "unescape";
  "this";
  "true"; "false"; "undefined"; "null"
]

let reserved = Hashtbl.create 107

let add s = if String.length s <= 5 then Hashtbl.replace reserved s ()

let mem s = Hashtbl.mem reserved s

let _ =
  List.iter add keyword;
  List.iter add provided
