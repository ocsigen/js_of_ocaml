open Util

let%expect_test "loop at the start of an the exception handler" =
  let program =
    compile_and_parse
      {|
type buf = {mutable pos : int; str : string}
let string_sub = String.sub
let read_line buf =
  let start = buf.pos in
  try
    while buf.str.[buf.pos] != '\n' do
      buf.pos <- buf.pos + 1
    done;
    let l =
      if buf.pos > 0 && buf.str.[buf.pos - 1] = '\r' then buf.pos - start - 1
      else buf.pos - start
    in
    let s = string_sub buf.str start l in
    buf.pos <- buf.pos + 1;
    s
  with _ ->
    let len = String.length buf.str in
    string_sub buf.str buf.pos (len - buf.pos)
  |}
  in
  print_fun_decl program (Some "read_line");
  [%expect
    {|
    function read_line(buf){
     var start = buf[1];
     try{
      for(;;){
       if(10 === caml_string_get(buf[2], buf[1])){
        a:
        {
         if(0 < buf[1] && 13 === caml_string_get(buf[2], buf[1] - 1 | 0)){var l = (buf[1] - start | 0) - 1 | 0; break a;}
         var l = buf[1] - start | 0;
        }
        var s = caml_call3(string_sub, buf[2], start, l);
        buf[1] = buf[1] + 1 | 0;
        return s;
       }
       buf[1] = buf[1] + 1 | 0;
      }
     }
     catch(exn){
      var len = runtime.caml_ml_string_length(buf[2]);
      return caml_call3(string_sub, buf[2], buf[1], len - buf[1] | 0);
     }
    }
    //end
    |}]
