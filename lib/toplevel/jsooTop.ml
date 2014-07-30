open Format
open Compiler
let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

let setup_fun () =
  Hashtbl.add Toploop.directive_table "enable" (Toploop.Directive_string Option.Optim.enable);
  Hashtbl.add Toploop.directive_table "disable" (Toploop.Directive_string Option.Optim.disable);
  Hashtbl.add Toploop.directive_table "debug_on" (Toploop.Directive_string Option.Debug.enable);
  Hashtbl.add Toploop.directive_table "debug_off" (Toploop.Directive_string Option.Debug.disable);
  Hashtbl.add Toploop.directive_table "tailcall" (Toploop.Directive_string (fun s ->
      let x = Option.Tailcall.of_string s in
      Option.Tailcall.set x));
  Hashtbl.add Toploop.directive_table "optim" (Toploop.Directive_int (fun i ->
      Driver.set_profile i));
  Topdirs.dir_directory "/cmis";
  let initial_primitive_count =
    Array.length (split_primitives (Symtable.data_primitive_names ())) in

  let compile s =
    let prims =
      split_primitives (Symtable.data_primitive_names ()) in
    let unbound_primitive p =
      try ignore (Js.Unsafe.eval_string p); false with _ -> true in
    let stubs = ref [] in
    Array.iteri
      (fun i p ->
         if i >= initial_primitive_count && unbound_primitive p then
           stubs :=
             Format.sprintf
               "function %s(){caml_failwith(\"%s not implemented\")}" p p
             :: !stubs)
      prims;
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    let res = Buffer.contents b in
    let res = String.concat "" !stubs ^ res in
    Js.Unsafe.eval_string res
  in
  Js.Unsafe.global##toplevelCompile <- compile (*XXX HACK!*);
  ()

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len',nl =
      try String.index_from s !p '\n' - !p + 1,false
      with _ -> String.length s - !p,true in
    let len'' = min len len' in
    String.blit s !p buffer 0 len'';
    (match ppf with
     | Some ppf ->
       Format.fprintf ppf "%s" (String.sub buffer 0 len'');
       if nl then Format.pp_print_newline ppf ();
       Format.pp_print_flush ppf ()
     | None -> ());
    p:=!p + len'';
    len''

let use ffp content =
  let name = "/dev/fake_stdin" in
  Sys_js.register_file ~name ~content;
  Toploop.use_silently ffp name

let execute printval ?pp_code pp_answer s =
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) pp_code) in
  try
    while true do
      try
        let phr = !Toploop.parse_toplevel_phrase lb in
        ignore(Toploop.execute_phrase printval pp_answer phr)
      with
        End_of_file ->
        raise End_of_file
      | x ->
        Errors.report_error Format.err_formatter x
    done
  with End_of_file ->
    flush_all ()

let setup = Lazy.lazy_from_fun setup_fun

let initialize () =
  Sys.interactive := false;
  Lazy.force setup;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  Sys.interactive := true
