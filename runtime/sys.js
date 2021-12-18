// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Sys

//Provides: caml_raise_sys_error (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
  caml_raise_with_string(caml_global_data.Sys_error, msg);
}

//Provides: caml_sys_exit
//Requires: caml_invalid_argument
function caml_sys_exit (code) {
  var g = globalThis;
  if(g.quit) g.quit(code);
  //nodejs
  if(g.process && g.process.exit)
    g.process.exit(code);
  caml_invalid_argument("Function 'exit' not implemented");
}

//Provides: caml_is_special_exception
function caml_is_special_exception(exn){
  switch(exn[2]) {
  case -8: // Match_failure
  case -11: // Assert_failure
  case -12: // Undefined_recursive_module
    return 1;
  default:
    return 0;
  }
}

//Provides: caml_format_exception
//Requires: MlBytes, caml_is_special_exception
function caml_format_exception(exn){
  var r = "";
  if(exn[0] == 0) {
    r += exn[1][1];
    if(exn.length == 3 && exn[2][0] == 0 && caml_is_special_exception(exn[1])) {

      var bucket = exn[2];
      var start = 1;
    } else {
      var start = 2
      var bucket = exn;
    }
    r += "(";
    for(var i = start; i < bucket.length; i ++){
      if(i > start) r+=", ";
      var v = bucket[i]
      if(typeof v == "number")
        r+= v.toString();
      else if(v instanceof MlBytes){
        r+= '"' + v.toString() + '"';
      }
      else if(typeof v == "string"){
        r+= '"' + v.toString() + '"';
      }
      else r += "_";
    }
    r += ")"
  } else if (exn[0] == 248){
    r += exn[1]
  }
  return r
}

//Provides: caml_fatal_uncaught_exception
//Requires: caml_named_value, caml_format_exception
function caml_fatal_uncaught_exception(err){
  if(err instanceof Array && (err[0] == 0 || err[0] == 248)) {
    var handler = caml_named_value("Printexc.handle_uncaught_exception");
    if(handler) handler(err,false);
    else {
      var msg = caml_format_exception(err);
      var at_exit = caml_named_value("Pervasives.do_at_exit");
      if(at_exit) { at_exit(0) }
      globalThis.console.error("Fatal error: exception " + msg + "\n");
    }
  }
  else {
    throw err
  }
}


//Provides: caml_set_static_env
function caml_set_static_env(k,v){
  if(!globalThis.jsoo_static_env)
    globalThis.jsoo_static_env = {}
  globalThis.jsoo_static_env[k] = v;
  return 0;
}
//Provides: caml_sys_getenv (const)
//Requires: caml_raise_not_found
//Requires: caml_string_of_jsstring
//Requires: caml_jsstring_of_string
function caml_sys_getenv (name) {
  var g = globalThis;
  var n = caml_jsstring_of_string(name);
  //nodejs env
  if(g.process
     && g.process.env
     && g.process.env[n] != undefined)
    return caml_string_of_jsstring(g.process.env[n]);
  if(globalThis.jsoo_static_env
     && globalThis.jsoo_static_env[n])
    return caml_string_of_jsstring(globalThis.jsoo_static_env[n])
  caml_raise_not_found ();
}

//Provides: caml_sys_unsafe_getenv
//Requires: caml_sys_getenv
function caml_sys_unsafe_getenv(name){
  return caml_sys_getenv (name);
}

//Provides: caml_argv
//Requires: caml_string_of_jsstring
var caml_argv = ((function () {
  var g = globalThis;
  var main = "a.out";
  var args = []

  if(g.process
     && g.process.argv
     && g.process.argv.length > 1) {
    var argv = g.process.argv
    //nodejs
    main = argv[1];
    args = argv.slice(2);
  }

  var p = caml_string_of_jsstring(main);
  var args2 = [0, p];
  for(var i = 0; i < args.length; i++)
    args2.push(caml_string_of_jsstring(args[i]));
  return args2;
})())

//Provides: caml_executable_name
//Requires: caml_argv
var caml_executable_name = caml_argv[1]

//Provides: caml_sys_get_argv
//Requires: caml_argv
function caml_sys_get_argv (a) {
  return [0, caml_argv[1], caml_argv];
}

//Provides: caml_sys_argv
//Requires: caml_argv
function caml_sys_argv (a) {
  return caml_argv;
}

//Provides: caml_sys_modify_argv
//Requires: caml_argv
function caml_sys_modify_argv(arg){
  caml_argv = arg;
  return 0;
}

//Provides: caml_sys_executable_name const
//Requires: caml_executable_name
function caml_sys_executable_name(a){
  return caml_executable_name
}

//Provides: caml_sys_system_command
//Requires: caml_jsstring_of_string
function caml_sys_system_command(cmd){
  var cmd = caml_jsstring_of_string(cmd);
  if (typeof require != "undefined"
      && require('child_process')
      && require('child_process').execSync) {
    try {require('child_process').execSync(cmd,{stdio: 'inherit'}); return 0}
    catch (e) {return 1}
  }
  else return 127;
}

//Provides: caml_sys_time mutable
var caml_initial_time = (new Date()).getTime() * 0.001;
function caml_sys_time () {
  var now = (new Date()).getTime();
  return now * 0.001 - caml_initial_time;
}

//Provides: caml_sys_time_include_children
//Requires: caml_sys_time
function caml_sys_time_include_children(b) {
  return caml_sys_time();
}

//Provides: caml_sys_random_seed mutable
//The function needs to return an array since OCaml 4.0...
function caml_sys_random_seed () {
  var now = (new Date()).getTime();
  var x = now^0xffffffff*Math.random();
  return [0,x];
}

//Provides: caml_sys_const_big_endian const
function caml_sys_const_big_endian () { return 0; }

//Provides: caml_sys_const_word_size const
function caml_sys_const_word_size () { return 32; }

//Provides: caml_sys_const_int_size const
function caml_sys_const_int_size () { return 32; }

//Provides: caml_sys_const_max_wosize const
// max_int / 4 so that the following does not overflow
//let max_string_length = word_size / 8 * max_array_length - 1;;
function caml_sys_const_max_wosize () { return (0x7FFFFFFF/4) | 0;}

//Provides: caml_sys_const_ostype_unix const
//Requires: os_type
function caml_sys_const_ostype_unix () { return os_type == "Unix" ? 1 : 0; }
//Provides: caml_sys_const_ostype_win32 const
//Requires: os_type
function caml_sys_const_ostype_win32 () { return os_type == "Win32" ? 1 : 0; }
//Provides: caml_sys_const_ostype_cygwin const
//Requires: os_type
function caml_sys_const_ostype_cygwin () { return os_type == "Cygwin" ? 1 : 0; }

//Provides: caml_sys_const_backend_type const
//Requires: caml_string_of_jsbytes
function caml_sys_const_backend_type () {
  return [0, caml_string_of_jsbytes("js_of_ocaml")];
}

//Provides: os_type
var os_type = (globalThis.process &&
               globalThis.process.platform &&
               globalThis.process.platform == "win32") ? "Cygwin" : "Unix";


//Provides: caml_sys_get_config const
//Requires: caml_string_of_jsbytes, os_type
function caml_sys_get_config () {
  return [0, caml_string_of_jsbytes(os_type), 32, 0];
}

//Provides: caml_sys_isatty
function caml_sys_isatty(_chan) {
  return 0;
}

//Provides: caml_runtime_variant
//Requires: caml_string_of_jsbytes
function caml_runtime_variant(_unit) {
  return caml_string_of_jsbytes("");
}
//Provides: caml_runtime_parameters
//Requires: caml_string_of_jsbytes
function caml_runtime_parameters(_unit) {
  return caml_string_of_jsbytes("");
}

//Provides: caml_install_signal_handler const
function caml_install_signal_handler(){return 0}

//Provides: unix_inet_addr_of_string
function unix_inet_addr_of_string () {return 0;}



//Provides: caml_runtime_warnings
var caml_runtime_warnings = 0;

//Provides: caml_ml_enable_runtime_warnings
//Requires: caml_runtime_warnings
function caml_ml_enable_runtime_warnings (bool) {
  caml_runtime_warnings = bool;
  return 0;
}

//Provides: caml_ml_runtime_warnings_enabled
//Requires: caml_runtime_warnings
function caml_ml_runtime_warnings_enabled (_unit) {
  return caml_runtime_warnings;
}


//Provides: caml_spacetime_enabled const (const)
function caml_spacetime_enabled(_unit) {
  return 0;
}

//Provides: caml_sys_const_naked_pointers_checked const (const)
function caml_sys_const_naked_pointers_checked(_unit) {
  return 0;
}

//Provides: caml_register_channel_for_spacetime const (const)
function caml_register_channel_for_spacetime(_channel) {
  return 0;
}

//Provides: caml_spacetime_only_works_for_native_code
//Requires: caml_failwith
function caml_spacetime_only_works_for_native_code() {
  caml_failwith("Spacetime profiling only works for native code");
}

//Always
//Requires: caml_fatal_uncaught_exception
function caml_setup_uncaught_exception_handler() {
  var g = globalThis;
  if(g.process && g.process.on) {
    g.process.on('uncaughtException', function (err, origin) {
      caml_fatal_uncaught_exception(err);
      g.process.exit (2);
    })
  }
  else if(g.addEventListener){
    g.addEventListener('error', function(event){
      if(event.error){
        caml_fatal_uncaught_exception(event.error);
      }
    });
  }
}
caml_setup_uncaught_exception_handler();
