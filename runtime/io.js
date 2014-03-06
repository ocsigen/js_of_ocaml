// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
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

///////////// Io && fake FileSystem

//Provides: caml_register_file
//Requires: caml_global_data, MlString
function caml_register_file(name,content) {
  if(!caml_global_data.files)
    caml_global_data.files = {};
  caml_global_data.files[(name instanceof MlString)?name.toString():name] = content;
}

//Provides: caml_sys_file_exists
//Requires: caml_global_data
function caml_sys_file_exists (name) {
  return (caml_global_data.files && caml_global_data.files[name.toString()])?1:0;
}

//Provides: caml_sys_remove
//Requires: caml_global_data
function caml_sys_remove(name){
  if(caml_global_data.files)
    delete caml_global_data.files[name.toString()];
}

//Provides: caml_sys_rename
//Requires: caml_global_data,caml_sys_file_exists,caml_register_file,caml_sys_remove
function caml_sys_rename(o,n){
  if(caml_sys_file_exists(o)){
    caml_register_file(n, caml_global_data.files[o.toString()]);
    caml_sys_remove(o);
  }
  return;
}

//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data
function caml_sys_open_internal(idx,v) {
  if(caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  var s = (v instanceof MlString)?v:(new MlString(v));
  s.offset = 0;
  caml_global_data.fds[idx] = s;
  caml_global_data.fd_last_idx = idx;
  return idx;
}
function caml_sys_open (name, flags, perms) {
    name = name.toString();
  if (caml_global_data.files && caml_global_data.files[name]) {
    var idx = caml_global_data.fd_last_idx?caml_global_data.fd_last_idx:0;
    return caml_sys_open_internal (idx+1,caml_global_data.files[name]);
  }
  else caml_raise_sys_error (name + ": no such file or directory");
}
caml_sys_open_internal(0,""); //stdin
caml_sys_open_internal(1,""); //stdout
caml_sys_open_internal(2,""); //stderr


// ocaml Channels

//Provides: caml_ml_out_channels
var caml_ml_out_channels = new Array();

//Provides: caml_ml_out_channels_list
//Requires: caml_ml_out_channels
function caml_ml_out_channels_list () {
  var l = 0;
  for(var c in caml_ml_out_channels){
    if(caml_ml_out_channels[c].opened)
      l=[0,caml_ml_out_channels[c],l];
  }
  return l;
}


//Provides: caml_ml_open_descriptor_out
//Requires: js_print_stderr, js_print_stdout, caml_ml_out_channels, caml_global_data,caml_sys_open
function caml_ml_open_descriptor_out (fd) {
  var output = function () { return; };
  switch(fd){
    case 1: output=js_print_stdout;break;
    case 2: output=js_print_stderr;break;
  }
  var channel = {
    data: caml_global_data.fds[fd],
    fd:fd,
    opened:true,

    buffer:"",
    output : output
  };
  caml_ml_out_channels[channel.fd]=channel;
  return channel;
}

//Provides: caml_ml_open_descriptor_in
//Requires: caml_global_data,caml_sys_open
function caml_ml_open_descriptor_in (fd)  {
  return {
    data: caml_global_data.fds[fd],
    fd:fd,
    opened:true
  };
}

//Input from in_channel

//Provides: caml_ml_close_channel
//Requires: caml_ml_flush, caml_ml_out_channels
function caml_ml_close_channel (channel) {
    caml_ml_flush(channel);
    channel.opened = false;
    delete caml_ml_out_channels[channel.fd];
    return 0;
}

//Provides: caml_ml_channel_size
function caml_ml_channel_size(chan) {
  return chan.data.getLen();
}

//Provides: caml_ml_channel_size_64
//Requires: caml_ml_channel_size,caml_int64_of_float
function caml_ml_channel_size_64(chan) {
  return caml_int64_of_float(chan.data.getLen());
}

//Provides: caml_ml_set_channel_output
function caml_ml_set_channel_output(chan,f) {
  chan.output = f;
  return;
}

//Provides: caml_ml_input
//Requires: caml_blit_string
function caml_ml_input (chan, s, i, l) {
  var l2 = chan.data.getLen() - chan.data.offset;
  if (l2 < l) l = l2;
  caml_blit_string(chan.data, chan.data.offset, s, i, l);
  chan.data.offset += l;
  return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string
function caml_input_value (chan) {
  caml_marshal_data_size (chan.data, chan.data.offset);
  return caml_input_value_from_string(chan.data, chan.data.offset);
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file
function caml_ml_input_char (chan) {
  if (chan.data.offset >= chan.data.getLen())
    caml_raise_end_of_file();
  var c = chan.data.safeGet(chan.data.offset);
  chan.data.offset++;
  return c;
}

//Provides: caml_ml_input_scan_line
function caml_ml_input_scan_line(chan){
    var p = chan.data.offset;
    var len = chan.data.getLen();
    if(p >= len) { return 0;}
    while(true) {
        if(p >= len) return - (p - chan.data.offset);
        if(chan.data.safeGet(p) == 10) return p - chan.data.offset + 1;
        p++;
    }
}

//Provides: caml_ml_flush
//Requires: caml_raise_sys_error
function caml_ml_flush (oc) {
    if(! oc.opened) caml_raise_sys_error("");
    if(oc.buffer == "") return 0;
    if(oc.output) {oc.output(oc.buffer)};
    oc.buffer = "";
}

//output to out_channel

//Provides: caml_ml_output
//Requires: caml_ml_flush
//Requires: MlString, caml_create_string, caml_blit_string, caml_raise_sys_error
function caml_ml_output (oc,buffer,offset,len) {
    if(! oc.opened) caml_raise_sys_error("");
    var string;
    if(offset == 0 && buffer.getLen() == len)
        string = buffer;
    else {
        string = caml_create_string(len);
        caml_blit_string(buffer,offset,string,0,len);
    }
    var jsstring = string.toString();
    var id = jsstring.lastIndexOf("\n");
    if(id < 0)
        oc.buffer+=jsstring;
    else {
        oc.buffer+=jsstring.substr(0,id+1);
        caml_ml_flush (oc);
        oc.buffer += jsstring.substr(id+1);
    }
}
//Provides: caml_ml_output_char
//Requires: caml_ml_output
//Requires: caml_new_string
function caml_ml_output_char (oc,c) {
    var s = caml_new_string(String.fromCharCode(c));
    caml_ml_output(oc,s,0,1);
}
