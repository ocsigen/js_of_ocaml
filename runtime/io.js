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

///////////// Io

//Provides: caml_sys_close
//Requires: caml_global_data
function caml_sys_close(fd) {
  delete caml_global_data.fds[fd];
  return 0;
}

//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data,caml_sys_file_exists
//Requires: caml_fs_register,caml_make_path,caml_fs_content
//Requires: caml_raise_no_such_file,caml_sys_is_directory
//Requires: caml_create_string,MlFile
function caml_sys_open_internal(idx,file,flags) {
  if(caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  flags=flags?flags:{};
  var info = {};
  info.file = file;
  info.offset = flags.append?file.data.getLen():0;
  info.flags = flags;
  caml_global_data.fds[idx] = info;
  caml_global_data.fd_last_idx = idx;
  return idx;
}
function caml_sys_open (name, flags, _perms) {
  var f = {};
  while(flags){
    switch(flags[1]){
    case 0: f.rdonly = 1;break;
    case 1: f.wronly = 1;break;
    case 2: f.append = 1;break;
    case 3: f.create = 1;break;
    case 4: f.truncate = 1;break;
    case 5: f.excl = 1; break;
    case 6: f.binary = 1;break;
    case 7: f.text = 1;break;
    case 8: f.nonblock = 1;break;
    }
    flags=flags[2];
  }
  var name2 = name.toString();
  var path = caml_make_path(name);
  if(f.rdonly && f.wronly)
    caml_raise_sys_error(name2 + " : flags Open_rdonly and Open_wronly are not compatible");
  if(f.text && f.binary)
    caml_raise_sys_error(name2 + " : flags Open_text and Open_binary are not compatible");
  if (caml_sys_file_exists(name)) {
    if (caml_sys_is_directory(name)) caml_raise_sys_error(name2 + " : is a directory");
    if (f.create && f.excl) caml_raise_sys_error(name2 + " : file already exists");
    var idx = caml_global_data.fd_last_idx?caml_global_data.fd_last_idx:0;
    var file = caml_fs_content(path);
    if(f.truncate) file.truncate();
    return caml_sys_open_internal (idx+1,file,f);
  } else if (f.create) {
    var idx = caml_global_data.fd_last_idx?caml_global_data.fd_last_idx:0;
    caml_fs_register(name,caml_create_string(0));
    var file = caml_fs_content(path);
    return caml_sys_open_internal (idx+1,file,f);
  }
  else caml_raise_no_such_file (name);
}
caml_sys_open_internal(0,new MlFile(caml_create_string(0))); //stdin
caml_sys_open_internal(1,new MlFile(caml_create_string(0))); //stdout
caml_sys_open_internal(2,new MlFile(caml_create_string(0))); //stderr


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
//Requires: caml_raise_sys_error,caml_new_string
//Requires: caml_create_string, caml_blit_string
function caml_std_output(chan,s){
  var str = caml_new_string(s);
  var slen = str.getLen();
  var clen = chan.file.data.getLen();
  var offset = chan.offset;
  if(offset + slen >= clen) {
    var new_str = caml_create_string (offset + slen);
    caml_blit_string(chan.file.data, 0, new_str, 0, clen);
    caml_blit_string(str, 0, new_str, offset, slen);
    chan.file.data = new_str;
  }
  chan.offset += slen;
  return 0;
}

function caml_ml_open_descriptor_out (fd) {
  var output;
  switch(fd){
    case 1: output=js_print_stdout;break;
    case 2: output=js_print_stderr;break;
    default: output=caml_std_output;
  }
  var data = caml_global_data.fds[fd];
  if(data.flags.rdonly) caml_raise_sys_error("fd "+ fd + " is readonly");
  var channel = {
    file:data.file,
    offset:data.offset,
    fd:fd,
    opened:true,

    buffer:"",
    output : output
  };
  caml_ml_out_channels[channel.fd]=channel;
  return channel;
}

//Provides: caml_ml_open_descriptor_in
//Requires: caml_global_data,caml_sys_open,caml_raise_sys_error
function caml_ml_open_descriptor_in (fd)  {
  var data = caml_global_data.fds[fd];
  if(data.flags.wronly) caml_raise_sys_error("fd "+ fd + " is writeonly");

  return {
    file:data.file,
    offset:data.offset,
    fd:fd,
    opened:true
  };
}


//Provides: caml_ml_set_binary_mode
//Requires: caml_global_data
function caml_ml_set_binary_mode(chan,mode){
  var data = caml_global_data.fds[chan.fd];
  data.flags.text = !mode
  data.flags.binary = mode
  return 0;
}

//Input from in_channel

//Provides: caml_ml_close_channel
//Requires: caml_ml_flush, caml_ml_out_channels
//Requires: caml_sys_close
function caml_ml_close_channel (channel) {
    caml_ml_flush(channel);
    channel.opened = false;
    delete caml_ml_out_channels[channel.fd];
    caml_sys_close(channel.fd)
    return 0;
}

//Provides: caml_ml_channel_size
function caml_ml_channel_size(chan) {
  return chan.file.data.getLen();
}

//Provides: caml_ml_channel_size_64
//Requires: caml_ml_channel_size,caml_int64_of_float
function caml_ml_channel_size_64(chan) {
  return caml_int64_of_float(chan.file.data.getLen());
}

//Provides: caml_ml_set_channel_output
function caml_ml_set_channel_output(chan,f) {
  chan.output = f;
  return 0;
}

//Provides: caml_ml_input
//Requires: caml_blit_string, caml_string_of_array
function caml_ml_input (chan, s, i, l) {
  var l2 = chan.file.data.getLen() - chan.offset;
  if (l2 < l) l = l2;
  caml_blit_string(chan.file.data, chan.offset, s, i, l);
  chan.offset += l;
  return l;
}

//Provides: caml_fs_file_content
//Requires: caml_string_of_array, caml_fs_content, caml_make_path, MlFile
//Requires: caml_raise_not_found
function caml_fs_file_content(name) {
  var path = caml_make_path(name);
  var f = caml_fs_content(path);
  if(f instanceof MlFile)
    return f.data;
  caml_raise_not_found();
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string, caml_string_of_array
function caml_input_value (chan) {
  var str = chan.file.data;
  var offset = chan.offset;
  var _len = caml_marshal_data_size (str, offset);
  var res = caml_input_value_from_string(str, offset);
  chan.offset = str.offset;
  return res;
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file, caml_array_bound_error
function caml_ml_input_char (chan) {
  if (chan.offset >= chan.file.data.getLen())
    caml_raise_end_of_file();
  var c = chan.file.data.safeGet(chan.offset);
  chan.offset++;
  return c;
}

//Provides: caml_ml_input_int
//Requires: caml_raise_end_of_file
function caml_ml_input_int (chan) {
  var s = chan.file.data, o = chan.offset;
  if ((o + 3) >= s.getLen())
    caml_raise_end_of_file();
  var r = (s.get(o) << 24) | (s.get(o+1) << 16) | (s.get(o+2) << 8) | (s.get(o+3));
  chan.offset+=4;
  return r;
}

//Provides: caml_ml_seek_in
function caml_ml_seek_in(chan,pos){
  chan.offset = pos;
  return 0;
}

//Provides: caml_ml_seek_in_64
//Requires: caml_int64_to_float
function caml_ml_seek_in_64(chan,pos){
  chan.offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_in
function caml_ml_pos_in(chan) {return chan.offset}

//Provides: caml_ml_pos_in_64
//Requires: caml_int64_of_float
function caml_ml_pos_in_64(chan) {return caml_int64_of_float(chan.offset)}

//Provides: caml_ml_input_scan_line
//Requires: caml_array_bound_error
function caml_ml_input_scan_line(chan){
  var p = chan.offset;
  var s = chan.file.data;
  var len = s.getLen();
  if(p >= len) { return 0;}
  while(true) {
    if(p >= len) return - (p - chan.offset);
    if(s.safeGet(p) == 10) return p - chan.offset + 1;
    p++;
  }
}

//Provides: caml_ml_flush
//Requires: caml_raise_sys_error
function caml_ml_flush (oc) {
    if(! oc.opened) caml_raise_sys_error("Cannot flush a closed channel");
    if(oc.buffer == "") return 0;
    if(oc.output) {
      switch(oc.output.length){
      case 2: oc.output(oc,oc.buffer);break;
      default: oc.output(oc.buffer)
      };
    }
    oc.buffer = "";
    return 0;
}

//output to out_channel

//Provides: caml_ml_output
//Requires: caml_ml_flush,caml_ml_string_length
//Requires: caml_create_string, caml_blit_string, caml_raise_sys_error
function caml_ml_output (oc,buffer,offset,len) {
    if(! oc.opened) caml_raise_sys_error("Cannot output to a closed channel");
    var string;
    if(offset == 0 && caml_ml_string_length(buffer) == len)
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
    return 0;
}
//Provides: caml_ml_output_char
//Requires: caml_ml_output
//Requires: caml_new_string
function caml_ml_output_char (oc,c) {
    var s = caml_new_string(String.fromCharCode(c));
    caml_ml_output(oc,s,0,1);
    return 0;
}

//Provides: caml_output_value
//Requires: caml_output_value_to_string, caml_ml_output,caml_ml_string_length
function caml_output_value (chan,v,_flags) {
  var s = caml_output_value_to_string(v);
  caml_ml_output(chan,s,0,caml_ml_string_length(s));
  return 0;
}


//Provides: caml_ml_seek_out
function caml_ml_seek_out(chan,pos){
  chan.offset = pos;
  return 0;
}

//Provides: caml_ml_seek_out_64
//Requires: caml_int64_to_float
function caml_ml_seek_out_64(chan,pos){
  chan.offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_out
function caml_ml_pos_out(chan) {return chan.offset}

//Provides: caml_ml_pos_out_64
//Requires: caml_int64_of_float
function caml_ml_pos_out_64(chan) {
  return caml_int64_of_float (chan.offset);
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
//Requires: caml_string_of_array
function caml_ml_output_int (oc,i) {
  var arr = [(i>>24) & 0xFF,(i>>16) & 0xFF,(i>>8) & 0xFF,i & 0xFF ];
  var s = caml_string_of_array(arr);
  caml_ml_output(oc,s,0,4);
  return 0
}
