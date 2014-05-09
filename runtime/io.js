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
  return;
}

//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data,caml_sys_file_exists
//Requires: caml_fs_register,caml_make_path,caml_fs_content
//Requires: caml_raise_no_such_file,caml_sys_is_directory
function caml_sys_open_internal(idx,v,flags) {
  if(caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  flags=flags?flags:{};
  var data = {};
  data.array = v;
  data.offset = flags.append?data.array.length:0;
  data.flags = flags;
  caml_global_data.fds[idx] = data;
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
    return caml_sys_open_internal (idx+1,file.content(),f);
  } else if (f.create) {
    var idx = caml_global_data.fd_last_idx?caml_global_data.fd_last_idx:0;
    caml_fs_register(name,[]);
    var file = caml_fs_content(path);
    return caml_sys_open_internal (idx+1,file.content(),f);
  }
  else caml_raise_no_such_file (name);
}
caml_sys_open_internal(0,[]); //stdin
caml_sys_open_internal(1,[]); //stdout
caml_sys_open_internal(2,[]); //stderr


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
//Requires: caml_raise_sys_error,MlString
function caml_std_output(chan,s){
  var str = new MlString(s),slen = str.getLen();
  for(var i = 0;i<slen;i++){
    chan.data.array[chan.data.offset + i] = str.get(i);
  }
  chan.data.offset += slen;
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
    data: data,
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
    data: data,
    fd:fd,
    opened:true
  };
}


//Provides: caml_ml_set_binary_mode
function caml_ml_set_binary_mode(chan,mode){
  chan.data.flags.text = !mode
  chan.data.flags.binary = mode
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
  return chan.data.array.length;
}

//Provides: caml_ml_channel_size_64
//Requires: caml_ml_channel_size,caml_int64_of_float
function caml_ml_channel_size_64(chan) {
  return caml_int64_of_float(chan.data.array.length);
}

//Provides: caml_ml_set_channel_output
function caml_ml_set_channel_output(chan,f) {
  chan.output = f;
  return;
}

//Provides: caml_ml_input
//Requires: caml_blit_string, MlStringFromArray
function caml_ml_input (chan, s, i, l) {
  var l2 = chan.data.array.length - chan.data.offset;
  if (l2 < l) l = l2;
  caml_blit_string(new MlStringFromArray(chan.data.array), chan.data.offset, s, i, l);
  chan.data.offset += l;
  return l;
}

//Provides: caml_fs_file_content
//Requires: MlStringFromArray, caml_fs_content, caml_make_path, MlFile
//Requires: caml_raise_not_found
function caml_fs_file_content(name) {
  var path = caml_make_path(name);
  var f = caml_fs_content(path);
  if(f instanceof MlFile)
    return new MlStringFromArray(f.content());
  caml_raise_not_found();
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string, MlStringFromArray
function caml_input_value (chan) {
  var str = new MlStringFromArray(chan.data.array);
  var _len = caml_marshal_data_size (str, chan.data.offset);
  var res = caml_input_value_from_string(str, chan.data.offset);
  chan.data.offset = str.offset;
  return res;
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file, caml_array_bound_error
function caml_ml_input_char (chan) {
  if (chan.data.offset >= chan.data.array.length)
    caml_raise_end_of_file();
  if(chan.data.offset < 0 || chan.data.offset > chan.data.array.length) caml_array_bound_error();
  var c = chan.data.array[chan.data.offset];
  chan.data.offset++;
  return c;
}

//Provides: caml_ml_input_int
//Requires: caml_raise_end_of_file
function caml_ml_input_int (chan) {
  if ((chan.data.offset + 3) >= chan.data.array.length)
    caml_raise_end_of_file();
  var a = chan.data.array, o = chan.data.offset;
  var r = (a[o] << 24) | (a[o+1] << 16) | (a[o+2] << 8) | (a[o+3]);
  chan.data.offset+=4;
  return r;
}

//Provides: caml_ml_seek_in
function caml_ml_seek_in(chan,pos){
  chan.data.offset = pos;
}

//Provides: caml_ml_seek_in_64
//Requires: caml_int64_to_float
function caml_ml_seek_in_64(chan,pos){
  chan.data.offset = caml_int64_to_float(pos);
}

//Provides: caml_ml_pos_in
function caml_ml_pos_in(chan) {return chan.data.offset}

//Provides: caml_ml_pos_in_64
//Requires: caml_int64_of_float
function caml_ml_pos_in_64(chan) {return caml_int64_of_float(chan.data.offset)}

//Provides: caml_ml_input_scan_line
//Requires: caml_array_bound_error
function caml_ml_input_scan_line(chan){
    var p = chan.data.offset;
    var len = chan.data.array.length;
    if(p >= len) { return 0;}
    while(true) {
        if(p >= len) return - (p - chan.data.offset);
        if(p < 0 || p > chan.data.array.length) caml_array_bound_error();
        if(chan.data.array[p] == 10) return p - chan.data.offset + 1;
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
}

//output to out_channel

//Provides: caml_ml_output
//Requires: caml_ml_flush
//Requires: MlString, caml_create_string, caml_blit_string, caml_raise_sys_error
function caml_ml_output (oc,buffer,offset,len) {
    if(! oc.opened) caml_raise_sys_error("Cannot output to a closed channel");
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

//Provides: caml_output_value
//Requires: caml_output_value_to_string, caml_ml_output
function caml_output_value (chan,v,_flags) {
  var s = caml_output_value_to_string(v);
  caml_ml_output(chan,s,0,s.getLen());
}


//Provides: caml_ml_seek_out
function caml_ml_seek_out(chan,pos){
  chan.data.offset = pos;
}

//Provides: caml_ml_seek_out_64
//Requires: caml_int64_to_float
function caml_ml_seek_out_64(chan,pos){
  chan.data.offset = caml_int64_to_float(pos);
}

//Provides: caml_ml_pos_out
function caml_ml_pos_out(chan) {return chan.data.offset}

//Provides: caml_ml_pos_out_64
//Requires: caml_int64_of_float
function caml_ml_pos_out_64(chan) {
  return caml_int64_of_float (chan.data.offset);
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
//Requires: MlStringFromArray
function caml_ml_output_int (oc,i) {
  var arr = [(i>>24) & 0xFF,(i>>16) & 0xFF,(i>>8) & 0xFF,i & 0xFF ];
  var s = new MlStringFromArray(arr);
  caml_ml_output(oc,s,0,4);
}
