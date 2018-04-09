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

//Provides: caml_std_output
//Requires: caml_new_string, caml_ml_string_length, caml_ml_channels
function caml_std_output(chanid,s){
  var chan = caml_ml_channels[chanid];
  var str = caml_new_string(s);
  var slen = caml_ml_string_length(str);
  chan.file.write(chan.offset, str, 0, slen);
  chan.offset += slen;
  return 0;
}

//Provides: caml_sys_open
//Requires: caml_raise_sys_error, caml_global_data
//Requires: caml_create_bytes,MlFakeFile
//Requires: js_print_stderr, js_print_stdout
//Requires: caml_std_output
//Requires: resolve_fs_device
function caml_sys_open_internal(idx,output,file,flags) {
  if(caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  flags=flags?flags:{};
  var info = {};
  info.file = file;
  info.offset = flags.append?file.length():0;
  info.flags = flags;
  info.output = output;
  caml_global_data.fds[idx] = info;
  if(!caml_global_data.fd_last_idx || idx > caml_global_data.fd_last_idx)
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
  if(f.rdonly && f.wronly)
    caml_raise_sys_error(name.toString() + " : flags Open_rdonly and Open_wronly are not compatible");
  if(f.text && f.binary)
    caml_raise_sys_error(name.toString() + " : flags Open_text and Open_binary are not compatible");
  var root = resolve_fs_device(name);
  var file = root.device.open(root.rest,f);
  var idx = caml_global_data.fd_last_idx?caml_global_data.fd_last_idx:0;
  return caml_sys_open_internal (idx+1,caml_std_output,file,f);
}
caml_sys_open_internal(0,caml_std_output, new MlFakeFile(caml_create_bytes(0))); //stdin
caml_sys_open_internal(1,js_print_stdout, new MlFakeFile(caml_create_bytes(0))); //stdout
caml_sys_open_internal(2,js_print_stderr, new MlFakeFile(caml_create_bytes(0))); //stderr


// ocaml Channels

//Provides: caml_ml_set_channel_name
function caml_ml_set_channel_name() {
  return 0
}

//Provides: caml_ml_channels
var caml_ml_channels = new Array();

//Provides: caml_ml_out_channels_list
//Requires: caml_ml_channels
function caml_ml_out_channels_list () {
  var l = 0;
  for(var c = 0; c < caml_ml_channels.length; c++){
    if(caml_ml_channels[c] && caml_ml_channels[c].opened && caml_ml_channels[c].out)
      l=[0,caml_ml_channels[c].fd,l];
  }
  return l;
}


//Provides: caml_ml_open_descriptor_out
//Requires: caml_ml_channels, caml_global_data
//Requires: caml_raise_sys_error
function caml_ml_open_descriptor_out (fd) {
  var data = caml_global_data.fds[fd];
  if(data.flags.rdonly) caml_raise_sys_error("fd "+ fd + " is readonly");
  var channel = {
    file:data.file,
    offset:data.offset,
    fd:fd,
    opened:true,
    out:true,
    buffer:""
  };
  caml_ml_channels[channel.fd]=channel;
  return channel.fd;
}

//Provides: caml_ml_open_descriptor_in
//Requires: caml_global_data,caml_sys_open,caml_raise_sys_error, caml_ml_channels
function caml_ml_open_descriptor_in (fd)  {
  var data = caml_global_data.fds[fd];
  if(data.flags.wronly) caml_raise_sys_error("fd "+ fd + " is writeonly");

  var channel = {
    file:data.file,
    offset:data.offset,
    fd:fd,
    opened:true,
    out: false,
    refill:null
  };
  caml_ml_channels[channel.fd]=channel;
  return channel.fd;
}


//Provides: caml_ml_set_binary_mode
//Requires: caml_global_data, caml_ml_channels
function caml_ml_set_binary_mode(chanid,mode){
  var chan = caml_ml_channels[chanid];
  var data = caml_global_data.fds[chan.fd];
  data.flags.text = !mode
  data.flags.binary = mode
  return 0;
}

//Input from in_channel

//Provides: caml_ml_close_channel
//Requires: caml_ml_flush, caml_ml_channels
//Requires: caml_sys_close
function caml_ml_close_channel (chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_flush(chanid);
  chan.opened = false;
  chan.file.close();
  caml_sys_close(chan.fd)
  return 0;
}

//Provides: caml_ml_channel_size
//Requires: caml_ml_channels
function caml_ml_channel_size(chanid) {
  var chan = caml_ml_channels[chanid];
  return chan.file.length();
}

//Provides: caml_ml_channel_size_64
//Requires: caml_int64_of_float,caml_ml_channels
function caml_ml_channel_size_64(chanid) {
  var chan = caml_ml_channels[chanid];
  return caml_int64_of_float(chan.file.length ());
}

//Provides: caml_ml_set_channel_output
//Requires: caml_ml_channels, caml_global_data
function caml_ml_set_channel_output(chanid,f) {
  var chan = caml_ml_channels[chanid];
  caml_global_data.fds[chan.fd].output = f;
  return 0;
}

//Provides: caml_ml_set_channel_refill
//Requires: caml_ml_channels, caml_global_data
function caml_ml_set_channel_refill(chanid,f) {
  caml_ml_channels[chanid].refill = f;
  return 0;
}

//Provides: caml_ml_refill_input
//Requires: caml_ml_bytes_length
function caml_ml_refill_input (chan) {
  var str = chan.refill();
  var str_len = caml_ml_bytes_length(str);
  if (str_len == 0) chan.refill = null;
  chan.file.write(chan.file.length(), str, 0, str_len);
  return str_len;
}

//Provides: caml_ml_may_refill_input
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_may_refill_input (chanid) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill == null) return;
  if (chan.file.length() != chan.offset) return;
  caml_ml_refill_input (chan);
}

//Provides: caml_ml_input
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_input (chanid, s, i, l) {
  var chan = caml_ml_channels[chanid];
  var l2 = chan.file.length() - chan.offset;
  if (l2 == 0 && chan.refill != null) l2 = caml_ml_refill_input(chan);
  if (l2 < l) l = l2;
  chan.file.read(chan.offset, s, i, l);
  chan.offset += l;
  return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string, caml_create_bytes, caml_ml_channels
function caml_input_value (chanid) {
  var chan = caml_ml_channels[chanid];

  var buf = caml_create_bytes(8);
  chan.file.read(chan.offset,buf,0,8);

  // Header is 20 bytes
  var len = caml_marshal_data_size (buf, 0) + 20;

  var buf = caml_create_bytes(len);
  chan.file.read(chan.offset,buf,0,len);

  var offset = [0];
  var res = caml_input_value_from_string(buf, offset);
  chan.offset = chan.offset + offset[0];
  return res;
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file, caml_array_bound_error
//Requires: caml_ml_may_refill_input, caml_ml_channels
function caml_ml_input_char (chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_may_refill_input(chanid);
  if (chan.offset >= chan.file.length())
    caml_raise_end_of_file();
  var res = chan.file.read_one(chan.offset);
  chan.offset++;
  return res;
}

//Provides: caml_ml_input_int
//Requires: caml_raise_end_of_file
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_input_int (chanid) {
  var chan = caml_ml_channels[chanid];
  var file = chan.file;
  while ((chan.offset + 3) >= file.length()) {
    var l = caml_ml_refill_input(chan);
    if (l == 0) caml_raise_end_of_file();
  }
  var o = chan.offset;
  var r =(file.read_one(o  ) << 24)
      |  (file.read_one(o+1) << 16)
      |  (file.read_one(o+2) << 8)
      |  (file.read_one(o+3));
  chan.offset+=4;
  return r;
}

//Provides: caml_ml_seek_in
//Requires: caml_raise_sys_error, caml_ml_channels
function caml_ml_seek_in(chanid,pos){
  var chan = caml_ml_channels[chanid];
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  chan.offset = pos;
  return 0;
}

//Provides: caml_ml_seek_in_64
//Requires: caml_int64_to_float, caml_raise_sys_error, caml_ml_channels
function caml_ml_seek_in_64(chanid,pos){
  var chan = caml_ml_channels[chanid];
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  chan.offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_in
//Requires: caml_ml_channels
function caml_ml_pos_in(chanid) {return caml_ml_channels[chanid].offset}

//Provides: caml_ml_pos_in_64
//Requires: caml_int64_of_float, caml_ml_channels
function caml_ml_pos_in_64(chanid) {return caml_int64_of_float(caml_ml_channels[chanid].offset)}

//Provides: caml_ml_input_scan_line
//Requires: caml_array_bound_error
//Requires: caml_ml_may_refill_input, caml_ml_channels
function caml_ml_input_scan_line(chanid){
  var chan = caml_ml_channels[chanid];
  caml_ml_may_refill_input(chanid);
  var p = chan.offset;
  var len = chan.file.length();
  if(p >= len) { return 0;}
  while(true) {
    if(p >= len) return - (p - chan.offset);
    if(chan.file.read_one(p) == 10) return p - chan.offset + 1;
    p++;
  }
}

//Provides: caml_ml_flush
//Requires: caml_raise_sys_error, caml_global_data, caml_ml_channels
function caml_ml_flush (chanid) {
    var chan = caml_ml_channels[chanid];
    if(! chan.opened) caml_raise_sys_error("Cannot flush a closed channel");
    if(!chan.buffer || chan.buffer == "") return 0;
    if(chan.fd
       && caml_global_data.fds[chan.fd]
       && caml_global_data.fds[chan.fd].output) {
      var output = caml_global_data.fds[chan.fd].output;
      switch(output.length){
      case 2: output(chanid,chan.buffer);break;
      default: output(chan.buffer)
      };
    }
    chan.buffer = "";
    return 0;
}

//output to out_channel

//Provides: caml_ml_output_bytes
//Requires: caml_ml_flush,caml_ml_bytes_length
//Requires: caml_create_bytes, caml_blit_bytes, caml_raise_sys_error, caml_ml_channels, caml_jsbytes_of_string
function caml_ml_output_bytes(chanid,buffer,offset,len) {
    var chan = caml_ml_channels[chanid];
    if(! chan.opened) caml_raise_sys_error("Cannot output to a closed channel");
    var string;
    if(offset == 0 && caml_ml_bytes_length(buffer) == len)
        string = buffer;
    else {
        string = caml_create_bytes(len);
        caml_blit_bytes(buffer,offset,string,0,len);
    }
    var jsstring = caml_jsbytes_of_string(string);
    var id = jsstring.lastIndexOf("\n");
    if(id < 0)
        chan.buffer+=jsstring;
    else {
        chan.buffer+=jsstring.substr(0,id+1);
        caml_ml_flush (chanid);
        chan.buffer += jsstring.substr(id+1);
    }
    return 0;
}

//Provides: caml_ml_output
//Requires: caml_ml_output_bytes
function caml_ml_output(chanid,buffer,offset,len){
    return caml_ml_output_bytes(chanid,buffer,offset,len);
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
//Requires: caml_new_string
function caml_ml_output_char (chanid,c) {
    var s = caml_new_string(String.fromCharCode(c));
    caml_ml_output(chanid,s,0,1);
    return 0;
}

//Provides: caml_output_value
//Requires: caml_output_value_to_string, caml_ml_output,caml_ml_string_length
function caml_output_value (chanid,v,_flags) {
  var s = caml_output_value_to_string(v);
  caml_ml_output(chanid,s,0,caml_ml_string_length(s));
  return 0;
}


//Provides: caml_ml_seek_out
//Requires: caml_ml_channels
function caml_ml_seek_out(chanid,pos){
  caml_ml_channels[chanid].offset = pos;
  return 0;
}

//Provides: caml_ml_seek_out_64
//Requires: caml_int64_to_float, caml_ml_channels
function caml_ml_seek_out_64(chanid,pos){
  caml_ml_channels[chanid].offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_out
//Requires: caml_ml_channels
function caml_ml_pos_out(chanid) {return caml_ml_channels[chanid].offset}

//Provides: caml_ml_pos_out_64
//Requires: caml_int64_of_float, caml_ml_channels
function caml_ml_pos_out_64(chanid) {
  return caml_int64_of_float (caml_ml_channels[chanid].offset);
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
//Requires: caml_string_of_array
function caml_ml_output_int (chanid,i) {
  var arr = [(i>>24) & 0xFF,(i>>16) & 0xFF,(i>>8) & 0xFF,i & 0xFF ];
  var s = caml_string_of_array(arr);
  caml_ml_output(chanid,s,0,4);
  return 0
}
