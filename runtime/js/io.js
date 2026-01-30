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

import { caml_ba_to_typed_array } from './bigarray.js';
import { caml_array_bound_error, caml_failwith, caml_raise_end_of_file } from './fail.js';
import { MlFakeFd_out } from './fs_fake.js';
import { caml_sys_open_for_node, fs_node_supported } from './fs_node.js';
import { resolve_fs_device } from './fs.js';
import { caml_int64_of_float, caml_int64_to_float } from './int64.js';
import { caml_input_value_from_bytes, caml_marshal_data_size, caml_marshal_header_size, caml_output_value_to_string } from './marshal.js';
import { caml_bytes_of_string, caml_bytes_of_uint8_array, caml_create_bytes, caml_ml_bytes_length, caml_ml_string_length, caml_string_of_jsbytes, caml_sub_uint8_array_to_jsbytes, caml_uint8_array_of_bytes, caml_uint8_array_of_string } from './mlBytes.js';
import { caml_io_buffer_size, caml_raise_sys_error } from './sys.js';

//Provides: caml_sys_fds
export var caml_sys_fds = new Array(3);

//Provides: caml_sys_close
export function caml_sys_close(fd) {
  var x = caml_sys_fds[fd];
  if (x) {
    x.file.close(false);
    delete caml_sys_fds[fd];
  }
  return 0;
}

//Provides: MlChanid
export function MlChanid(id) {
  this.id = id;
}

//Provides: caml_sys_open
function caml_sys_open_internal(file, idx) {
  var chanid;
  if (idx === undefined) {
    idx = caml_sys_fds.length;
    chanid = new MlChanid(idx);
  } else if (caml_sys_fds[idx]) {
    chanid = caml_sys_fds[idx].chanid;
  } else chanid = new MlChanid(idx);
  caml_sys_fds[idx] = { file: file, chanid: chanid };
  return idx | 0;
}
export function caml_sys_open(name, flags, perms) {
  var f = {};
  while (flags) {
    switch (flags[1]) {
      case 0:
        f.rdonly = 1;
        break;
      case 1:
        f.wronly = 1;
        break;
      case 2:
        f.append = 1;
        f.writeonly = 1;
        break;
      case 3:
        f.create = 1;
        break;
      case 4:
        f.truncate = 1;
        break;
      case 5:
        f.excl = 1;
        break;
      case 6:
        f.binary = 1;
        break;
      case 7:
        f.text = 1;
        break;
      case 8:
        f.nonblock = 1;
        break;
    }
    flags = flags[2];
  }
  var root = resolve_fs_device(name);
  var file = root.device.open(root.rest, f, perms);
  return caml_sys_open_internal(file, undefined);
}
(function () {
  var is_node = fs_node_supported();
  function file(fd, flags) {
    if (is_node) {
      return caml_sys_open_for_node(fd, flags);
    } else return new MlFakeFd_out(fd, flags);
  }
  caml_sys_open_internal(
    file(0, { rdonly: 1, altname: "/dev/stdin", isCharacterDevice: true }),
    0,
  );
  caml_sys_open_internal(
    file(1, { buffered: is_node ? 1 : 2, wronly: 1, isCharacterDevice: true }),
    1,
  );
  caml_sys_open_internal(
    file(2, { buffered: is_node ? 1 : 2, wronly: 1, isCharacterDevice: true }),
    2,
  );
})();

// ocaml Channels

//Provides: caml_ml_set_channel_name
export function caml_ml_set_channel_name(chanid, name) {
  var chan = caml_ml_channel_get(chanid);
  chan.name = name;
  return 0;
}

//Provides: caml_ml_channels
class caml_ml_channels_state {
  constructor() {
    this.map = new globalThis.WeakMap();
    this.opened = new globalThis.Set();
  }

  close(chanid) {
    this.opened.delete(chanid);
  }

  get(chanid) {
    return this.map.get(chanid);
  }

  set(chanid, val) {
    if (val.opened) this.opened.add(chanid);
    return this.map.set(chanid, val);
  }

  all() {
    return this.opened.values();
  }
}

export var caml_ml_channels = new caml_ml_channels_state();

//Provides: caml_ml_channel_get
export function caml_ml_channel_get(id) {
  return caml_ml_channels.get(id);
}

//Provides: caml_ml_channel_redirect
export function caml_ml_channel_redirect(captured, into) {
  var to_restore = caml_ml_channel_get(captured);
  var new_ = caml_ml_channel_get(into);
  caml_ml_channels.set(captured, new_);
  return to_restore;
}

//Provides: caml_ml_channel_restore
export function caml_ml_channel_restore(captured, to_restore) {
  caml_ml_channels.set(captured, to_restore);
  return 0;
}

//Provides: caml_ml_out_channels_list
export function caml_ml_out_channels_list() {
  var l = 0;
  var keys = caml_ml_channels.all();
  for (var k of keys) {
    var chan = caml_ml_channel_get(k);
    if (chan.opened && chan.out) l = [0, k, l];
  }
  return l;
}

//Provides: caml_ml_open_descriptor_out
export function caml_ml_open_descriptor_out(fd) {
  var fd_desc = caml_sys_fds[fd];
  if (fd_desc === undefined)
    caml_raise_sys_error("fd " + fd + " doesn't exist");
  var file = fd_desc.file;
  var chanid = fd_desc.chanid;
  var buffered = file.flags.buffered !== undefined ? file.flags.buffered : 1;
  var channel = {
    file: file,
    offset: file.pos(),
    fd: fd,
    opened: true,
    out: true,
    buffer_curr: 0,
    buffer: new Uint8Array(caml_io_buffer_size),
    buffered: buffered,
  };
  caml_ml_channels.set(chanid, channel);
  return chanid;
}

//Provides: caml_ml_open_descriptor_in
export function caml_ml_open_descriptor_in(fd) {
  var fd_desc = caml_sys_fds[fd];
  if (fd_desc === undefined)
    caml_raise_sys_error("fd " + fd + " doesn't exist");
  var file = fd_desc.file;
  var chanid = fd_desc.chanid;
  var refill = null;
  var channel = {
    file: file,
    offset: file.pos(),
    fd: fd,
    opened: true,
    out: false,
    buffer_curr: 0,
    buffer_max: 0,
    buffer: new Uint8Array(caml_io_buffer_size),
    refill: refill,
  };
  caml_ml_channels.set(chanid, channel);
  return chanid;
}

//Provides: caml_ml_open_descriptor_in_with_flags
//Version: >= 5.1
export function caml_ml_open_descriptor_in_with_flags$v5_1_plus(fd, _flags) {
  return caml_ml_open_descriptor_in(fd);
}

//Provides: caml_ml_open_descriptor_out_with_flags
//Version: >= 5.1
export function caml_ml_open_descriptor_out_with_flags$v5_1_plus(fd, _flags) {
  return caml_ml_open_descriptor_out(fd);
}

//Provides: caml_channel_descriptor
//Alias: win_filedescr_of_channel
export function caml_channel_descriptor(chanid) {
  var chan = caml_ml_channel_get(chanid);
  return chan.fd;
}

//Provides: caml_ml_set_binary_mode
export function caml_ml_set_binary_mode(chanid, mode) {
  var chan = caml_ml_channel_get(chanid);
  chan.file.flags.text = !mode;
  chan.file.flags.binary = mode;
  return 0;
}

//Provides: caml_ml_is_binary_mode
//Version: >= 5.2
export function caml_ml_is_binary_mode$v5_2_plus(chanid) {
  var chan = caml_ml_channel_get(chanid);
  return chan.file.flags.binary;
}

//Input from in_channel

//Provides: caml_ml_close_channel
export function caml_ml_close_channel(chanid) {
  var chan = caml_ml_channel_get(chanid);
  if (chan.opened) {
    chan.opened = false;
    caml_ml_channels.close(chanid);
    caml_sys_close(chan.fd);
    chan.fd = -1;
    chan.buffer = new Uint8Array(0);
    chan.buffer_curr = 0;
    chan.buffer_max = 0;
  }
  return 0;
}

//Provides: caml_ml_channel_size
export function caml_ml_channel_size(chanid) {
  var chan = caml_ml_channel_get(chanid);
  return chan.file.length() | 0;
}

//Provides: caml_ml_channel_size_64
export function caml_ml_channel_size_64(chanid) {
  var chan = caml_ml_channel_get(chanid);
  return caml_int64_of_float(chan.file.length());
}

//Provides: caml_ml_set_channel_output
export function caml_ml_set_channel_output(chanid, f) {
  var chan = caml_ml_channel_get(chanid);
  chan.output = function (s) {
    f(s);
  };
  return 0;
}

//Provides: caml_ml_set_channel_refill
export function caml_ml_set_channel_refill(chanid, f) {
  caml_ml_channel_get(chanid).refill = f;
  return 0;
}

//Provides: caml_refill
export function caml_refill(chan) {
  if (chan.refill != null) {
    var str = chan.refill();
    var str_a = caml_uint8_array_of_string(str);
    if (str_a.length === 0) {
      chan.refill = null;
    } else {
      if (chan.buffer.length < chan.buffer_max + str_a.length) {
        var b = new Uint8Array(chan.buffer_max + str_a.length);
        b.set(chan.buffer);
        chan.buffer = b;
      }
      chan.buffer.set(str_a, chan.buffer_max);
      chan.offset += str_a.length;
      chan.buffer_max += str_a.length;
    }
  } else {
    if (chan.fd === -1) {
      caml_raise_sys_error("Bad file descriptor");
    }
    var nread = chan.file.read(
      chan.buffer,
      chan.buffer_max,
      chan.buffer.length - chan.buffer_max,
      false,
    );
    chan.offset += nread;
    chan.buffer_max += nread;
  }
}

//Provides: caml_ml_input
export function caml_ml_input(chanid, b, i, l) {
  var ba = caml_uint8_array_of_bytes(b);
  return caml_ml_input_block(chanid, ba, i, l);
}

//Provides: caml_ml_input_bigarray
//Version: >= 5.2
export function caml_ml_input_bigarray$v5_2_plus(chanid, b, i, l) {
  var ba = caml_ba_to_typed_array(b);
  return caml_ml_input_block(chanid, ba, i, l);
}

//Provides: caml_ml_input_block
export function caml_ml_input_block(chanid, ba, i, l) {
  var chan = caml_ml_channel_get(chanid);
  var n = l;
  var avail = chan.buffer_max - chan.buffer_curr;
  if (l <= avail) {
    ba.set(chan.buffer.subarray(chan.buffer_curr, chan.buffer_curr + l), i);
    chan.buffer_curr += l;
  } else if (avail > 0) {
    ba.set(chan.buffer.subarray(chan.buffer_curr, chan.buffer_curr + avail), i);
    chan.buffer_curr += avail;
    n = avail;
  } else {
    chan.buffer_curr = 0;
    chan.buffer_max = 0;
    caml_refill(chan);
    var avail = chan.buffer_max - chan.buffer_curr;
    if (n > avail) n = avail;
    ba.set(chan.buffer.subarray(chan.buffer_curr, chan.buffer_curr + n), i);
    chan.buffer_curr += n;
  }
  return n | 0;
}

//Provides: caml_input_value
export function caml_input_value(chanid) {
  var chan = caml_ml_channel_get(chanid);
  var header = new Uint8Array(caml_marshal_header_size);
  function block(buffer, offset, n) {
    var r = 0;
    while (r < n) {
      if (chan.buffer_curr >= chan.buffer_max) {
        chan.buffer_curr = 0;
        chan.buffer_max = 0;
        caml_refill(chan);
      }
      if (chan.buffer_curr >= chan.buffer_max) break;
      buffer[offset + r] = chan.buffer[chan.buffer_curr];
      chan.buffer_curr++;
      r++;
    }
    return r;
  }
  var r = block(header, 0, caml_marshal_header_size);
  if (r === 0) caml_raise_end_of_file();
  else if (r < caml_marshal_header_size)
    caml_failwith("input_value: truncated object");
  var len = caml_marshal_data_size(caml_bytes_of_uint8_array(header), 0);
  var buf = new Uint8Array(len + caml_marshal_header_size);
  buf.set(header, 0);
  var r = block(buf, caml_marshal_header_size, len);
  if (r < len) caml_failwith("input_value: truncated object " + r + "  " + len);
  var res = caml_input_value_from_bytes(caml_bytes_of_uint8_array(buf), 0);
  return res;
}

//Provides: caml_input_value_to_outside_heap
//Version: >= 5
export function caml_input_value_to_outside_heap(c) {
  return caml_input_value(c);
}

//Provides: caml_ml_input_char
export function caml_ml_input_char(chanid) {
  var chan = caml_ml_channel_get(chanid);
  if (chan.buffer_curr >= chan.buffer_max) {
    chan.buffer_curr = 0;
    chan.buffer_max = 0;
    caml_refill(chan);
  }
  if (chan.buffer_curr >= chan.buffer_max) caml_raise_end_of_file();
  var res = chan.buffer[chan.buffer_curr];
  chan.buffer_curr++;
  return res;
}

//Provides: caml_ml_input_int
export function caml_ml_input_int(chanid) {
  var res = 0;
  for (var i = 0; i < 4; i++) {
    res = ((res << 8) + caml_ml_input_char(chanid)) | 0;
  }
  return res | 0;
}

//Provides: caml_seek_in
export function caml_seek_in(chanid, pos) {
  var chan = caml_ml_channel_get(chanid);
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  if (
    pos >= chan.offset - chan.buffer_max &&
    pos <= chan.offset &&
    chan.file.flags.binary
  ) {
    chan.buffer_curr = chan.buffer_max - (chan.offset - pos);
  } else {
    chan.file.seek(pos, 0);
    chan.offset = pos;
    chan.buffer_curr = 0;
    chan.buffer_max = 0;
  }
  return 0;
}

//Provides: caml_ml_seek_in
export function caml_ml_seek_in(chanid, pos) {
  return caml_seek_in(chanid, pos);
}

//Provides: caml_ml_seek_in_64
export function caml_ml_seek_in_64(chanid, pos) {
  var pos = caml_int64_to_float(pos);
  return caml_seek_in(chanid, pos);
}

//Provides: caml_pos_in
export function caml_pos_in(chanid) {
  var chan = caml_ml_channel_get(chanid);
  return chan.offset - (chan.buffer_max - chan.buffer_curr);
}

//Provides: caml_ml_pos_in
export function caml_ml_pos_in(chanid) {
  return caml_pos_in(chanid) | 0;
}

//Provides: caml_ml_pos_in_64
export function caml_ml_pos_in_64(chanid) {
  return caml_int64_of_float(caml_pos_in(chanid));
}

//Provides: caml_ml_input_scan_line
export function caml_ml_input_scan_line(chanid) {
  var chan = caml_ml_channel_get(chanid);
  var p = chan.buffer_curr;
  do {
    if (p >= chan.buffer_max) {
      if (chan.buffer_curr > 0) {
        chan.buffer.set(chan.buffer.subarray(chan.buffer_curr), 0);
        p -= chan.buffer_curr;
        chan.buffer_max -= chan.buffer_curr;
        chan.buffer_curr = 0;
      }
      if (chan.buffer_max >= chan.buffer.length) {
        return -chan.buffer_max | 0;
      }
      var prev_max = chan.buffer_max;
      caml_refill(chan);
      if (prev_max === chan.buffer_max) {
        return -chan.buffer_max | 0;
      }
    }
  } while (chan.buffer[p++] !== 10);
  return (p - chan.buffer_curr) | 0;
}

//Provides: caml_ml_flush
export function caml_ml_flush(chanid) {
  var chan = caml_ml_channel_get(chanid);
  if (!chan.opened) caml_raise_sys_error("Cannot flush a closed channel");
  if (!chan.buffer || chan.buffer_curr === 0) return 0;
  if (chan.output) {
    chan.output(
      caml_sub_uint8_array_to_jsbytes(chan.buffer, 0, chan.buffer_curr),
    );
  } else {
    for (var pos = 0; pos < chan.buffer_curr; ) {
      pos += chan.file.write(chan.buffer, pos, chan.buffer_curr - pos, false);
    }
  }
  chan.offset += chan.buffer_curr;
  chan.buffer_curr = 0;
  return 0;
}

//output to out_channel

//Provides: caml_ml_output_ta
export function caml_ml_output_ta(chanid, buffer, offset, len) {
  var chan = caml_ml_channel_get(chanid);
  if (!chan.opened) caml_raise_sys_error("Cannot output to a closed channel");
  buffer = buffer.subarray(offset, offset + len);
  if (chan.buffer_curr + buffer.length > chan.buffer.length) {
    var b = new Uint8Array(chan.buffer_curr + buffer.length);
    b.set(chan.buffer);
    chan.buffer = b;
  }
  switch (chan.buffered) {
    case 0: // Unbuffered
      chan.buffer.set(buffer, chan.buffer_curr);
      chan.buffer_curr += buffer.length;
      caml_ml_flush(chanid);
      break;
    case 1: // Buffered (the default)
      chan.buffer.set(buffer, chan.buffer_curr);
      chan.buffer_curr += buffer.length;
      if (chan.buffer_curr >= chan.buffer.length) caml_ml_flush(chanid);
      break;
    case 2: // Buffered (only for stdout and stderr)
      var id = buffer.lastIndexOf(10);
      if (id < 0) {
        chan.buffer.set(buffer, chan.buffer_curr);
        chan.buffer_curr += buffer.length;
        if (chan.buffer_curr >= chan.buffer.length) caml_ml_flush(chanid);
      } else {
        chan.buffer.set(buffer.subarray(0, id + 1), chan.buffer_curr);
        chan.buffer_curr += id + 1;
        caml_ml_flush(chanid);
        chan.buffer.set(buffer.subarray(id + 1), chan.buffer_curr);
        chan.buffer_curr += buffer.length - id - 1;
      }
      break;
  }
  return 0;
}

//Provides: caml_ml_output_bytes
export function caml_ml_output_bytes(chanid, buffer, offset, len) {
  var buffer = caml_uint8_array_of_bytes(buffer);
  return caml_ml_output_ta(chanid, buffer, offset, len);
}

//Provides: caml_ml_output_bigarray
//Version: >= 5.2
export function caml_ml_output_bigarray$v5_2_plus(chanid, buffer, offset, len) {
  var buffer = caml_ba_to_typed_array(buffer);
  return caml_ml_output_ta(chanid, buffer, offset, len);
}

//Provides: caml_ml_output
export function caml_ml_output(chanid, buffer, offset, len) {
  return caml_ml_output_bytes(
    chanid,
    caml_bytes_of_string(buffer),
    offset,
    len,
  );
}

//Provides: caml_ml_output_char
export function caml_ml_output_char(chanid, c) {
  var s = caml_string_of_jsbytes(String.fromCharCode(c));
  caml_ml_output(chanid, s, 0, 1);
  return 0;
}

//Provides: caml_output_value
export function caml_output_value(chanid, v, flags) {
  var s = caml_output_value_to_string(v, flags);
  caml_ml_output(chanid, s, 0, caml_ml_string_length(s));
  return 0;
}

//Provides: caml_seek_out
export function caml_seek_out(chanid, pos) {
  caml_ml_flush(chanid);
  var chan = caml_ml_channel_get(chanid);
  chan.file.seek(pos, 0);
  chan.offset = pos;
  return 0;
}

//Provides: caml_ml_seek_out
export function caml_ml_seek_out(chanid, pos) {
  return caml_seek_out(chanid, pos);
}
//Provides: caml_ml_seek_out_64
export function caml_ml_seek_out_64(chanid, pos) {
  var pos = caml_int64_to_float(pos);
  return caml_seek_out(chanid, pos);
}

//Provides: caml_pos_out
export function caml_pos_out(chanid) {
  var chan = caml_ml_channel_get(chanid);
  return chan.offset + chan.buffer_curr;
}

//Provides: caml_ml_pos_out
export function caml_ml_pos_out(chanid) {
  return caml_pos_out(chanid) | 0;
}

//Provides: caml_ml_pos_out_64
export function caml_ml_pos_out_64(chanid) {
  return caml_int64_of_float(caml_pos_out(chanid));
}

//Provides: caml_ml_output_int
export function caml_ml_output_int(chanid, i) {
  var arr = [(i >> 24) & 0xff, (i >> 16) & 0xff, (i >> 8) & 0xff, i & 0xff];
  caml_ml_output_ta(chanid, new Uint8Array(arr), 0, 4);
  return 0;
}

//Provides: caml_ml_is_buffered
export function caml_ml_is_buffered(chanid) {
  return caml_ml_channel_get(chanid).buffered ? 1 : 0;
}

//Provides: caml_ml_set_buffered
export function caml_ml_set_buffered(chanid, v) {
  caml_ml_channel_get(chanid).buffered = v;
  if (!v) caml_ml_flush(chanid);
  return 0;
}
