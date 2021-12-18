// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
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

//Provides: caml_marshal_constants
var caml_marshal_constants = {
  PREFIX_SMALL_BLOCK:         0x80,
  PREFIX_SMALL_INT:           0x40,
  PREFIX_SMALL_STRING:        0x20,
  CODE_INT8:                  0x00,
  CODE_INT16:                 0x01,
  CODE_INT32:                 0x02,
  CODE_INT64:                 0x03,
  CODE_SHARED8:               0x04,
  CODE_SHARED16:              0x05,
  CODE_SHARED32:              0x06,
  CODE_BLOCK32:               0x08,
  CODE_BLOCK64:               0x13,
  CODE_STRING8:               0x09,
  CODE_STRING32:              0x0A,
  CODE_DOUBLE_BIG:            0x0B,
  CODE_DOUBLE_LITTLE:         0x0C,
  CODE_DOUBLE_ARRAY8_BIG:     0x0D,
  CODE_DOUBLE_ARRAY8_LITTLE:  0x0E,
  CODE_DOUBLE_ARRAY32_BIG:    0x0F,
  CODE_DOUBLE_ARRAY32_LITTLE: 0x07,
  CODE_CODEPOINTER:           0x10,
  CODE_INFIXPOINTER:          0x11,
  CODE_CUSTOM:                0x12,
  CODE_CUSTOM_LEN:            0x18,
  CODE_CUSTOM_FIXED:          0x19
}


//Provides: MlStringReader
//Requires: caml_string_of_jsbytes, caml_jsbytes_of_string
function MlStringReader (s, i) { this.s = caml_jsbytes_of_string(s); this.i = i; }
MlStringReader.prototype = {
  read8u:function () { return this.s.charCodeAt(this.i++); },
  read8s:function () { return this.s.charCodeAt(this.i++) << 24 >> 24; },
  read16u:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (s.charCodeAt(i) << 8) | s.charCodeAt(i + 1)
  },
  read16s:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (s.charCodeAt(i) << 24 >> 16) | s.charCodeAt(i + 1);
  },
  read32u:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return ((s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
            (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3)) >>> 0;
  },
  read32s:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return (s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
      (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3);
  },
  readstr:function (len) {
    var i = this.i;
    this.i = i + len;
    return caml_string_of_jsbytes(this.s.substring(i, i + len));
  }
}

//Provides: BigStringReader
//Requires: caml_string_of_array, caml_ba_get_1
function BigStringReader (bs, i) { this.s = bs; this.i = i; }
BigStringReader.prototype = {
  read8u:function () { return caml_ba_get_1(this.s,this.i++); },
  read8s:function () { return caml_ba_get_1(this.s,this.i++) << 24 >> 24; },
  read16u:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (caml_ba_get_1(s,i) << 8) | caml_ba_get_1(s,i + 1)
  },
  read16s:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (caml_ba_get_1(s,i) << 24 >> 16) | caml_ba_get_1(s,i + 1);
  },
  read32u:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return ((caml_ba_get_1(s,i)   << 24) | (caml_ba_get_1(s,i+1) << 16) |
            (caml_ba_get_1(s,i+2) << 8)  | caml_ba_get_1(s,i+3)         ) >>> 0;
  },
  read32s:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return (caml_ba_get_1(s,i)   << 24) | (caml_ba_get_1(s,i+1) << 16) |
      (caml_ba_get_1(s,i+2) << 8)  | caml_ba_get_1(s,i+3);
  },
  readstr:function (len) {
    var i = this.i;
    var arr = new Array(len)
    for(var j = 0; j < len; j++){
      arr[j] = caml_ba_get_1(this.s, i+j);
    }
    this.i = i + len;
    return caml_string_of_array(arr);
  }
}



//Provides: caml_float_of_bytes
//Requires: caml_int64_float_of_bits, caml_int64_of_bytes
function caml_float_of_bytes (a) {
  return caml_int64_float_of_bits (caml_int64_of_bytes (a));
}

//Provides: caml_input_value_from_string mutable
//Requires: MlStringReader, caml_input_value_from_reader
function caml_input_value_from_string(s,ofs) {
  var reader = new MlStringReader (s, typeof ofs=="number"?ofs:ofs[0]);
  return caml_input_value_from_reader(reader, ofs)
}

//Provides: caml_input_value_from_bytes mutable
//Requires: MlStringReader, caml_input_value_from_reader, caml_string_of_bytes
function caml_input_value_from_bytes(s,ofs) {
  var reader = new MlStringReader (caml_string_of_bytes(s), typeof ofs=="number"?ofs:ofs[0]);
  return caml_input_value_from_reader(reader, ofs)
}

//Provides: caml_int64_unmarshal
//Requires: caml_int64_of_bytes
function caml_int64_unmarshal(reader, size){
  var t = new Array(8);;
  for (var j = 0;j < 8;j++) t[j] = reader.read8u();
  size[0] = 8;
  return caml_int64_of_bytes (t);
}

//Provides: caml_int64_marshal
//Requires: caml_int64_to_bytes
function caml_int64_marshal(writer, v, sizes) {
  var b = caml_int64_to_bytes (v);
  for (var i = 0; i < 8; i++) writer.write (8, b[i]);
  sizes[0] = 8; sizes[1] = 8;
}

//Provides: caml_int32_unmarshal
function caml_int32_unmarshal(reader, size){
  size[0] = 4;
  return reader.read32s ();
}

//Provides: caml_nativeint_unmarshal
//Requires: caml_failwith
function caml_nativeint_unmarshal(reader, size){
  switch (reader.read8u ()) {
  case 1:
    size[0] = 4;
    return reader.read32s ();
  case 2:
    caml_failwith("input_value: native integer value too large");
  default: caml_failwith("input_value: ill-formed native integer");
  }
}

//Provides: caml_custom_ops
//Requires: caml_int64_unmarshal, caml_int64_marshal, caml_int64_compare, caml_int64_hash
//Requires: caml_int32_unmarshal, caml_nativeint_unmarshal
//Requires: caml_ba_serialize, caml_ba_deserialize, caml_ba_compare, caml_ba_hash
var caml_custom_ops =
    {"_j": {
      deserialize : caml_int64_unmarshal,
      serialize  : caml_int64_marshal,
      fixed_length : 8,
      compare : caml_int64_compare,
      hash : caml_int64_hash
    },
     "_i": {
       deserialize : caml_int32_unmarshal,
       fixed_length : 4,
     },
     "_n": {
       deserialize : caml_nativeint_unmarshal,
       fixed_length : 4,
     },
     "_bigarray":{
       deserialize : (function (reader, sz) {return caml_ba_deserialize (reader,sz,"_bigarray")}),
       serialize : caml_ba_serialize,
       compare : caml_ba_compare,
       hash: caml_ba_hash,
     },
     "_bigarr02":{
       deserialize : (function (reader, sz) {return caml_ba_deserialize (reader,sz,"_bigarr02")}),
       serialize : caml_ba_serialize,
       compare : caml_ba_compare,
       hash: caml_ba_hash,
     }
    }

//Provides: caml_input_value_from_reader mutable
//Requires: caml_failwith
//Requires: caml_float_of_bytes, caml_custom_ops

function caml_input_value_from_reader(reader, ofs) {
  var _magic = reader.read32u ()
  var _block_len = reader.read32u ();
  var num_objects = reader.read32u ();
  var _size_32 = reader.read32u ();
  var _size_64 = reader.read32u ();
  var stack = [];
  var intern_obj_table = (num_objects > 0)?[]:null;
  var obj_counter = 0;
  function intern_rec () {
    var code = reader.read8u ();
    if (code >= 0x40 /*cst.PREFIX_SMALL_INT*/) {
      if (code >= 0x80 /*cst.PREFIX_SMALL_BLOCK*/) {
        var tag = code & 0xF;
        var size = (code >> 4) & 0x7;
        var v = [tag];
        if (size == 0) return v;
        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
        stack.push(v, size);
        return v;
      } else
        return (code & 0x3F);
    } else {
      if (code >= 0x20/*cst.PREFIX_SMALL_STRING */) {
        var len = code & 0x1F;
        var v = reader.readstr (len);
        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
        return v;
      } else {
        switch(code) {
        case 0x00: //cst.CODE_INT8:
          return reader.read8s ();
        case 0x01: //cst.CODE_INT16:
          return reader.read16s ();
        case 0x02: //cst.CODE_INT32:
          return reader.read32s ();
        case 0x03: //cst.CODE_INT64:
          caml_failwith("input_value: integer too large");
          break;
        case 0x04: //cst.CODE_SHARED8:
          var offset = reader.read8u ();
          return intern_obj_table[obj_counter - offset];
        case 0x05: //cst.CODE_SHARED16:
          var offset = reader.read16u ();
          return intern_obj_table[obj_counter - offset];
        case 0x06: //cst.CODE_SHARED32:
          var offset = reader.read32u ();
          return intern_obj_table[obj_counter - offset];
        case 0x08: //cst.CODE_BLOCK32:
          var header = reader.read32u ();
          var tag = header & 0xFF;
          var size = header >> 10;
          var v = [tag];
          if (size == 0) return v;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          stack.push(v, size);
          return v;
        case 0x13: //cst.CODE_BLOCK64:
          caml_failwith ("input_value: data block too large");
          break;
        case 0x09: //cst.CODE_STRING8:
          var len = reader.read8u();
          var v = reader.readstr (len);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0A: //cst.CODE_STRING32:
          var len = reader.read32u();
          var v = reader.readstr (len);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0C: //cst.CODE_DOUBLE_LITTLE:
          var t = new Array(8);;
          for (var i = 0;i < 8;i++) t[7 - i] = reader.read8u ();
          var v = caml_float_of_bytes (t);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0B: //cst.CODE_DOUBLE_BIG:
          var t = new Array(8);;
          for (var i = 0;i < 8;i++) t[i] = reader.read8u ();
          var v = caml_float_of_bytes (t);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0E: //cst.CODE_DOUBLE_ARRAY8_LITTLE:
          var len = reader.read8u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
            v[i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x0D: //cst.CODE_DOUBLE_ARRAY8_BIG:
          var len = reader.read8u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            v [i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x07: //cst.CODE_DOUBLE_ARRAY32_LITTLE:
          var len = reader.read32u();
          var v = new Array(len+1);
          v[0] = 254;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          var t = new Array(8);;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
            v[i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x0F: //cst.CODE_DOUBLE_ARRAY32_BIG:
          var len = reader.read32u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            v [i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x10: //cst.CODE_CODEPOINTER:
        case 0x11: //cst.CODE_INFIXPOINTER:
          caml_failwith ("input_value: code pointer");
          break;
        case 0x12: //cst.CODE_CUSTOM:
        case 0x18: //cst.CODE_CUSTOM_LEN:
        case 0x19: //cst.CODE_CUSTOM_FIXED:
          var c, s = "";
          while ((c = reader.read8u ()) != 0) s += String.fromCharCode (c);
          var ops = caml_custom_ops[s];
          var expected_size;
          if(!ops)
            caml_failwith("input_value: unknown custom block identifier");
          switch(code){
          case 0x12: // cst.CODE_CUSTOM (deprecated)
            break;
          case 0x19: // cst.CODE_CUSTOM_FIXED
            if(!ops.fixed_length)
              caml_failwith("input_value: expected a fixed-size custom block");
            expected_size = ops.fixed_length;
            break;
          case 0x18: // cst.CODE_CUSTOM_LEN
            expected_size = reader.read32u ();
            // Skip size64
            reader.read32s(); reader.read32s();
            break;
          }
          var old_pos = reader.i;
          var size = [0];
          var v = ops.deserialize(reader, size);
          if(expected_size != undefined){
            if(expected_size != size[0])
              caml_failwith("input_value: incorrect length of serialized custom block");
          }
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        default:
          caml_failwith ("input_value: ill-formed message");
        }
      }
    }
  }
  var res = intern_rec ();
  while (stack.length > 0) {
    var size = stack.pop();
    var v = stack.pop();
    var d = v.length;
    if (d < size) stack.push(v, size);
    v[d] = intern_rec ();
  }
  if (typeof ofs!="number") ofs[0] = reader.i;
  return res;
}

//Provides: caml_marshal_data_size mutable
//Requires: caml_failwith, caml_bytes_unsafe_get
function caml_marshal_data_size (s, ofs) {
  function get32(s,i) {
    return (caml_bytes_unsafe_get(s, i) << 24) |
      (caml_bytes_unsafe_get(s, i + 1) << 16) |
      (caml_bytes_unsafe_get(s, i + 2) << 8) |
      caml_bytes_unsafe_get(s, i + 3);
  }
  if (get32(s, ofs) != (0x8495A6BE|0))
    caml_failwith("Marshal.data_size: bad object");
  return (get32(s, ofs + 4));
}

//Provides: MlObjectTable
var MlObjectTable;
if (typeof globalThis.WeakMap === 'undefined') {
  MlObjectTable = function() {
    /* polyfill (using linear search) */
    function NaiveLookup(objs) { this.objs = objs; }
    NaiveLookup.prototype.get = function(v) {
      for (var i = 0; i < this.objs.length; i++) {
        if (this.objs[i] === v) return i;
      }
    };
    NaiveLookup.prototype.set = function() {
      // Do nothing here. [MlObjectTable.store] will push to [this.objs] directly.
    };

    return function MlObjectTable() {
      this.objs = []; this.lookup = new NaiveLookup(this.objs);
    };
  }();
}
else {
  MlObjectTable = function MlObjectTable() {
    this.objs = []; this.lookup = new globalThis.WeakMap();
  };
}

MlObjectTable.prototype.store = function(v) {
  this.lookup.set(v, this.objs.length);
  this.objs.push(v);
}

MlObjectTable.prototype.recall = function(v) {
  var i = this.lookup.get(v);
  return (i === undefined)
    ? undefined : this.objs.length - i;   /* index is relative */
}

//Provides: caml_legacy_custom_code
//Version: >= 4.08
var caml_legacy_custom_code = false

//Provides: caml_legacy_custom_code
//Version: < 4.08
var caml_legacy_custom_code = true

//Provides: caml_output_val
//Requires: caml_int64_to_bytes, caml_failwith
//Requires: caml_int64_bits_of_float
//Requires: caml_is_ml_bytes, caml_ml_bytes_length, caml_bytes_unsafe_get
//Requires: caml_is_ml_string, caml_ml_string_length, caml_string_unsafe_get
//Requires: MlObjectTable, caml_list_to_js_array, caml_legacy_custom_code, caml_custom_ops
//Requires: caml_invalid_argument,caml_string_of_jsbytes
var caml_output_val = function (){
  function Writer () { this.chunk = []; }
  Writer.prototype = {
    chunk_idx:20, block_len:0, obj_counter:0, size_32:0, size_64:0,
    write:function (size, value) {
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
    },
    write_at:function (pos, size, value) {
      var pos = pos;
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[pos++] = (value >> i) & 0xFF;
    },
    write_code:function (size, code, value) {
      this.chunk[this.chunk_idx++] = code;
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
    },
    write_shared:function (offset) {
      if (offset < (1 << 8)) this.write_code(8, 0x04 /*cst.CODE_SHARED8*/, offset);
      else if (offset < (1 << 16)) this.write_code(16, 0x05 /*cst.CODE_SHARED16*/, offset);
      else this.write_code(32, 0x06 /*cst.CODE_SHARED32*/, offset);
    },
    pos:function () { return this.chunk_idx },
    finalize:function () {
      this.block_len = this.chunk_idx - 20;
      this.chunk_idx = 0;
      this.write (32, 0x8495A6BE);
      this.write (32, this.block_len);
      this.write (32, this.obj_counter);
      this.write (32, this.size_32);
      this.write (32, this.size_64);
      return this.chunk;
    }
  }
  return function (v, flags) {
    flags = caml_list_to_js_array(flags);

    var no_sharing = (flags.indexOf(0 /*Marshal.No_sharing*/) !== -1),
        closures =  (flags.indexOf(1 /*Marshal.Closures*/) !== -1);
    /* Marshal.Compat_32 is redundant since integers are 32-bit anyway */

    if (closures)
      globalThis.console.warn("in caml_output_val: flag Marshal.Closures is not supported.");

    var writer = new Writer ();
    var stack = [];
    var intern_obj_table = no_sharing ? null : new MlObjectTable();

    function memo(v) {
      if (no_sharing) return false;
      var existing_offset = intern_obj_table.recall(v);
      if (existing_offset) { writer.write_shared(existing_offset); return true; }
      else { intern_obj_table.store(v); return false; }
    }

    function extern_rec (v) {
      if (v.caml_custom) {
        if (memo(v)) return;
        var name = v.caml_custom;
        var ops = caml_custom_ops[name];
        var sz_32_64 = [0,0];
        if(!ops.serialize)
          caml_invalid_argument("output_value: abstract value (Custom)");
        if(caml_legacy_custom_code) {
          writer.write (8, 0x12 /*cst.CODE_CUSTOM*/);
          for (var i = 0; i < name.length; i++)
            writer.write (8, name.charCodeAt(i));
          writer.write(8, 0);
          ops.serialize(writer, v, sz_32_64);
        } else if(ops.fixed_length == undefined){
          writer.write (8, 0x18 /*cst.CODE_CUSTOM_LEN*/);
          for (var i = 0; i < name.length; i++)
            writer.write (8, name.charCodeAt(i));
          writer.write(8, 0);
          var header_pos = writer.pos ();
          for(var i = 0; i < 12; i++) {
            writer.write(8, 0);
          }
          ops.serialize(writer, v, sz_32_64);
          writer.write_at(header_pos, 32, sz_32_64[0]);
          writer.write_at(header_pos + 4, 32, 0); // zero
          writer.write_at(header_pos + 8, 32, sz_32_64[1]);
        } else {
          writer.write (8, 0x19 /*cst.CODE_CUSTOM_FIXED*/);
          for (var i = 0; i < name.length; i++)
            writer.write (8, name.charCodeAt(i));
          writer.write(8, 0);
          var old_pos = writer.pos();
          ops.serialize(writer, v, sz_32_64);
          if (ops.fixed_length != writer.pos() - old_pos)
            caml_failwith("output_value: incorrect fixed sizes specified by " + name);
        }
        writer.size_32 += 2 + ((sz_32_64[0] + 3) >> 2);
        writer.size_64 += 2 + ((sz_32_64[1] + 7) >> 3);
      }
      else if (v instanceof Array && v[0] === (v[0]|0)) {
        if (v[0] == 251) {
          caml_failwith("output_value: abstract value (Abstract)");
        }
        if (v.length > 1 && memo(v)) return;
        if (v[0] < 16 && v.length - 1 < 8)
          writer.write (8, 0x80 /*cst.PREFIX_SMALL_BLOCK*/ + v[0] + ((v.length - 1)<<4));
        else
          writer.write_code(32, 0x08 /*cst.CODE_BLOCK32*/, ((v.length-1) << 10) | v[0]);
        writer.size_32 += v.length;
        writer.size_64 += v.length;
        if (v.length > 1) stack.push (v, 1);
      } else if (caml_is_ml_bytes(v)) {
        if(!(caml_is_ml_bytes(caml_string_of_jsbytes("")))) {
          caml_failwith("output_value: [Bytes.t] cannot safely be marshaled with [--enable use-js-string]");
        }
        if (memo(v)) return;
        var len = caml_ml_bytes_length(v);
        if (len < 0x20)
          writer.write (8, 0x20 /*cst.PREFIX_SMALL_STRING*/ + len);
        else if (len < 0x100)
          writer.write_code (8, 0x09/*cst.CODE_STRING8*/, len);
        else
          writer.write_code (32, 0x0A /*cst.CODE_STRING32*/, len);
        for (var i = 0;i < len;i++)
          writer.write (8, caml_bytes_unsafe_get(v,i));
        writer.size_32 += 1 + (((len + 4) / 4)|0);
        writer.size_64 += 1 + (((len + 8) / 8)|0);
      } else if (caml_is_ml_string(v)) {
        var len = caml_ml_string_length(v);
        if (len < 0x20)
          writer.write (8, 0x20 /*cst.PREFIX_SMALL_STRING*/ + len);
        else if (len < 0x100)
          writer.write_code (8, 0x09/*cst.CODE_STRING8*/, len);
        else
          writer.write_code (32, 0x0A /*cst.CODE_STRING32*/, len);
        for (var i = 0;i < len;i++)
          writer.write (8, caml_string_unsafe_get(v,i));
        writer.size_32 += 1 + (((len + 4) / 4)|0);
        writer.size_64 += 1 + (((len + 8) / 8)|0);
      } else {
        if (v != (v|0)){
          var type_of_v = typeof v;
          //
          // If a float happens to be an integer it is serialized as an integer
          // (Js_of_ocaml cannot tell whether the type of an integer number is
          // float or integer.) This can result in unexpected crashes when
          // unmarshalling using the standard runtime. It seems better to
          // systematically fail on marshalling.
          //
          //          if(type_of_v != "number")
          caml_failwith("output_value: abstract value ("+type_of_v+")");
          //          var t = caml_int64_to_bytes(caml_int64_bits_of_float(v));
          //          writer.write (8, 0x0B /*cst.CODE_DOUBLE_BIG*/);
          //          for(var i = 0; i<8; i++){writer.write(8,t[i])}
        }
        else if (v >= 0 && v < 0x40) {
          writer.write (8, 0X40 /*cst.PREFIX_SMALL_INT*/ + v);
        } else {
          if (v >= -(1 << 7) && v < (1 << 7))
            writer.write_code(8, 0x00 /*cst.CODE_INT8*/, v);
          else if (v >= -(1 << 15) && v < (1 << 15))
            writer.write_code(16, 0x01 /*cst.CODE_INT16*/, v);
          else
            writer.write_code(32, 0x02 /*cst.CODE_INT32*/, v);
        }
      }
    }
    extern_rec (v);
    while (stack.length > 0) {
      var i = stack.pop ();
      var v = stack.pop ();
      if (i + 1 < v.length) stack.push (v, i + 1);
      extern_rec (v[i]);
    }
    if (intern_obj_table) writer.obj_counter = intern_obj_table.objs.length;
    writer.finalize();
    return writer.chunk;
  }
} ();

//Provides: caml_output_value_to_string mutable
//Requires: caml_output_val, caml_string_of_array
function caml_output_value_to_string (v, flags) {
  return caml_string_of_array (caml_output_val (v, flags));
}

//Provides: caml_output_value_to_bytes mutable
//Requires: caml_output_val, caml_bytes_of_array
function caml_output_value_to_bytes (v, flags) {
  return caml_bytes_of_array (caml_output_val (v, flags));
}

//Provides: caml_output_value_to_buffer
//Requires: caml_output_val, caml_failwith, caml_blit_bytes
function caml_output_value_to_buffer (s, ofs, len, v, flags) {
  var t = caml_output_val (v, flags);
  if (t.length > len) caml_failwith ("Marshal.to_buffer: buffer overflow");
  caml_blit_bytes(t, 0, s, ofs, t.length);
  return 0;
}
