/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

function INT32(v) {return v|0;}
function mk_block(size, tag) { return [tag]; }
function set(x, i, v) { x[i+1]=v; }
function float_of_bytes (x) { return 1.2345; }
var STRING_TAG = 0;

////////////////////

var INTEXT_MAGIC_NUMBER = 0x8495A6BE, PREFIX_SMALL_BLOCK =  0x80,
    PREFIX_SMALL_INT =    0x40,       PREFIX_SMALL_STRING = 0x20;

var CODE_INT8 =      0x00, CODE_INT16 =      0x01,
    CODE_INT32 =     0x02, CODE_INT64 =      0x03,
    CODE_SHARED8 =   0x04, CODE_SHARED16 =   0x05,
    CODE_SHARED32 =  0x06, CODE_BLOCK32 =    0x08,
    CODE_BLOCK64 =   0x13, CODE_STRING8 =    0x09,
    CODE_STRING32 =  0x0A, CODE_DOUBLE_BIG = 0x0B,
    CODE_DOUBLE_LITTLE =         0x0C, CODE_DOUBLE_ARRAY8_BIG =  0x0D,
    CODE_DOUBLE_ARRAY8_LITTLE =  0x0E, CODE_DOUBLE_ARRAY32_BIG = 0x0F,
    CODE_DOUBLE_ARRAY32_LITTLE = 0x07, CODE_CODEPOINTER =        0x10,
    CODE_INFIXPOINTER =          0x11, CODE_CUSTOM =             0x12;
//    CODE_DOUBLE_NATIVE =         CODE_DOUBLE_BIG,
//    CODE_DOUBLE_ARRAY8_NATIVE =  CODE_DOUBLE_ARRAY8_BIG,
//    CODE_DOUBLE_ARRAY32_NATIVE = CODE_DOUBLE_ARRAY32_BIG,
//    CODE_DOUBLE_NATIVE =         CODE_DOUBLE_LITTLE,
//    CODE_DOUBLE_ARRAY8_NATIVE =  CODE_DOUBLE_ARRAY8_LITTLE,
//    CODE_DOUBLE_ARRAY32_NATIVE = CODE_DOUBLE_ARRAY32_LITTLE,
//    ENTRIES_PER_TRAIL_BLOCK =  1025, SIZE_EXTERN_OUTPUT_BLOCK = 8100;

function Reader (chunk) {
    this.chunk = chunk;
    this.chunk_idx = 0;
}

Reader.prototype.read8u = function () {
    return this.chunk[this.chunk_idx++];
}
Reader.prototype.read8s = function () {
    var r = this.chunk[this.chunk_idx++];
    if (r & 0x80)
	return r - 256;
    else
	return r;
}
Reader.prototype.read16u = function () {
    var r = this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    return r;
}
Reader.prototype.read16s = function () {
    var r = this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    if (r & 0x8000)
	return r - 65536;
    else
	return r;
}
/* don't know if works... */
Reader.prototype.read32u = function () {
    var r = this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    return INT32(r);
}
Reader.prototype.read32s = function () {
    var r = this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    r = (r << 8) | this.chunk[this.chunk_idx++];
    return INT32(r);
}

function input_size (chunk, error) {
    var reader = new Reader (chunk);

    if (reader.read8u () != 0x84
	|| reader.read8u () != 0x95
	|| reader.read8u () != 0xA6
	|| reader.read8u () != 0xBE){
	error ("ill-formed message (bad magic)");
    }

    var block_len = reader.read32u ();
    return block_len;
}

function input_val (chunk, error) {
    var reader = new Reader (chunk);

    if (reader.read8u () != 0x84
	|| reader.read8u () != 0x95
	|| reader.read8u () != 0xA6
	|| reader.read8u () != 0xBE){
	error ("ill-formed message (bad magic)");
    }

    /* sizes */
    var block_len = reader.read32u ();
    var num_objects = reader.read32u ();
    var size_32 = reader.read32u ();
    var size_64 = reader.read32u ();

    var intern_obj_table = [];
    var obj_counter = 0;
    function intern_rec () {
	var code = reader.read8u ();
	if (code >= PREFIX_SMALL_INT) {
	    if (code >= PREFIX_SMALL_BLOCK) {
		var tag = code & 0xF;
		var size = (code >> 4) & 0x7;
		var v = mk_block (size, tag);
		intern_obj_table[obj_counter++] = v;
		for(var d = 0; d < size; d++)
		    set (v, d, intern_rec ());
		return v;
	    } else {
		return (code & 0x3F);
	    }
	} else {
	    if (code >= PREFIX_SMALL_STRING) {
		var len = (code & 0x1F);
		var v = new MlString (len);
		intern_obj_table[obj_counter++] = v;
		for (var d = 0;d < len;d++) {
		    v.set (d, reader.read8u ());
		}
		return v;
	    } else {
		switch(code) {
		case CODE_INT8:
		    return reader.read8s ();
		case CODE_INT16:
		    return reader.read16s ();
		case CODE_INT32:
		    return reader.read32s ();
		case CODE_INT64:
		    var t = [];
		    for (var j = 0;j < 8;j++)
			t[j] = reader.read8u();
		    return int64_of_bytes (t);
		case CODE_SHARED8: {
		    var ofs = reader.read8u ();
		    return intern_obj_table[obj_counter - ofs];
		}
		case CODE_SHARED16: {
		    var ofs = reader.read16u ();
		    return intern_obj_table[obj_counter - ofs];
		}
		case CODE_SHARED32: {
		    var ofs = reader.read32u ();
		    return intern_obj_table[obj_counter - ofs];
		}
		case CODE_BLOCK32: {
		    var header = reader.read32u ();
		    var tag = header & 0xFF;
		    var size = header >> 10;
		    var v = mk_block (size, tag);
		    intern_obj_table[obj_counter++] = v;
		    for(var d = 0; d < size; d++)
			set (v, d, intern_rec ());
		    return v;
		}
		case CODE_BLOCK64:
		    error ("data block too large");
		    break;
		case CODE_STRING8: {
		    var len = reader.read8u();
		    var v = new MlString (len);
		    intern_obj_table[obj_counter++] = v;
		    for (var d = 0;d < len;d++) {
			v.set (d, reader.read8u ());
		    }
		    return v;
		}
		case CODE_STRING32: {
		    var len = reader.read32u();
		    var v = new MlString (len);
		    intern_obj_table[obj_counter++] = v;
		    for (var d = 0;d < len;d++) {
			v.set (d, reader.read8u ());
		    }
		    return v;
		}
		case CODE_DOUBLE_LITTLE: {
		    var t = [];
		    for (var i = 0;i < 8;i++)
			t[7 - i] = reader.read8u();
		    return float_of_bytes (t);
		}

		case CODE_DOUBLE_BIG: {
		    var t = [];
		    for (var i = 0;i < 8;i++)
			t[i] = reader.read8u();
		    return float_of_bytes (t);
		}
		case CODE_DOUBLE_ARRAY8_LITTLE: {
		    var len = reader.read8u();
		    var v = mk_block (len, DOUBLE_ARRAY_TAG);
		    for (var i = 0;i < len;i++) {
			var t = [];
			for (var j = 0;j < 8;j++)
			    t[7 - j] = reader.read8u();
			set (v, i, unbox_float (float_of_bytes (t)));
		    }
		    return v;
		}
		case CODE_DOUBLE_ARRAY8_BIG: {
		    var len = reader.read8u();
		    var v = mk_block (len, DOUBLE_ARRAY_TAG);
		    for (var i = 0;i < len;i++) {
			var t = [];
			for (var j = 0;j < 8;j++)
			    t[j] = reader.read8u();
			set (v, i, unbox_float (float_of_bytes (t)));
		    }
		    return v;
		}
		case CODE_DOUBLE_ARRAY32_LITTLE: {
		    var len = reader.read32u();
		    var v = mk_block (len, DOUBLE_ARRAY_TAG);
		    for (var i = 0;i < len;i++) {
			var t = [];
			for (var j = 0;j < 8;j++)
			    t[7 - j] = reader.read8u();
			set (v, i, unbox_float (float_of_bytes (t)));
		    }
		    return v;
		}
		case CODE_DOUBLE_ARRAY32_BIG: {
		    var len = reader.read32u();
		    var v = mk_block (len, DOUBLE_ARRAY_TAG);
		    for (var i = 0;i < len;i++) {
			var t = [];
			for (var j = 0;j < 8;j++)
			    t[j] = reader.read8u();
			set (v, i, unbox_float (float_of_bytes (t)));
		    }
		    return v;
		}
		case CODE_CODEPOINTER:
		    /* no checks */
		    return reader.read32u ();
		case CODE_INFIXPOINTER: {
		    /* no checks */
		    var ofs = reader.read32u ();
		    var clos = intern_rec ();
		    return clos.shift (ofs);
		}
		case CODE_CUSTOM: {
		    var s = "";
		    var si = reader.read8u ();
		    while (si != 0) {
			s += String.fromCharCode (si);
			si = reader.read8u ();
		    }
		    var c = find_custom (s);
		    if (!c)
			error ("unknown custom identifier " + s);
		    else
			return c.deserialize (reader);
		}
		default:
		    error ("ill-formed message (" + code + ")");
		}
	    }
	}
    }
    var v = intern_rec ();
    return v;
}

// Caml name: unmarshal
// Type:      string -> int -> 'a
caml_input_value_from_string = function (s, ofs) {
    var s = s.toByteArray();
    function caml_failwith (s) {throw (s);};
    return input_val (s.slice (ofs + 1),caml_failwith);
}

// Caml name: marshal_data_size
// Type:      string -> int -> int
caml_marshal_data_size = function (s, ofs) {
    var s = s.toByteArray();
    function caml_failwith (s) {throw (s);};
    return input_size (s.slice (ofs + 1), caml_failwith);
}

function Writer () {
    this.chunk = [];
    this.chunk_idx = 20;
    this.block_len = 0;
    this.obj_counter = 0;
    this.size_32 = 0;
    this.size_64 = 0;
}

Writer.prototype.write = function (size, value) {
    for (var i = size - 8;i >= 0;i -= 8)
	this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
}

Writer.prototype.write_code = function (size, code, value) {
    this.chunk[this.chunk_idx++] = code;
    for (var i = size - 8;i >= 0;i -= 8)
	this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
}

Writer.prototype.finalize = function () {
    this.block_len = this.chunk_idx - 20;
    this.chunk_idx = 0;
    this.write (32, 0x8495A6BE);
    this.write (32, this.block_len);
    this.write (32, this.obj_counter);
    this.write (32, this.size_32);
    this.write (32, this.size_64);
    return this.chunk;
}

function HD(v) {return (v.length << 10) | v[0];}

function output_val (v, error) {
    var writer = new Writer ();
    function extern_rec (v) {
        if (v instanceof MlString) {
            var len = v.length;
            if (len < 0x20) {
                writer.write (8, PREFIX_SMALL_STRING + len);
            } else if (len < 0x100) {
                writer.write_code (8, CODE_STRING8, len);
            } else {
                writer.write_code (32, CODE_STRING32, len);
            }
            for (var i = 0;i < len;i++)
                writer.write (8, v.get(i));
            writer.size_32 += 1 + (((len + 4) / 4)|0);
            writer.size_64 += 1 + (((len + 8) / 8)|0);
            writer.obj_counter++;
        } else if (v instanceof Array) {
	    if (v.length == 1) {
		if (v[0] < 16)
		    writer.write (8, PREFIX_SMALL_BLOCK + v[0]);
		else
		    writer.write_code (32, CODE_BLOCK32, HD(v));
		return;
	    }
            if (v[0] < 16 && v.length - 1 < 8) {
                writer.write (8, PREFIX_SMALL_BLOCK + v[0] + ((v.length - 1)<<4));
            } else {
                writer.write_code(32, CODE_BLOCK32, HD(v));
            }
            writer.size_32 += v.length ;
            writer.size_64 += v.length ;
            writer.obj_counter++;
            for (i = 1; i < v.length; i++) {
                extern_rec (v[i]);
            }
        } else {
            v |= 0;
	    if (v >= 0 && v < 0x40) {
		writer.write (8, PREFIX_SMALL_INT + v);
	    } else {
		if (v >= -(1 << 7) && v < (1 << 7)) {
		    writer.write_code(8, CODE_INT8, v);
		} else {
		    if (v >= -(1 << 15) && v < (1 << 15)) {
			writer.write_code(16, CODE_INT16, v);
		    } else {
			writer.write_code(32, CODE_INT32, v);
		    }
		}
	    }
        }
    }
    extern_rec (v);
    writer.finalize ();
    return writer.chunk;
}


// Caml name: to_string
// Type:      'a -> extern_flags list -> string
function caml_output_value_to_string (v, fl) {
    /* ignores flags... */
    var vm = this;
    function caml_failwith (s) {throw (s);};
    var t = output_val (v, caml_failwith);
    var b = new MlString (t.length);
    for (var i = 0;i < t.length;i++) {
	b.set(i, t[i]);
    }
    return b;
}

// Caml name: to_channel
// Type:      out_channel -> 'a -> extern_flags list -> unit
//function caml_output_value (chan, v, fl) {
//}

// Caml name: to_buffer_unsafe
// Type:      string -> int -> int -> 'a -> extern_flags list -> int
function caml_output_value_to_buffer (s, ofs, len, v, fl) {
    var vm = this;
    function caml_failwith (s) {throw (s);};
    var t = output_val (v, caml_failwith);
    for (var i = 0;i < t.length;i++) {
	s.set (ofs + i, t[i]);
    }
    return t.length;
}

