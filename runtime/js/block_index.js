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

///////////// Block indices
// In bytecode, JSOO, and WASM, valid block indices are represented as tag-0
// blocks containing the sequence of field positions.
//
// On JSOO, blocks are arrays whose [0] element is the tag and whose subsequent
// elements are the fields.
//
// For more information on block indices and their bytecode representation, see
// https://github.com/oxcaml/oxcaml/blob/main/jane/doc/extensions/_03-unboxed-types/03-block-indices.md#representation-of-block-indices
//
// We additionally adopt the convention in JSOO that indices with a non-zero
// tag are special:
//
// - A tag-1 block whose only field is the number of times the pointer has
//   been advanced represents a known-invalid index. Reads/writes from/to
//   such an index raise.
// - A tag-2 (resp. tag-3, tag-4) block [tag, offset] represents an index to
//   position [offset] of a string (resp. bytes, bigarray). The tag tells us
//   how to perform the indexing into the base value.
//
// Our motivation for permitting a representation of these invalid indices is
// that pointers should generally be safe to create and manipulate, and the
// only points of unsafety should be reads and writes.
//
// Our motivation for permitting string-like indices is to allow slices backed
// by strings, bytes, and bigstrings on JSOO, as they are on native.

//Provides: caml_get_idx_bytecode mutable (mutable, const)
//Requires: caml_invalid_argument
//Requires: caml_string_geti8, caml_bytes_geti8, caml_ba_uint8_geti8
//Version: >= 5.2
//If: oxcaml
function caml_get_idx_bytecode(base, idx) {
  switch (TAG(idx)) {
    case 0: {
      var depth = idx.length - 1;
      var res = base;
      for (var i = 1; i <= depth; i++) {
        res = res[idx[i] + 1];
      }
      return res;
    }
    // The tag of a string-like index tells us how to index into the base
    // value (see the note on block indices above).

    // These are bounds checked and sign extended. We could add
    // [caml_string_geti8u]-like primitives (which already exist for the
    // wider reads).

    case 2:
      return caml_string_geti8(base, idx[1]);
    case 3:
      return caml_bytes_geti8(base, idx[1]);
    case 4:
      return caml_ba_uint8_geti8(base, idx[1]);
    default:
      caml_invalid_argument(
        "caml_get_idx_bytecode: attempted to read from an invalid index",
      );
  }
}

//Provides: caml_set_idx_bytecode (mutable, const, mutable)
//Requires: caml_invalid_argument
//Requires: caml_bytes_set8, caml_ba_uint8_set8
//Version: >= 5.2
//If: oxcaml
function caml_set_idx_bytecode(base, idx, v) {
  switch (TAG(idx)) {
    case 0: {
      var depth = idx.length - 1;
      var dst = base;
      for (var i = 1; i < depth; i++) {
        dst = dst[idx[i] + 1];
      }
      dst[idx[depth] + 1] = v;
      return 0;
    }
    // The tag of a string-like index tells us how to index into the base
    // value (see the note on block indices above). Note that writing through
    // a tag-2 (string) index is invalid: strings are immutable.

    // These are bounds checked. We could add a [caml_ba_uint8_set8u]-like
    // primitive (which already exists for the wider writes). We don't use
    // [caml_bytes_unsafe_set] for consistency.

    case 3:
      caml_bytes_set8(base, idx[1], v);
      return 0;
    case 4:
      caml_ba_uint8_set8(base, idx[1], v);
      return 0;
    default:
      caml_invalid_argument(
        "caml_set_idx_bytecode: attempted to write to an invalid index",
      );
  }
}

//Provides: caml_deepen_idx_bytecode const (const, const)
//Version: >= 5.2
//If: oxcaml
function caml_deepen_idx_bytecode(idx_prefix, idx_suffix) {
  if (TAG(idx_prefix) !== 0) {
    // Deepening a non-block (invalid or string-like) index is only meaningful
    // if the suffix is the identity (empty) index.
    if (idx_suffix.length === 1) return idx_prefix;
    else {
      var idx = new Array(2);
      idx[0] = 1; // Invalid index
      idx[1] = 0;
      return idx;
    }
  }
  var prefix_depth = idx_prefix.length - 1;
  var suffix_depth = idx_suffix.length - 1;
  var block = new Array(1 + prefix_depth + suffix_depth).fill(0);
  for (var i = 0; i < prefix_depth; i++) {
    block[1 + i] = idx_prefix[1 + i];
  }
  for (var i = 0; i < suffix_depth; i++) {
    block[1 + prefix_depth + i] = idx_suffix[1 + i];
  }
  return block;
}

///////////// Atomic block indices
// We do not support nested atomic indices, so a valid atomic index is a tag-0
// block with exactly one position.

//Provides: caml_check_atomic_idx
//Requires: caml_unimplemented_ext_ptr, caml_invalid_argument
//Version: >= 5.2
//If: oxcaml
function caml_check_atomic_idx(base, idx) {
  // A null base means [idx] is a raw address (an external ptr), which cannot
  // be dereferenced here.
  if (base === null) caml_unimplemented_ext_ptr();
  if (TAG(idx) !== 0 || idx.length !== 2)
    caml_invalid_argument(
      "caml_check_atomic_idx: attempted to access an invalid index",
    );
}

//Provides: caml_atomic_load_idx_bytecode (mutable, const)
//Requires: caml_check_atomic_idx, caml_atomic_load_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_load_idx_bytecode(base, idx) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_load_field(base, idx[1]);
}

//Provides: caml_atomic_set_idx_bytecode (mutable, const, mutable)
//Requires: caml_check_atomic_idx, caml_atomic_set_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_set_idx_bytecode(base, idx, v) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_set_field(base, idx[1], v);
}

//Provides: caml_atomic_exchange_idx_bytecode (mutable, const, mutable)
//Requires: caml_check_atomic_idx, caml_atomic_exchange_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_exchange_idx_bytecode(base, idx, v) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_exchange_field(base, idx[1], v);
}

//Provides: caml_atomic_compare_exchange_idx_bytecode (mutable, const, const, mutable)
//Requires: caml_check_atomic_idx, caml_atomic_compare_exchange_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_compare_exchange_idx_bytecode(base, idx, o, n) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_compare_exchange_field(base, idx[1], o, n);
}

//Provides: caml_atomic_cas_idx_bytecode (mutable, const, const, mutable)
//Requires: caml_check_atomic_idx, caml_atomic_cas_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_cas_idx_bytecode(base, idx, o, n) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_cas_field(base, idx[1], o, n);
}

//Provides: caml_atomic_fetch_add_idx_bytecode (mutable, const, const)
//Requires: caml_check_atomic_idx, caml_atomic_fetch_add_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_fetch_add_idx_bytecode(base, idx, i) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_fetch_add_field(base, idx[1], i);
}

//Provides: caml_atomic_add_idx_bytecode (mutable, const, const)
//Requires: caml_check_atomic_idx, caml_atomic_add_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_add_idx_bytecode(base, idx, i) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_add_field(base, idx[1], i);
}

//Provides: caml_atomic_sub_idx_bytecode (mutable, const, const)
//Requires: caml_check_atomic_idx, caml_atomic_sub_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_sub_idx_bytecode(base, idx, i) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_sub_field(base, idx[1], i);
}

//Provides: caml_atomic_land_idx_bytecode (mutable, const, const)
//Requires: caml_check_atomic_idx, caml_atomic_land_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_land_idx_bytecode(base, idx, i) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_land_field(base, idx[1], i);
}

//Provides: caml_atomic_lor_idx_bytecode (mutable, const, const)
//Requires: caml_check_atomic_idx, caml_atomic_lor_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_lor_idx_bytecode(base, idx, i) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_lor_field(base, idx[1], i);
}

//Provides: caml_atomic_lxor_idx_bytecode (mutable, const, const)
//Requires: caml_check_atomic_idx, caml_atomic_lxor_field
//Version: >= 5.2
//If: oxcaml
function caml_atomic_lxor_idx_bytecode(base, idx, i) {
  caml_check_atomic_idx(base, idx);
  return caml_atomic_lxor_field(base, idx[1], i);
}

// We are reasonably sure that only the [ptr] primitives below actually need
// to check for the error cases currently handled in the [idx] primitives.
// Checking only in the [ptr] primitives would improve the performance of the
// [idx] primitives in isolation.
// TODO: Consider making the change described above.

///////////// Pointers
// In bytecode, a pointer is an unboxed pair of a base value and a block
// index. Unboxed products are represented as blocks in bytecode, so a
// pointer arrives as a single tag-0 block [0, base, idx], and
// reading/writing through it is exactly reading/writing at the block index.
//
// External pointers (and pointers with a [Null] base) are unsupported in
// bytecode and fail with an 'unimplemented' message. The messages are those
// of the OxCaml bytecode runtime.

//Provides: caml_unimplemented_ext_ptr
//Requires: caml_failwith
//Version: >= 5.2
//If: oxcaml
function caml_unimplemented_ext_ptr() {
  caml_failwith("External ptrs are unimplemented on bytecode");
}

//Provides: caml_get_ptr_bytecode (mutable)
//Requires: caml_get_idx_bytecode, caml_unimplemented_ext_ptr
//Version: >= 5.2
//If: oxcaml
function caml_get_ptr_bytecode(ptr) {
  if (ptr[1] === null) caml_unimplemented_ext_ptr();
  return caml_get_idx_bytecode(ptr[1], ptr[2]);
}

//Provides: caml_set_ptr_bytecode (mutable, mutable)
//Requires: caml_set_idx_bytecode, caml_unimplemented_ext_ptr
//Version: >= 5.2
//If: oxcaml
function caml_set_ptr_bytecode(ptr, v) {
  if (ptr[1] === null) caml_unimplemented_ext_ptr();
  return caml_set_idx_bytecode(ptr[1], ptr[2], v);
}

//Provides: caml_get_ext_ptr_bytecode (mutable)
//Requires: caml_failwith
//Version: >= 5.2
//If: oxcaml
function caml_get_ext_ptr_bytecode(_idx) {
  caml_failwith("External ptr primitives are unimplemented on bytecode");
}

//Provides: caml_set_ext_ptr_bytecode (mutable, mutable)
//Requires: caml_failwith
//Version: >= 5.2
//If: oxcaml
function caml_set_ext_ptr_bytecode(_idx, _v) {
  caml_failwith("External ptr primitives are unimplemented on bytecode");
}
