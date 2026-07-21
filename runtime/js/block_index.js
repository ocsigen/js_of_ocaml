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
// We additionally adopt the convention in JSOO that some known-invalid indices
// are represented as a tag-1 block whose only field is the number of times the
// pointer has been advanced. Reads/writes from/to such an index raise.
//
// Our motivation for permitting a representation of these invalid indices is
// that pointers should generally be safe to create and manipulate, and the
// only points of unsafety should be reads and writes.

//Provides: caml_get_idx_bytecode mutable (mutable, const)
//Requires: caml_invalid_argument
//Version: >= 5.2
//If: oxcaml
function caml_get_idx_bytecode(base, idx) {
  if (idx[0] !== 0) {
    caml_invalid_argument(
      "caml_get_idx_bytecode: attempted to read from an invalid index",
    );
  }
  var depth = idx.length - 1;
  var res = base;
  for (var i = 1; i <= depth; i++) {
    res = res[idx[i] + 1];
  }
  return res;
}

//Provides: caml_set_idx_bytecode (mutable, const, mutable)
//Requires: caml_invalid_argument
//Version: >= 5.2
//If: oxcaml
function caml_set_idx_bytecode(base, idx, v) {
  if (idx[0] !== 0) {
    caml_invalid_argument(
      "caml_set_idx_bytecode: attempted to write to an invalid index",
    );
  }
  var depth = idx.length - 1;
  var dst = base;
  for (var i = 1; i < depth; i++) {
    dst = dst[idx[i] + 1];
  }
  dst[idx[depth] + 1] = v;
  return 0;
}

//Provides: caml_deepen_idx_bytecode const (const, const)
//Version: >= 5.2
//If: oxcaml
function caml_deepen_idx_bytecode(idx_prefix, idx_suffix) {
  var prefix_depth = idx_prefix.length - 1;
  var suffix_depth = idx_suffix.length - 1;
  var block = new Array(1 + prefix_depth + suffix_depth);
  block[0] = 0;
  for (var i = 0; i < prefix_depth; i++) {
    block[1 + i] = idx_prefix[1 + i];
  }
  for (var i = 0; i < suffix_depth; i++) {
    block[1 + prefix_depth + i] = idx_suffix[1 + i];
  }
  return block;
}
