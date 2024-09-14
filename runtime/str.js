// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2020 - Hugo Heuzard
// Copyright (C) 2020 - Shachar Itzhaky
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

// Based on https://github.com/ocaml/ocaml/blob/4.07/otherlibs/str/strstubs.c
// Copied from https://github.com/jscoq/jscoq/blob/v8.11/coq-js/js_stub/str.js

//Provides: re_match
//Requires: caml_jsbytes_of_string, caml_js_from_array, caml_uint8_array_of_string
//Requires: caml_string_get

const re_match = (() => {
  const re_word_letters = [
    0x00, 0x00, 0x00, 0x00 /* 0x00-0x1F: none */, 0x00, 0x00, 0xff,
    0x03 /* 0x20-0x3F: digits 0-9 */, 0xfe, 0xff, 0xff,
    0x87 /* 0x40-0x5F: A to Z, _ */, 0xfe, 0xff, 0xff,
    0x07 /* 0x60-0x7F: a to z */, 0x00, 0x00, 0x00, 0x00 /* 0x80-0x9F: none */,
    0x00, 0x00, 0x00, 0x00 /* 0xA0-0xBF: none */, 0xff, 0xff, 0x7f,
    0xff /* 0xC0-0xDF: Latin-1 accented uppercase */, 0xff, 0xff, 0x7f,
    0xff /* 0xE0-0xFF: Latin-1 accented lowercase */,
  ];

  const opcodes = {
    CHAR: 0,
    CHARNORM: 1,
    STRING: 2,
    STRINGNORM: 3,
    CHARCLASS: 4,
    BOL: 5,
    EOL: 6,
    WORDBOUNDARY: 7,
    BEGGROUP: 8,
    ENDGROUP: 9,
    REFGROUP: 10,
    ACCEPT: 11,
    SIMPLEOPT: 12,
    SIMPLESTAR: 13,
    SIMPLEPLUS: 14,
    GOTO: 15,
    PUSHBACK: 16,
    SETMARK: 17,
    CHECKPROGRESS: 18,
  };

  function is_word_letter(c) {
    return (re_word_letters[c >> 3] >> (c & 7)) & 1;
  }

  function in_bitset(s, i) {
    return (caml_string_get(s, i >> 3) >> (i & 7)) & 1;
  }

  function re_match_impl(re, s, pos, partial) {
    const prog = caml_js_from_array(re[1]);
    const cpool = caml_js_from_array(re[2]);
    const normtable = caml_jsbytes_of_string(re[3]);
    const numgroups = re[4] | 0;
    const numregisters = re[5] | 0;
    const startchars = re[6] | 0;

    const s_ = caml_uint8_array_of_string(s);

    let pc = 0;
    let quit = false;
    const stack = [];
    const groups = new Array(numgroups);
    const re_register = new Array(numregisters);

    for (let i = 0; i < groups.length; i++) {
      groups[i] = { start: -1, end: -1 };
    }
    groups[0].start = pos;

    const backtrack = () => {
      while (stack.length) {
        const item = stack.pop();
        if (item.undo) {
          item.undo.obj[item.undo.prop] = item.undo.value;
        } else if (item.pos) {
          pc = item.pos.pc;
          pos = item.pos.txt;
          return;
        }
      }
      quit = true;
    };

    const push = (item) => {
      stack.push(item);
    };

    const accept = () => {
      groups[0].end = pos;
      const result = new Array(1 + groups.length * 2);
      result[0] = 0; // tag
      for (let i = 0; i < groups.length; i++) {
        const g = groups[i];
        if (g.start < 0 || g.end < 0) {
          g.start = g.end = -1;
        }
        result[2 * i + 1] = g.start;
        result[2 * i + 1 + 1] = g.end;
      }
      return result;
    };

    const prefix_match = () => {
      if (partial) return accept();
      else backtrack();
    };

    /* Main DFA interpreter loop */
    while (!quit) {
      const op = prog[pc] & 0xff;
      const sarg = prog[pc] >> 8;
      const uarg = sarg & 0xff;
      let c = s_[pos];
      let group;

      pc++;

      switch (op) {
        case opcodes.CHAR:
          if (pos === s_.length) {
            prefix_match();
            break;
          }
          if (c === uarg) pos++;
          else backtrack();
          break;
        case opcodes.CHARNORM:
          if (pos === s_.length) {
            prefix_match();
            break;
          }
          if (normtable.charCodeAt(c) === uarg) pos++;
          else backtrack();
          break;
        case opcodes.STRING:
          for (
            let arg = caml_jsbytes_of_string(cpool[uarg]), i = 0;
            i < arg.length;
            i++
          ) {
            if (pos === s_.length) {
              prefix_match();
              break;
            }
            if (c === arg.charCodeAt(i)) c = s_[++pos];
            else {
              backtrack();
              break;
            }
          }
          break;
        case opcodes.STRINGNORM:
          for (
            let arg = caml_jsbytes_of_string(cpool[uarg]), i = 0;
            i < arg.length;
            i++
          ) {
            if (pos === s_.length) {
              prefix_match();
              break;
            }
            if (normtable.charCodeAt(c) === arg.charCodeAt(i)) c = s_[++pos];
            else {
              backtrack();
              break;
            }
          }
          break;
        case opcodes.CHARCLASS:
          if (pos === s_.length) {
            prefix_match();
            break;
          }
          if (in_bitset(cpool[uarg], c)) pos++;
          else backtrack();
          break;
        case opcodes.BOL:
          if (pos > 0 && s_[pos - 1] != 10 /* \n */) {
            backtrack();
          }
          break;
        case opcodes.EOL:
          if (pos < s_.length && s_[pos] != 10 /* \n */) {
            backtrack();
          }
          break;
        case opcodes.WORDBOUNDARY:
          if (pos == 0) {
            if (pos === s_.length) {
              prefix_match();
              break;
            }
            if (is_word_letter(s_[0])) break;
            backtrack();
          } else if (pos === s_.length) {
            if (is_word_letter(s_[pos - 1])) break;
            backtrack();
          } else {
            if (is_word_letter(s_[pos - 1]) != is_word_letter(s_[pos])) break;
            backtrack();
          }
          break;
        case opcodes.BEGGROUP:
          group = groups[uarg];
          push({ undo: { obj: group, prop: "start", value: group.start } });
          group.start = pos;
          break;
        case opcodes.ENDGROUP:
          group = groups[uarg];
          push({ undo: { obj: group, prop: "end", value: group.end } });
          group.end = pos;
          break;
        case opcodes.REFGROUP:
          group = groups[uarg];
          if (group.start < 0 || group.end < 0) {
            backtrack();
            break;
          }
          for (let i = group.start; i < group.end; i++) {
            if (pos === s_.length) {
              prefix_match();
              break;
            }
            if (s_[i] != s_[pos]) {
              backtrack();
              break;
            }
            pos++;
          }
          break;
        case opcodes.SIMPLEOPT:
          if (in_bitset(cpool[uarg], c)) pos++;
          break;
        case opcodes.SIMPLESTAR:
          while (in_bitset(cpool[uarg], c)) c = s_[++pos];
          break;
        case opcodes.SIMPLEPLUS:
          if (pos === s_.length) {
            prefix_match();
            break;
          }
          if (in_bitset(cpool[uarg], c)) {
            do {
              c = s_[++pos];
            } while (in_bitset(cpool[uarg], c));
          } else backtrack();
          break;
        case opcodes.ACCEPT:
          return accept();
        case opcodes.GOTO:
          pc = pc + sarg;
          break;
        case opcodes.PUSHBACK:
          push({ pos: { pc: pc + sarg, txt: pos } });
          break;
        case opcodes.SETMARK:
          push({
            undo: { obj: re_register, prop: uarg, value: re_register[uarg] },
          });
          re_register[uarg] = pos;
          break;
        case opcodes.CHECKPROGRESS:
          if (re_register[uarg] === pos) backtrack();
          break;
        default:
          throw new Error("Invalid bytecode");
      }
    }
    return 0;
  }

  return re_match_impl;
})();

//Provides: re_search_forward
//Requires: re_match, caml_ml_string_length, caml_invalid_argument
function re_search_forward(re, s, pos) {
  if (pos < 0 || pos > caml_ml_string_length(s))
    caml_invalid_argument("Str.search_forward");
  while (pos <= caml_ml_string_length(s)) {
    const res = re_match(re, s, pos, 0);
    if (res) return res;
    pos++;
  }

  return [0]; /* [||] : int array */
}

//Provides: re_search_backward
//Requires: re_match, caml_ml_string_length, caml_invalid_argument
function re_search_backward(re, s, pos) {
  if (pos < 0 || pos > caml_ml_string_length(s))
    caml_invalid_argument("Str.search_backward");
  while (pos >= 0) {
    const res = re_match(re, s, pos, 0);
    if (res) return res;
    pos--;
  }

  return [0]; /* [||] : int array */
}

//Provides: re_string_match
//Requires: re_match, caml_ml_string_length, caml_invalid_argument
function re_string_match(re, s, pos) {
  if (pos < 0 || pos > caml_ml_string_length(s))
    caml_invalid_argument("Str.string_match");
  const res = re_match(re, s, pos, 0);
  if (res) return res;
  else return [0];
}

//Provides: re_partial_match
//Requires: re_match, caml_ml_string_length, caml_invalid_argument
function re_partial_match(re, s, pos) {
  if (pos < 0 || pos > caml_ml_string_length(s))
    caml_invalid_argument("Str.partial_match");
  const res = re_match(re, s, pos, 1);
  if (res) return res;
  else return [0];
}

//Provides: re_replacement_text
//Requires: caml_jsbytes_of_string, caml_string_of_jsbytes
//Requires: caml_array_get
//Requires: caml_failwith
// external re_replacement_text: string -> int array -> string -> string
function re_replacement_text(repl, groups, orig) {
  const repl_ = caml_jsbytes_of_string(repl);
  const len = repl_.length;
  const orig_ = caml_jsbytes_of_string(orig);
  let res = ""; //result
  let n = 0; // current position
  let cur; //current char
  let start;
  let end;
  let c;
  while (n < len) {
    cur = repl_.charAt(n++);
    if (cur != "\\") {
      res += cur;
    } else {
      if (n == len) caml_failwith("Str.replace: illegal backslash sequence");
      cur = repl_.charAt(n++);
      switch (cur) {
        case "\\":
          res += cur;
          break;
        case "0":
        case "1":
        case "2":
        case "3":
        case "4":
        case "5":
        case "6":
        case "7":
        case "8":
        case "9":
          c = +cur;
          if (c * 2 >= groups.length - 1)
            caml_failwith("Str.replace: reference to unmatched group");
          start = caml_array_get(groups, c * 2);
          end = caml_array_get(groups, c * 2 + 1);
          if (start == -1)
            caml_failwith("Str.replace: reference to unmatched group");
          res += orig_.slice(start, end);
          break;
        default:
          res += "\\" + cur;
      }
    }
  }
  return caml_string_of_jsbytes(res);
}

//Provides: caml_str_initialize
function caml_str_initialize(unit) {
  return 0;
}
