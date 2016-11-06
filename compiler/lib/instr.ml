(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type t =
  | ACC0
  | ACC1
  | ACC2
  | ACC3
  | ACC4
  | ACC5
  | ACC6
  | ACC7
  | ACC
  | PUSH
  | PUSHACC0
  | PUSHACC1
  | PUSHACC2
  | PUSHACC3
  | PUSHACC4
  | PUSHACC5
  | PUSHACC6
  | PUSHACC7
  | PUSHACC
  | POP
  | ASSIGN
  | ENVACC1
  | ENVACC2
  | ENVACC3
  | ENVACC4
  | ENVACC
  | PUSHENVACC1
  | PUSHENVACC2
  | PUSHENVACC3
  | PUSHENVACC4
  | PUSHENVACC
  | PUSH_RETADDR
  | APPLY
  | APPLY1
  | APPLY2
  | APPLY3
  | APPTERM
  | APPTERM1
  | APPTERM2
  | APPTERM3
  | RETURN
  | RESTART
  | GRAB
  | CLOSURE
  | CLOSUREREC
  | OFFSETCLOSUREM2
  | OFFSETCLOSURE0
  | OFFSETCLOSURE2
  | OFFSETCLOSURE
  | PUSHOFFSETCLOSUREM2
  | PUSHOFFSETCLOSURE0
  | PUSHOFFSETCLOSURE2
  | PUSHOFFSETCLOSURE
  | GETGLOBAL
  | PUSHGETGLOBAL
  | GETGLOBALFIELD
  | PUSHGETGLOBALFIELD
  | SETGLOBAL
  | ATOM0
  | ATOM
  | PUSHATOM0
  | PUSHATOM
  | MAKEBLOCK
  | MAKEBLOCK1
  | MAKEBLOCK2
  | MAKEBLOCK3
  | MAKEFLOATBLOCK
  | GETFIELD0
  | GETFIELD1
  | GETFIELD2
  | GETFIELD3
  | GETFIELD
  | GETFLOATFIELD
  | SETFIELD0
  | SETFIELD1
  | SETFIELD2
  | SETFIELD3
  | SETFIELD
  | SETFLOATFIELD
  | VECTLENGTH
  | GETVECTITEM
  | SETVECTITEM
  | GETSTRINGCHAR
  | SETSTRINGCHAR
  | BRANCH
  | BRANCHIF
  | BRANCHIFNOT
  | SWITCH
  | BOOLNOT
  | PUSHTRAP
  | POPTRAP
  | RAISE
  | CHECK_SIGNALS
  | C_CALL1
  | C_CALL2
  | C_CALL3
  | C_CALL4
  | C_CALL5
  | C_CALLN
  | CONST0
  | CONST1
  | CONST2
  | CONST3
  | CONSTINT
  | PUSHCONST0
  | PUSHCONST1
  | PUSHCONST2
  | PUSHCONST3
  | PUSHCONSTINT
  | NEGINT
  | ADDINT
  | SUBINT
  | MULINT
  | DIVINT
  | MODINT
  | ANDINT
  | ORINT
  | XORINT
  | LSLINT
  | LSRINT
  | ASRINT
  | EQ
  | NEQ
  | LTINT
  | LEINT
  | GTINT
  | GEINT
  | OFFSETINT
  | OFFSETREF
  | ISINT
  | GETMETHOD
  | BEQ
  | BNEQ
  | BLTINT
  | BLEINT
  | BGTINT
  | BGEINT
  | ULTINT
  | UGEINT
  | BULTINT
  | BUGEINT
  | GETPUBMET
  | GETDYNMET
  | STOP
  | EVENT
  | BREAK
  | RERAISE
  | RAISE_NOTRACE
  | FIRST_UNIMPLEMENTED_OP


type kind =
  | KNullary
  | KUnary
  | KBinary
  | KJump
  | KCond_jump
  | KCmp_jump
  | KSwitch
  | KClosurerec
  | KClosure
  | KNullaryCall
  | KUnaryCall
  | KBinaryCall
  | KStop of int
  | K_will_not_happen

type desc = { code : t; kind : kind; name : string; opcode : int }

let ops,ops_rev =
  let ops_rev = Hashtbl.create 17 in
  let if_v4 =
    match Util.Version.v with
      `V3    -> (fun _ default -> default)
    | `V4_02 -> (fun k _ -> k)
    | `V4_03 -> (fun k _ -> k)
    | `V4_04 -> (fun k _ -> k)
  in
  let instrs =
    [| ACC0, KNullary, "ACC0";
       ACC1, KNullary, "ACC1";
       ACC2, KNullary, "ACC2";
       ACC3, KNullary, "ACC3";
       ACC4, KNullary, "ACC4";
       ACC5, KNullary, "ACC5";
       ACC6, KNullary, "ACC6";
       ACC7, KNullary, "ACC7";
       ACC, KUnary, "ACC";
       PUSH, KNullary, "PUSH";
       PUSHACC0, KNullary, "PUSHACC0";
       PUSHACC1, KNullary, "PUSHACC1";
       PUSHACC2, KNullary, "PUSHACC2";
       PUSHACC3, KNullary, "PUSHACC3";
       PUSHACC4, KNullary, "PUSHACC4";
       PUSHACC5, KNullary, "PUSHACC5";
       PUSHACC6, KNullary, "PUSHACC6";
       PUSHACC7, KNullary, "PUSHACC7";
       PUSHACC, KUnary, "PUSHACC";
       POP, KUnary, "POP";
       ASSIGN, KUnary, "ASSIGN";
       ENVACC1, KNullary, "ENVACC1";
       ENVACC2, KNullary, "ENVACC2";
       ENVACC3, KNullary, "ENVACC3";
       ENVACC4, KNullary, "ENVACC4";
       ENVACC, KUnary, "ENVACC";
       PUSHENVACC1, KNullary, "PUSHENVACC1";
       PUSHENVACC2, KNullary, "PUSHENVACC2";
       PUSHENVACC3, KNullary, "PUSHENVACC3";
       PUSHENVACC4, KNullary, "PUSHENVACC4";
       PUSHENVACC, KUnary, "PUSHENVACC";
       PUSH_RETADDR, KUnary, "PUSH_RETADDR";
       APPLY, KUnaryCall, "APPLY";
       APPLY1, KNullaryCall, "APPLY1";
       APPLY2, KNullaryCall, "APPLY2";
       APPLY3, KNullaryCall, "APPLY3";
       APPTERM, KStop 2, "APPTERM";
       APPTERM1, KStop 1, "APPTERM1";
       APPTERM2, KStop 1, "APPTERM2";
       APPTERM3, KStop 1, "APPTERM3";
       RETURN, KStop 1, "RETURN";
       RESTART, KNullary, "RESTART";
       GRAB, KUnary, "GRAB";
       CLOSURE, KClosure, "CLOSURE";
       CLOSUREREC, KClosurerec, "CLOSUREREC";
       OFFSETCLOSUREM2, KNullary, "OFFSETCLOSUREM2";
       OFFSETCLOSURE0, KNullary, "OFFSETCLOSURE0";
       OFFSETCLOSURE2, KNullary, "OFFSETCLOSURE2";
       OFFSETCLOSURE, KUnary, "OFFSETCLOSURE";
       PUSHOFFSETCLOSUREM2, KNullary, "PUSHOFFSETCLOSUREM2";
       PUSHOFFSETCLOSURE0, KNullary, "PUSHOFFSETCLOSURE0";
       PUSHOFFSETCLOSURE2, KNullary, "PUSHOFFSETCLOSURE2";
       PUSHOFFSETCLOSURE, KUnary, "PUSHOFFSETCLOSURE";
       GETGLOBAL, KUnary, "GETGLOBAL";
       PUSHGETGLOBAL, KUnary, "PUSHGETGLOBAL";
       GETGLOBALFIELD, KBinary, "GETGLOBALFIELD";
       PUSHGETGLOBALFIELD, KBinary, "PUSHGETGLOBALFIELD";
       SETGLOBAL, KUnary, "SETGLOBAL";
       ATOM0, KNullary, "ATOM0";
       ATOM, KUnary, "ATOM";
       PUSHATOM0, KNullary, "PUSHATOM0";
       PUSHATOM, KUnary, "PUSHATOM";
       MAKEBLOCK, KBinary, "MAKEBLOCK";
       MAKEBLOCK1, KUnary, "MAKEBLOCK1";
       MAKEBLOCK2, KUnary, "MAKEBLOCK2";
       MAKEBLOCK3, KUnary, "MAKEBLOCK3";
       MAKEFLOATBLOCK, KUnary, "MAKEFLOATBLOCK";
       GETFIELD0, KNullary, "GETFIELD0";
       GETFIELD1, KNullary, "GETFIELD1";
       GETFIELD2, KNullary, "GETFIELD2";
       GETFIELD3, KNullary, "GETFIELD3";
       GETFIELD, KUnary, "GETFIELD";
       GETFLOATFIELD, KUnary, "GETFLOATFIELD";
       SETFIELD0, KNullary, "SETFIELD0";
       SETFIELD1, KNullary, "SETFIELD1";
       SETFIELD2, KNullary, "SETFIELD2";
       SETFIELD3, KNullary, "SETFIELD3";
       SETFIELD, KUnary, "SETFIELD";
       SETFLOATFIELD, KUnary, "SETFLOATFIELD";
       VECTLENGTH, KNullary, "VECTLENGTH";
       GETVECTITEM, KNullary, "GETVECTITEM";
       SETVECTITEM, KNullary, "SETVECTITEM";
       GETSTRINGCHAR, KNullary, "GETSTRINGCHAR";
       SETSTRINGCHAR, KNullary, "SETSTRINGCHAR";
       BRANCH, KJump, "BRANCH";
       BRANCHIF, KCond_jump, "BRANCHIF";
       BRANCHIFNOT, KCond_jump, "BRANCHIFNOT";
       SWITCH, KSwitch, "SWITCH";
       BOOLNOT, KNullary, "BOOLNOT";
       PUSHTRAP, KCond_jump, "PUSHTRAP";
       POPTRAP, KNullary, "POPTRAP";
       RAISE, KStop 0, "RAISE";
       CHECK_SIGNALS, KNullary, "CHECK_SIGNALS";
       C_CALL1, KUnaryCall, "C_CALL1";
       C_CALL2, KUnaryCall, "C_CALL2";
       C_CALL3, KUnaryCall, "C_CALL3";
       C_CALL4, KUnaryCall, "C_CALL4";
       C_CALL5, KUnaryCall, "C_CALL5";
       C_CALLN, KBinaryCall, "C_CALLN";
       CONST0, KNullary, "CONST0";
       CONST1, KNullary, "CONST1";
       CONST2, KNullary, "CONST2";
       CONST3, KNullary, "CONST3";
       CONSTINT, KUnary, "CONSTINT";
       PUSHCONST0, KNullary, "PUSHCONST0";
       PUSHCONST1, KNullary, "PUSHCONST1";
       PUSHCONST2, KNullary, "PUSHCONST2";
       PUSHCONST3, KNullary, "PUSHCONST3";
       PUSHCONSTINT, KUnary, "PUSHCONSTINT";
       NEGINT, KNullary, "NEGINT";
       ADDINT, KNullary, "ADDINT";
       SUBINT, KNullary, "SUBINT";
       MULINT, KNullary, "MULINT";
       DIVINT, KNullary, "DIVINT";
       MODINT, KNullary, "MODINT";
       ANDINT, KNullary, "ANDINT";
       ORINT, KNullary, "ORINT";
       XORINT, KNullary, "XORINT";
       LSLINT, KNullary, "LSLINT";
       LSRINT, KNullary, "LSRINT";
       ASRINT, KNullary, "ASRINT";
       EQ, KNullary, "EQ";
       NEQ, KNullary, "NEQ";
       LTINT, KNullary, "LTINT";
       LEINT, KNullary, "LEINT";
       GTINT, KNullary, "GTINT";
       GEINT, KNullary, "GEINT";
       OFFSETINT, KUnary, "OFFSETINT";
       OFFSETREF, KUnary, "OFFSETREF";
       ISINT, KNullary, "ISINT";
       GETMETHOD, KNullary, "GETMETHOD";
       BEQ, KCmp_jump, "BEQ";
       BNEQ, KCmp_jump, "BNEQ";
       BLTINT, KCmp_jump, "BLTINT";
       BLEINT, KCmp_jump, "BLEINT";
       BGTINT, KCmp_jump, "BGTINT";
       BGEINT, KCmp_jump, "BGEINT";
       ULTINT, KNullary, "ULTINT";
       UGEINT, KNullary, "UGEINT";
       BULTINT, KCmp_jump, "BULTINT";
       BUGEINT, KCmp_jump, "BUGEINT";
       GETPUBMET, KBinary, "GETPUBMET";
       GETDYNMET, KNullary, "GETDYNMET";
       STOP, KStop 0, "STOP";
       EVENT, K_will_not_happen, "EVENT";
       BREAK, K_will_not_happen, "BREAK";
       RERAISE, if_v4 (KStop 0) K_will_not_happen, "RERAISE";
       RAISE_NOTRACE, if_v4 (KStop 0) K_will_not_happen, "RAISE_NOTRACE";
       FIRST_UNIMPLEMENTED_OP, K_will_not_happen, "FIRST_UNIMPLEMENTED_OP"|] in
  let ops =
    Array.mapi
      (fun i (c, k, n) ->
         Hashtbl.add ops_rev c i;
         {code = c; kind = k; name = n; opcode = i}
      ) instrs
  in
  ops,ops_rev

let to_int c = Hashtbl.find ops_rev c

let int_to_buf buf i =
  Buffer.add_char buf (Char.chr (i land 0xFF));
  Buffer.add_char buf (Char.chr ((i lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((i lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((i lsr 24) land 0xFF))

let compile b l =
  List.iter (fun i ->
      let i = match i with
        | `C i -> i
        | `I i -> to_int i in
      int_to_buf b i) l

let compile_to_string l =
  let b = Buffer.create 50 in
  compile b l;
  Buffer.contents b

let get code i = Char.code (code.[i])

let getu code pc =
  let i = pc * 4 in
  let b1 = get code i in
  let b2 = get code (i + 1) in
  let b3 = get code (i + 2) in
  let b4 = get code (i + 3) in
  (b4 lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1

let getu32 code pc = Int32.of_int (getu code pc)

let gets code pc =
  let i = pc * 4 in
  let b1 = get code i in
  let b2 = get code (i + 1) in
  let b3 = get code (i + 2) in
  let b4 = get code (i + 3) in
  let b4' = if b4 >= 128 then b4-256 else b4 in
  (b4' lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1

let gets32 code pc = Int32.of_int (gets code pc)

exception Bad_instruction of int

let get_instr code pc =
  let i = getu code pc in
  if i < 0 || i >= Array.length ops then raise (Bad_instruction i);
  let ins = ops.(i) in
  if ins.kind = K_will_not_happen then raise (Bad_instruction i);
  ins

(****)

let same_custom x y =
  Obj.field x 0 == Obj.field (Obj.repr y) 0

let rec print_obj f x =
  if Obj.is_block x then begin
    let tag = Obj.tag x in
    if tag = Obj.string_tag then
        Format.fprintf f "%S" (Obj.magic x : string)
    else if tag = Obj.double_tag then
        Format.fprintf f "%.12g" (Obj.magic x : float)
    else if tag = Obj.double_array_tag then begin
        let a = (Obj.magic x : float array) in
        Format.fprintf f "[|";
        for i = 0 to Array.length a - 1 do
          if i > 0 then Format.fprintf f ", ";
          Format.fprintf f "%.12g" a.(i)
        done;
        Format.fprintf f "|]"
    end else if tag = Obj.custom_tag && same_custom x 0l then
        Format.fprintf f "%ldl" (Obj.magic x : int32)
    else if tag = Obj.custom_tag && same_custom x 0n then
        Format.fprintf f "%ndn" (Obj.magic x : nativeint)
    else if tag = Obj.custom_tag && same_custom x 0L then
        Format.fprintf f "%LdL" (Obj.magic x : int64)
    else if tag < Obj.no_scan_tag then begin
        Format.fprintf f "<%d>" (Obj.tag x);
        match Obj.size x with
          0 -> ()
        | 1 ->
            Format.fprintf f "("; print_obj f (Obj.field x 0);
            Format.fprintf f ")"
        | n ->
            Format.fprintf f "("; print_obj f (Obj.field x 0);
            for i = 1 to n - 1 do
              Format.fprintf f ", "; print_obj f (Obj.field x i)
            done;
            Format.fprintf f ")"
    end else
        Format.fprintf f "<tag %d>" tag
  end else
    Format.fprintf f "%d" (Obj.magic x : int)
