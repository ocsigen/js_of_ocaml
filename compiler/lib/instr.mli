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
  | OFFSETCLOSUREM3
  | OFFSETCLOSURE0
  | OFFSETCLOSURE3
  | OFFSETCLOSURE
  | PUSHOFFSETCLOSUREM3
  | PUSHOFFSETCLOSURE0
  | PUSHOFFSETCLOSURE3
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
  | GETBYTESCHAR
  | SETBYTESCHAR
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
  | GETSTRINGCHAR
  | PERFORM
  | RESUME
  | RESUMETERM
  | REPERFORMTERM
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

type desc =
  { code : t
  ; kind : kind
  ; name : string
  ; opcode : int
  }

val find : t -> desc

val get_instr_exn : string -> int -> desc

val gets : string -> int -> int

val getu : string -> int -> int

val gets32 : string -> int -> int32

val getu32 : string -> int -> int32
