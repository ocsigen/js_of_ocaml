
# Module `Js_of_ocaml_compiler.Instr`

```ocaml
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
  | MAKE_FAUX_MIXEDBLOCK
  | WITH_STACK
  | WITH_STACK_BIND
  | FIRST_UNIMPLEMENTED_OP
```
```ocaml
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
```
```ocaml
type desc = {
  code : t;
  kind : kind;
  name : string;
  opcode : int;
}
```
```ocaml
val find : t -> desc
```
```ocaml
val get_instr_exn : string -> int -> desc
```
```ocaml
val gets : string -> int -> int
```
```ocaml
val getu : string -> int -> int
```
```ocaml
val gets32 : string -> int -> int32
```
```ocaml
val getu32 : string -> int -> int32
```