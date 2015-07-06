(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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



%token TProvides TRequires TVersion
%token TA_Pure TA_Const TA_Mutable TA_Mutator TA_Shallow TA_Object_literal
%token<string> TIdent TVNum
%token TComma TSemi EOF EOL LE LT GE GT EQ LPARENT RPARENT
%token<string> TOTHER

%start annot
%type <Jsoo_primitive.t> annot

%%

annot:
  | TProvides TSemi id=TIdent opt=option(prim_annot)
    args=option(delimited(LPARENT, separated_list(TComma,arg_annot),RPARENT))
    endline
    { `Provides (None,id,(match opt with None -> `Mutator | Some k -> k),args) }
  | TRequires TSemi l=separated_nonempty_list(TComma,TIdent) endline
    { `Requires (None,l) }
  | TVersion TSemi l=separated_nonempty_list(TComma,version) endline
    { `Version (None,l) }

prim_annot:
  | TA_Pure {`Pure}
  | TA_Const {`Pure}
  | TA_Mutable {`Mutable}
  | TA_Mutator {`Mutator}

arg_annot:
  | TA_Const { `Const }
  | TA_Shallow { `Shallow_const}
  | TA_Object_literal { `Object_literal}
  | TA_Mutable { `Mutable}

op:
  | LE {(<=)}
  | LT {(<)}
  | GT {(>)}
  | GE {(>=)}
  | EQ {(=)}

version:
  | op TVNum { $1,$2 }

endline:
  | EOL { () }
  | EOF { () }
  | TOTHER { failwith $1  }
