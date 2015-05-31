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

//Provides: caml_CamlinternalMod_init_mod
//Requires: caml_raise_with_arg, caml_global_data
function caml_CamlinternalMod_init_mod(loc,shape) {
  function undef_module (_x) {
    caml_raise_with_arg(caml_global_data.Undefined_recursive_module, loc);
  }
  function loop (shape,struct,idx){
    if(typeof shape === "number")
      switch(shape){
      case 0://function
        struct[idx]={fun:undef_module};
        break;
      case 1://lazy
        struct[idx]=[246, undef_module];
        break;
      default://case 2://class
        struct[idx]=[];
      }
    else
      switch(shape[0]){
      case 0://module
        struct[idx] = [0];
        for(var i=1;i<shape[1].length;i++)
          loop(shape[1][i],struct[idx],i);
        break;
      default://case 1://Value
        struct[idx] = shape[1];
      }
  }
  var res = [];
  loop(shape,res,0);
  return res[0]
}
//Provides: caml_CamlinternalMod_update_mod
//Requires: caml_update_dummy
function caml_CamlinternalMod_update_mod(shape,real,x) {
  if(typeof shape === "number")
    switch(shape){
    case 0://function
      real.fun = x;
      break;
    case 1://lazy
    default://case 2://class
      caml_update_dummy(real,x);
    }
  else
    switch(shape[0]){
    case 0://module
      for(var i=1;i<shape[1].length;i++)
        caml_CamlinternalMod_update_mod(shape[1][i],real[i],x[i]);
      break;
    //case 1://Value
    default:
    };
  return 0
}
