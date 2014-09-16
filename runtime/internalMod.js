



//Provides: caml_CamlinternalMod_init_mod
//Requires: caml_raise_with_arg, caml_global_data
function caml_CamlinternalMod_init_mod(loc,shape) {
  function undef_module (_x) {
    caml_raise_with_arg(caml_global_data[12], loc);
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
    case 2://class
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
      caml_update_dummy(real,x);
    };
  return 0
}
