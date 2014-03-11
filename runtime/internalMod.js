



//Provides: caml_CamlinternalMod_init_mod
function caml_CamlinternalMod_init_mod(loc,shape) {
    function loop (shape,struct,idx){
        switch(shape){
        case 0://function
            struct[idx]={};//fun:null
            return;
        case 1://lazy
        case 2://class
            struct[idx]=[];
            break;
        default://module
            struct[0] = [0];
            for(var i=1;i<shape[1].length;i++)
                loop(shape[1][i],struct[0],i)
        }
    }
    var res = [];
    loop(shape,res,0);
    return res[0]
}
//Provides: caml_CamlinternalMod_update_mod
//Requires: caml_update_dummy
function caml_CamlinternalMod_update_mod(shape,real,x) {
  switch(shape){
  case 0://function
    real.fun = x;
    break;
  case 1://lazy
  case 2://class
    caml_update_dummy(real,x);
    break;
  default:
    for(var i=1;i<shape[1].length;i++)
      caml_CamlinternalMod_update_mod(shape[1][i],real[i],x[i])
  }
}
