



//Provides: caml_CamlinternalMod_init_mod
function caml_CamlinternalMod_init_mod(loc,shape) {
    function loop (shape,struct,idx){
        switch(shape){
        case 0://function
            struct[idx]={};//fun:null
            return;
        case 1://lazy
        case 2://class
            struct[idx]=null;
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
function caml_CamlinternalMod_update_mod(shape,real,x) {
    function loop (shape,real,x,idx){
        switch(shape){
        case 0://function
            var tmp = x[idx];
            while(tmp.fun)
                tmp = tmp.fun;
            real[idx].fun = tmp;
            break;
        case 1:
        case 2:
            real[idx]=x[idx];
            break;
        default:
            for(var i=1;i<shape[1].length;i++)
                loop(shape[1][i],real[idx],x[idx],i)
        }
    }
    loop(shape,[real],[x],0)
}
