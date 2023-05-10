#!/usr/bin/env -S node --experimental-wasm-stringref  --experimental-wasm-gc
(async function () {
    const fs = require('fs/promises');
    const path = require('path');
    const runtimePath =
          path.resolve(path.dirname(process.argv[1]), 'runtime.wasm');
    const runtime = fs.readFile(runtimePath);
    const code = fs.readFile(process.argv[2]);

    var caml_callback;

    let math =
        {cos:Math.cos, sin:Math.sin, asin:Math.asin, atan2:Math.atan2,
         pow:Math.pow, fmod:(x, y) => x%y,
         log:(x)=>console.log('ZZZZZ', x)}
    let bindings =
        {identity:(x)=>x,
         from_bool:(x)=>!!x,
         get:(x,y)=>x[y],
         set:(x,y,z)=>x[y]=z,
         eval:eval,
         strict_equals:(x,y)=>x===y,
         fun_call:(f,args)=>f.apply(null,args),
         meth_call:(o,f,args)=>o[f].apply(o,args),
         new_array:(n)=>new Array(n),
         new_obj:()=>({}),
         new:(c,args)=>{return new c(...args)},
         array_length:(a)=>a.length,
         array_get:(a,i)=>a[i],
         array_set:(a,i,v)=>a[i]=v,
         get_int:(a,i)=>a[i],
         wrap_callback_strict:(arity,f)=>function (){
             var n = arguments.length;
             var args = new Array(arity);
             var len = Math.min(arguments.length, arity)
             for (var i = 0; i < len; i++) args[i] = arguments[i];
             return caml_callback(f, arity, args);
         },
         format:(f)=>""+f
        }
    const runtimeModule =
          await WebAssembly.instantiate(await runtime,
                                        {Math:math,bindings:bindings});

    caml_callback = runtimeModule.instance.exports.caml_callback;

    const wasmModule =
          await WebAssembly.instantiate(await code,
                                        {env:runtimeModule.instance.exports,
                                         Math:math})
    try {
        wasmModule.instance.exports._initialize()
    } catch (e) {
        if (e instanceof WebAssembly.Exception &&
            e.is(runtimeModule.instance.exports.ocaml_exit))
            process.exit(e.getArg(runtimeModule.instance.exports.ocaml_exit, 0));
        if (e instanceof WebAssembly.Exception &&
            e.is(runtimeModule.instance.exports.ocaml_exception)) {
            console.log('Uncaught exception')
            process.exit(1)
        }
        throw e;
    }

})()
