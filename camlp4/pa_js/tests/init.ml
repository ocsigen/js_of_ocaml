Sys.interactive := false;;
#use "topfind";;
#camlp4o;;
#load "lib/pa_js.cmo";;
Sys.interactive := true;;

module Js = struct
  type +'a t
  type (-'a, +'b) meth_callback
  type 'a opt = 'a
  type 'a optdef = 'a

  type +'a meth
  type +'a gen_prop
  type 'a readonly_prop = <get : 'a> gen_prop
  type 'a writeonly_prop = <set : 'a -> unit> gen_prop
  type 'a prop = <get : 'a; set : 'a -> unit> gen_prop
  type 'a optdef_prop = <get : 'a optdef; set : 'a -> unit> gen_prop

  type +'a constr

  (****)

  type 'a callback = (unit, 'a) meth_callback
  module Unsafe = struct
    type any
    type any_js_array = any

    let inject : 'a -> any =
      fun _ -> assert false

    let get : 'a -> 'b -> 'c = fun _ _ -> assert false
    let set : 'a -> 'b -> 'c -> unit = fun _ _ _ -> assert false

    let meth_call : 'a -> string -> any array -> 'b =
      fun _ _ _ -> assert false

    let obj : (string * any) array -> 'a =
      fun _ -> assert false

  end
  let wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback =
    fun _ -> assert false

end;;
let () = flush_all ()
