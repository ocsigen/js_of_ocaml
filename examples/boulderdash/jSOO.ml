(* Java Script Objects Operations *)

type obj

type value =
  | Obj of obj
  | Num of float
  | String of string
  | Block of Obj.t
  | Nil

(* obtain objects *)
external new_obj : obj -> obj = "jsoo_new"
external eval : string -> obj = "jsoo_eval"
external inject : value -> obj = "jsoo_inject"
external extract : obj -> value = "jsoo_extract"
let null = inject Nil
let string s = inject (String s)
let float f = inject (Num f)
let int i = inject (Num (float_of_int i))

(* set fields *)
external get : string -> obj -> obj = "jsoo_get"
external set : string -> obj -> obj -> unit = "jsoo_set"
let unset field obj = set field obj null

let (>>>) x f = f x

(* call JS functions *)
external call : obj -> obj array -> obj -> obj = "jsoo_call"
let call_method field args dest =
  let meth = get field dest in
    call dest args meth
let call_function args f =
  call null args f

(* build JS event handlers from caml closures *)
external wrap_event : (unit -> unit) -> obj = "jsoo_wrap_event"
external get_event_arg : unit -> obj = "jsoo_get_event_args"
let wrap_event f =
  wrap_event
    (fun () ->
(*       try*)
	 f (get_event_arg ()) ;
(*	 Thread.exit ();
       with e ->
	 Thread.thread_uncaught_exception e;
	 Thread.exit ()*)
    )

let as_string x = match extract x with String s -> s | _ -> failwith "as_string"
let as_obj x = match extract x with Obj o -> o | _ -> failwith "as_obj"
let as_int x = match extract x with Num f -> int_of_float f | _ -> failwith "as_int"
let as_float x = match extract x with Num f -> f | _ -> failwith "as_float"
let as_block x = match extract x with Block b -> b | _ -> failwith "as_block"
