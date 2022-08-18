open Js_of_ocaml
open Js_of_ocaml_wscript

type obj
type variant

class coClassInfo js = object
  method name : string = Js.to_string (Js.Unsafe.get js "Name")
end

class coClasses js = object
  method count : int = Js.Unsafe.get js "Count"
  method item (i : int) : coClassInfo option =
    Js.Opt.case
      (Js.Unsafe.meth_call js "Item" [|Js.Unsafe.inject i|])
      (fun () -> None)
      (fun x -> Some (new coClassInfo x))
end

class typeLibInfo js = object
  method coClasses : coClasses option =
    Js.Opt.case
      (Js.Unsafe.get js "CoClasses")
      (fun () -> None)
      (fun x -> Some (new coClasses x))
  method setContainingFile (x : string) : unit = Js.Unsafe.set js "ContainingFile" (Js.string x)
  method name : string = Js.to_string (Js.Unsafe.get js "Name")
end

let typeLibInfo () = new typeLibInfo (Js.Unsafe.new_obj Wscript.obj [|Js.(Unsafe.inject (string "TLI.TypeLibInfo"))|])
