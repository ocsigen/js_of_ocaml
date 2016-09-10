(** Ppx syntax extension for Js_of_ocaml.

    To migrate from the camlp4 extension, it is advised to use
    {{:https://github.com/janestreet/camlp4-to-ppx}camlp4-to-ppx},
    which provides built-in support for Js_of_ocaml syntax.
*)

val js_mapper : string list -> Ast_mapper.mapper
(**
   A syntax extension is available for manipulating object properties,
   invoking methods and creating objects.  The syntax and typing rules
   are as follows:

   {ul
   {- Getting a property
   {[
obj : <m : u Js.prop> Js.t
--------------------------
     obj##.m : u
   ]}
   }
   {- Setting a property
   {[
obj : <m : u Js.prop> Js.t
  e : u
--------------------------
  obj##.m := e : unit
   ]}
   }
   {- Invoking a method
   {[
obj : <m : t_1 -> ... -> t_n -> u Js.meth; ..> Js.t
e_i : t_i               (1 <= i <= n)
----------------------------------------------------
        obj##m e_1 ... e_n : u
   ]}
   For easier chaining, the following alternative syntax is also possible:
   [ obj##(m e_1 ... e_n) ].

   Beware, partial application is not allowed.
   }
   {- Using an object constructor
   {[
constr : (t_1 -> ... -> t_n -> u Js.t) Js.constr
   e_i : t_i               (1 <= i <= n)
------------------------------------------------
          new%js constr e1 ... en : u Js.t
   ]}
   [ constr ] here must be an identifier. For constructors
   that are not identifiers, bind them first:
   {[
let a = Js.Unsafe.global##.A in
new%js a
   ]}
   }
   {- Creating a literal object
   {[
object%js (self) (* Equivalent of this *)
  val x = 3 (* read-only prop *)
  val mutable y = 4 (* read/write prop *)
  method foo i = self##.y := self##.x + i
end
   ]}
   Properties are defined with the [val] keyword. [mutable] makes the
   property writable. [self] can be any identifier and will be bind
   to [this] in javascript.

   In this case, the object has the following type:
   {[
< foo : int -> unit Js.meth;
    x : int Js.readonly_prop;
    y : int Js.prop
> Js.t
   ]}
   }
   }

*)
