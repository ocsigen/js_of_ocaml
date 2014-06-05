module MakeTo( C : sig type 'a elt val elt : 'a elt -> Dom.node Js.t end) :
  Tyxml_cast_sigs.TO with type 'a elt = 'a C.elt

module MakeOf( C : sig type 'a elt val elt : Dom.node Js.t -> 'a elt end) :
  Tyxml_cast_sigs.OF with type 'a elt = 'a C.elt
