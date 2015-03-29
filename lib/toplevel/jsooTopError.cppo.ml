exception Camlp4 of (Location.t * exn)
let loc = function 
#if OCAML_VERSION > (4,0,0)
    | Syntaxerr.Error(x) -> Some(Syntaxerr.location_of_error x)
#endif
    | Lexer.Error(_, loc)
#if OCAML_VERSION < (4,1,0)
    | Typecore.Error(loc, _)
    | Typetexp.Error(loc, _) 
    | Typeclass.Error(loc, _) 
    | Typemod.Error(loc, _) 
#else
    | Typecore.Error(loc, _, _)
    | Typetexp.Error(loc, _, _) 
    | Typeclass.Error(loc, _, _) 
    | Typemod.Error(loc, _, _) 
#endif
    | Typedecl.Error(loc, _) 
    | Translcore.Error(loc, _) 
    | Translclass.Error(loc, _) 
    | Translmod.Error(loc, _)
    | Camlp4 (loc, _) -> Some loc
    | _ -> None
