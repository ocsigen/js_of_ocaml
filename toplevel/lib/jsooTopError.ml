exception Camlp4 of (Location.t * exn)
let loc = function
    | Syntaxerr.Error(x) -> Some(Syntaxerr.location_of_error x)
    | Lexer.Error(_, loc)
    | Typecore.Error(loc, _, _)
    | Typetexp.Error(loc, _, _)
    | Typeclass.Error(loc, _, _)
    | Typemod.Error(loc, _, _)
    | Typedecl.Error(loc, _)
    | Translcore.Error(loc, _)
    | Translclass.Error(loc, _)
    | Translmod.Error(loc, _)
    | Camlp4 (loc, _) -> Some loc
    | _ -> None
