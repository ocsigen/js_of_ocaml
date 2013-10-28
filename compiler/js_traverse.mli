

class type mapper = object
  method expression : Javascript.expression -> Javascript.expression
  method expression_o : Javascript.expression option -> Javascript.expression option
  method statement : Javascript.statement -> Javascript.statement
  method statement_o : Javascript.statement option -> Javascript.statement option
  method source : Javascript.source_element -> Javascript.source_element
  method ident : Javascript.ident -> Javascript.ident
  method program : Javascript.program -> Javascript.program
end

class map : mapper

class subst : (Javascript.ident -> Javascript.ident) ->  object
    inherit mapper
  end

open Util

type t = {
  use_name : StringSet.t;
  def_name : StringSet.t;
  def : Code.VarSet.t;
  use : Code.VarSet.t;
}


class type freevar =
  object('a)
    inherit mapper
    method merge_info : 'a -> unit
    method block : Javascript.ident list -> unit

    method def_var : Javascript.ident -> unit
    method use_var : Javascript.ident -> unit
    method state : t
    method get_free_name : Util.StringSet.t
    method get_free : Code.VarSet.t
  end

class free : freevar

class rename_str : freevar

class compact_vardecl : object('a)
  inherit free
  method exc  : Javascript.IdentSet.t
end
