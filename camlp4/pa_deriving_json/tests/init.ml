Sys.interactive := false;;
#use "topfind";;
#directory "../../lib/deriving_json/"
#load "deriving_Json_lexer.cmo";;
#load "deriving_Json.cmo";;
#camlp4o;;
#require "deriving.syntax.std"
#load "lib/pa_deriving_Json.cmo";;
Sys.interactive := true;;
