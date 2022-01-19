module Ast = Flow_ast

let options : Parser_env.parse_options =
{
  enums = false;
  esproposal_decorators = false;
  esproposal_export_star_as = false;
  types = false;
  use_strict = true
}

let program f =
  let ic = open_in_bin f in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  Parser_flow.parse_program true (Some (File_key.SourceFile f)) content


let expression content =
  let env = Parser_env.init_env ?token_sink:None ?parse_options:None None content in
  Parser_flow.parse_expression env true
