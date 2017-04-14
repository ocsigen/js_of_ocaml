
module Css_angle = Css_angle
module Css_color = Css_color
module Css_length = Css_length
module Json_convert = Json_convert
module Regexp1 = Regexp1
module Time = Time
module Url1 = Url1

let _ =
  Firebug.console##log(
    Js.string (
      Printf.sprintf "Test results: %d successes out of %d tests"
        !Common.success_count !Common.test_count
    )
  );
  if !Common.success_count <> !Common.test_count
  then exit 1
  else exit 0
