open Common

let log_stop = log_start "CSS.Colors test suite"

let () =
  let cols = [
    CSS.Color.RGB(120, 3, 56);
    CSS.Color.RGBA(120, 3, 56,1.);
    CSS.Color.RGB_percent(10, 3,60);
    CSS.Color.RGBA_percent(100,53,60,0.45);
    CSS.Color.HSL(120,75,56);
    CSS.Color.HSLA(180, 3,56,0.2);
    CSS.Color.RGB (CSS.Color.rgb_of_name CSS.Color.Dodgerblue);
    CSS.Color.RGB (CSS.Color.rgb_of_name CSS.Color.Pink);
    CSS.Color.Name CSS.Color.Hotpink;
    CSS.Color.Name CSS.Color.Cornsilk;
  ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Color.js c  in
        let ml = CSS.Color.ml js in
        if c = ml then
          log_success ()
        else
          log_failure (Printf.sprintf "%s   %s"
            (CSS.Color.string_of_t c)
            (CSS.Color.string_of_t ml)
            )
      with
        | Invalid_argument s -> log_failure s
        | Failure s -> log_failure s
    )
    cols

let () = log_stop ()
