module Arg = Arg
open StdLabels

module List = struct
  include List

  let is_empty = function
    | [] -> true
    | _ -> false
end

let normalize_argv ?(warn = fun _ -> ()) a =
  let bad = ref [] in
  let a =
    Array.map
      ~f:(fun s ->
        let size = String.length s in
        if size <= 2
        then s
        else if Char.equal s.[0] '-'
                && (not (Char.equal s.[1] '-'))
                && not (Char.equal s.[2] '=')
        then (
          bad := s :: !bad;
          (* long option with one dash lets double the dash *)
          "-" ^ s)
        else s)
      a
  in
  if not (List.is_empty !bad)
  then
    warn
      (Format.sprintf
         "[Warning] long options with a single '-' are now deprecated. Please use '--' \
          for the following options: %s@."
         (String.concat ~sep:", " !bad));
  a
