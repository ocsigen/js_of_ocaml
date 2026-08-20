include Sizable_int.Make (struct
  let name = "Targetint"

  let allowed_sizes = [| 63; 32; 31 |]
end)

let max_i31s = lazy (of_int64_exn (Int64.sub (Int64.shift_left 1L 30) 1L) (* 2^30 -1 *))

let min_i31s = lazy (of_int64_exn (Int64.neg (Int64.shift_left 1L 30)) (* -2^30 *))

let is_within_i31s x =
  Stdlib.( >= ) (compare x (Lazy.force min_i31s)) 0
  && Stdlib.( <= ) (compare x (Lazy.force max_i31s)) 0
