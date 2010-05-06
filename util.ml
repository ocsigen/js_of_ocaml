
module Int = struct type t = int let compare (x : int) y = compare x y end
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module StringSet = Set.Make (String)

let opt_map f x = match x with None -> None | Some v -> Some (f v)
let opt_iter f x = match x with None -> () | Some v -> f v
let opt_bind x f = match x with None -> None | Some v -> f v
let opt_filter p x =
  match x with None -> None | Some v -> if p v then Some v else None
