type t = int * string [@@deriving json]

type v =
  | A
  | B of int
[@@deriving json]

type p =
  [ `C
  | `D of int
  ]
[@@deriving json]

type a = int

and b = string [@@deriving json]

type tree =
  | Leaf
  | Node of tree * tree
[@@deriving json]

let roundtrip json v = Deriving_Json.from_string json (Deriving_Json.to_string json v)

let () =
  assert (roundtrip json (1, "a") = (1, "a"));
  assert (roundtrip v_json (B 2) = B 2);
  assert (roundtrip p_json (`D 3) = `D 3);
  assert (roundtrip a_json 4 = 4);
  assert (roundtrip b_json "e" = "e");
  let n = Node (Leaf, Node (Leaf, Leaf)) in
  assert (roundtrip tree_json n = n)
