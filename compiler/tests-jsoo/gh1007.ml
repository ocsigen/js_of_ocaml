module MyList : sig
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
end = struct
  let rec length_aux len = function
    | [] -> len
    | _ :: l -> length_aux (len + 1) l

  let length l = length_aux 0 l

  let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | a :: l -> rev_append l (a :: l2)

  let stable_sort cmp l =
    let rec rev_merge l1 l2 accu =
      match l1, l2 with
      | [], l2 -> rev_append l2 accu
      | l1, [] -> rev_append l1 accu
      | h1 :: t1, h2 :: t2 ->
          if cmp h1 h2 <= 0
          then rev_merge t1 l2 (h1 :: accu)
          else rev_merge l1 t2 (h2 :: accu)
    in
    let rec rev_merge_rev l1 l2 accu =
      match l1, l2 with
      | [], l2 -> rev_append l2 accu
      | l1, [] -> rev_append l1 accu
      | h1 :: t1, h2 :: t2 ->
          if cmp h1 h2 > 0
          then rev_merge_rev t1 l2 (h1 :: accu)
          else rev_merge_rev l1 t2 (h2 :: accu)
    in
    let rec sort n l =
      match n, l with
      | 2, x1 :: x2 :: tl ->
          let s = if cmp x1 x2 <= 0 then [ x1; x2 ] else [ x2; x1 ] in
          s, tl
      | 3, x1 :: x2 :: x3 :: tl ->
          let s =
            if cmp x1 x2 <= 0
            then
              if cmp x2 x3 <= 0
              then [ x1; x2; x3 ]
              else if cmp x1 x3 <= 0
              then [ x1; x3; x2 ]
              else [ x3; x1; x2 ]
            else if cmp x1 x3 <= 0
            then [ x2; x1; x3 ]
            else if cmp x2 x3 <= 0
            then [ x2; x3; x1 ]
            else [ x3; x2; x1 ]
          in
          s, tl
      | n, l ->
          let n1 = n asr 1 in
          let n2 = n - n1 in
          let s1, l2 = rev_sort n1 l in
          let s2, tl = rev_sort n2 l2 in
          rev_merge_rev s1 s2 [], tl
    and rev_sort n l =
      match n, l with
      | 2, x1 :: x2 :: tl ->
          let s = if cmp x1 x2 > 0 then [ x1; x2 ] else [ x2; x1 ] in
          s, tl
      | 3, x1 :: x2 :: x3 :: tl ->
          let s =
            if cmp x1 x2 > 0
            then
              if cmp x2 x3 > 0
              then [ x1; x2; x3 ]
              else if cmp x1 x3 > 0
              then [ x1; x3; x2 ]
              else [ x3; x1; x2 ]
            else if cmp x1 x3 > 0
            then [ x2; x1; x3 ]
            else if cmp x2 x3 > 0
            then [ x2; x3; x1 ]
            else [ x3; x2; x1 ]
          in
          s, tl
      | n, l ->
          let n1 = n asr 1 in
          let n2 = n - n1 in
          let s1, l2 = sort n1 l in
          let s2, tl = sort n2 l2 in
          rev_merge s1 s2 [], tl
    in
    let len = length l in
    if len < 2 then l else fst (sort len l)
end

type t =
  | Empty
  | Node of t

let rec f x =
  match x with
  | Empty -> ()
  | Node next ->
      let _ = MyList.stable_sort compare [ 1; 2; 3; 4 ] in
      f next

let%expect_test _ =
  f (Node Empty);
  [%expect{||}]
