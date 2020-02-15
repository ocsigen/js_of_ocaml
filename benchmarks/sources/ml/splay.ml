(*
// Copyright 2009 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// This benchmark is based on a JavaScript log processing module used
// by the V8 profiler to generate execution time profiles for runs of
// JavaScript applications, and it effectively measures how fast the
// JavaScript engine is at allocating nodes and reclaiming the memory
// used for old nodes. Because of the way splay trees work, the engine
// also has to deal with a lot of changes to the large tree object
// graph.
*)
(* Translation in ocaml by VB:
   This program is probably not the best splay tree implementation in OCaml,
   because it tries to follow exactly the steps of the Google v8 benchmark.
*)

let kSplayTreeSize = 8000

let kSplayTreeModifications = 80

let kSplayTreePayloadDepth = 5

type content_leaf =
  { array : int array
  ; string : string
  }

type content =
  | CLeaf of content_leaf
  | CNode of content * content

type tree =
  | Empty
  | Node of (tree * float * content * tree)

(**
 * Perform the splay operation for the given key. Moves the node with
 * the given key to the top of the tree.  If no node has the given
 * key, the last node on the search path is moved to the top of the
 * tree. This is the simplified top-down splaying algorithm from:
 * "Self-adjusting Binary Search Trees" by Sleator and Tarjan
*)
let rec splay_ ((left, key, value, right) as a) k =
  if k = key
  then a
  else if k < key
  then
    match left with
    | Empty -> a (* not found *)
    | Node (lleft, lk, lv, lright) -> (
        if k = lk
        then lleft, lk, lv, Node (lright, key, value, right) (* zig *)
        else if k < lk
        then
          match lleft with
          | Empty -> Empty, lk, lv, Node (lright, key, value, right)
          (* not found *)
          | Node n ->
              (* zig-zig *)
              let llleft, llk, llv, llright = splay_ n k in
              llleft, llk, llv, Node (llright, lk, lv, Node (lright, key, value, right))
        else
          match lright with
          | Empty -> lleft, lk, lv, Node (Empty, key, value, right)
          | Node n ->
              (* zig-zag *)
              let lrleft, lrk, lrv, lrright = splay_ n k in
              Node (lleft, lk, lv, lrleft), lrk, lrv, Node (lrright, key, value, right))
  else
    match right with
    | Empty -> a
    | Node (rleft, rk, rv, rright) -> (
        if k = rk
        then Node (left, key, value, rleft), rk, rv, rright (* zag *)
        else if k > rk
        then
          match rright with
          | Empty -> Node (left, key, value, rleft), rk, rv, rright (* not found *)
          | Node n ->
              (* zag-zag *)
              let rrleft, rrk, rrv, rrright = splay_ n k in
              Node (Node (left, key, value, rleft), rk, rv, rrleft), rrk, rrv, rrright
        else
          match rleft with
          | Empty -> Node (left, key, value, rleft), rk, rv, rright (* not found *)
          | Node n ->
              (* zag-zig *)
              let rlleft, rlk, rlv, rlright = splay_ n k in
              Node (left, key, value, rlleft), rlk, rlv, Node (rlright, rk, rv, rright))

let splay t key =
  match t with
  | Empty -> t
  | Node n -> Node (splay_ n key)

let insert key value t =
  (* Splay on the key to move the last node on the search path for
     the key to the root of the tree. *)
  let t = splay t key in
  match t with
  | Empty -> Node (Empty, key, value, Empty)
  | Node (left, rk, rv, right) ->
      if rk = key
      then t
      else if key > rk
      then Node (Node (left, rk, rv, Empty), key, value, right)
      else Node (left, key, value, Node (Empty, rk, rv, right))

let remove key t =
  let t = splay t key in
  match t with
  | Empty -> t
  | Node (_, rk, _, _) when rk <> key -> raise Not_found
  | Node (Empty, _, _, right) -> right
  | Node (left, _, _, right) -> (
      match splay left key with
      | Node (lleft, lk, lv, Empty) -> Node (lleft, lk, lv, right)
      | _ -> failwith "remove")

let find key t =
  let t = splay t key in
  match t with
  | Node (_, k, v, _) when k = key -> Some v, t
  | _ -> None, t

let rec findMax = function
  (* here we do not splay (but that's what the original program does) *)
  | Empty -> raise Not_found
  | Node (_, k, _, Empty) -> k
  | Node (_, _, _, right) -> findMax right

let findGreatestLessThan key t =
  (* Splay on the key to move the node with the given key or the last
     node on the search path to the top of the tree.
     Now the result is either the root node or the greatest node in
     the left subtree. *)
  let t = splay t key in
  match t with
  | Empty -> None, t
  | Node (_, k, _, _) when k < key -> Some k, t
  | Node (Empty, _, _, _) -> None, t
  | Node (left, _, _, _) -> Some (findMax left), t

let exportKeys t =
  let rec aux l length = function
    | Empty -> l, length
    | Node (left, k, _, right) ->
        let l, length = aux l length right in
        aux (k :: l) (length + 1) left
  in
  aux [] 0 t

let rec generatePayloadTree depth tag =
  if depth = 0
  then
    CLeaf
      { array = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]
      ; string = "String for key " ^ tag ^ " in leaf node"
      }
  else CNode (generatePayloadTree (depth - 1) tag, generatePayloadTree (depth - 1) tag)

let random =
  let seed = ref 49734321 in
  fun () ->
    (*    // Robert Jenkins' 32 bit integer hash function. *)
    let s = !seed in
    let s = (s + 0x7ed55d16 + (s lsl 12)) land 0xffffffff in
    let s = s lxor 0xc761c23c lxor (s lsr 19) in
    let s = s + 0x165667b1 + (s lsl 5) in
    let s = (s + 0xd3a2646c) lxor (s lsl 9) in
    let s = (s + 0xfd7046c5 + (s lsl 3)) land 0xffffffff in
    let s = s lxor 0xb55a4f09 lxor (s lsr 16) in
    seed := s;
    float (s land 0xfffffff) /. float 0x10000000

let generateKey = random

let insertNewNode t =
  let rec aux t =
    let key = generateKey () in
    let vo, t = find key t in
    match vo with
    | None -> key, t
    | _ -> aux t
  in
  let key, t = aux t in
  let payload = generatePayloadTree kSplayTreePayloadDepth (string_of_float key) in
  key, insert key payload t

let splaySetup () =
  let rec aux i t =
    if i < kSplayTreeSize then aux (i + 1) (snd (insertNewNode t)) else t
  in
  aux 0 Empty

let splayTearDown t =
  let keys, length = exportKeys t in
  (*  // Verify that the splay tree has the right size. *)
  if length <> kSplayTreeSize then failwith "Splay tree has wrong size";
  (*  // Verify that the splay tree has sorted, unique keys. *)
  match keys with
  | [] -> ()
  | a :: l ->
      ignore
        (List.fold_left
           (fun b e -> if b >= e then failwith "Splay tree not sorted" else e)
           a
           l)

let splayRun t =
  (*  // Replace a few nodes in the splay tree. *)
  let rec aux i t =
    if i < kSplayTreeModifications
    then
      let key, t = insertNewNode t in
      aux
        (i + 1)
        (match findGreatestLessThan key t with
        | None, t -> remove key t
        | Some k, t -> remove k t)
    else t
  in
  aux 0 t

let ( ++ ) a b = b a

let () = splaySetup () ++ splayRun ++ splayTearDown
