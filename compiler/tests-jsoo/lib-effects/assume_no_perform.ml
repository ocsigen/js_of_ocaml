open Printf
open Effect
open Effect.Deep

module type TREE = sig
  type 'a t
  (** The type of tree. *)

  val deep : int -> int t
  (** [deep n] constructs a tree of depth n, in linear time, where every node at
      level [l] has value [l]. *)

  val to_iter : 'a t -> ('a -> unit) -> unit
  (** Iterator function. *)

  val to_gen : 'a t -> unit -> 'a option
  (** Generator function. [to_gen t] returns a generator function [g] for the
      tree that traverses the tree in depth-first fashion, returning [Some x]
      for each node when [g] is invoked. [g] returns [None] once the traversal
      is complete. *)

  val to_gen_cps : 'a t -> unit -> 'a option
  (** CPS version of the generator function. *)
end

module Tree : TREE = struct
  type 'a t =
    | Leaf
    | Node of 'a t * 'a * 'a t

  let rec deep = function
    | 0 -> Leaf
    | n ->
        let t = deep (n - 1) in
        Node (t, n, t)

  let rec iter f = function
    | Leaf -> ()
    | Node (l, x, r) ->
        iter f l;
        f x;
        iter f r

  (* val to_iter : 'a t -> ('a -> unit) -> unit *)
  let to_iter t f = iter f t

  (* val to_gen : 'a t -> (unit -> 'a option) *)
  let to_gen (type a) (t : a t) =
    let module M = struct
      type _ Effect.t += Next : a -> unit Effect.t
    end in
    let open M in
    let rec step =
      ref (fun () ->
          try_with
            (fun t ->
              iter (fun x -> perform (Next x)) t;
              None)
            t
            { effc =
                (fun (type a) (e : a Effect.t) ->
                  match e with
                  | Next v ->
                      Some
                        (fun (k : (a, _) continuation) ->
                          (step := fun () -> continue k ());
                          Some v)
                  | _ -> None)
            })
    in
    fun () -> !step ()

  let to_gen_cps t =
    let next = ref t in
    let cont = ref Leaf in
    let rec iter t k =
      match t with
      | Leaf -> run k
      | Node (left, x, right) -> iter left (Node (k, x, right))
    and run = function
      | Leaf -> None
      | Node (k, x, right) ->
          next := right;
          cont := k;
          Some x
    in
    fun () -> iter !next !cont
end

let get_mean_sd l =
  let get_mean l =
    List.fold_right (fun a v -> a +. v) l 0. /. (float_of_int @@ List.length l)
  in
  let mean = get_mean l in
  let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
  mean, sd

let benchmark f n =
  let rec run acc = function
    | 0 -> acc
    | n ->
        let t1 = Sys.time () in
        let () = f () in
        let d = Sys.time () -. t1 in
        run (d :: acc) (n - 1)
  in
  let r = run [] n in
  get_mean_sd r

(* Main follows *)

type _ Effect.t += Dummy : unit t [@@warning "-38"]

let () =
  try_with
    (fun () ->
      let n = try int_of_string Sys.argv.(1) with _ -> 21 in
      let t = Tree.deep n in
      let iter_fun () = Tree.to_iter t (fun _ -> ()) in
      let rec consume_all f =
        match f () with
        | None -> ()
        | Some _ -> consume_all f
      in

      (* The code below should be called in direct style despite the installed
         effect handler *)
      Jsoo_runtime.Effect.assume_no_perform (fun () ->
          let m, sd = benchmark iter_fun 5 in
          let () = printf "Iter: mean = %f, sd = %f\n%!" m sd in

          let gen_cps_fun () =
            let f = Tree.to_gen_cps t in
            consume_all f
          in

          let m, sd = benchmark gen_cps_fun 5 in
          printf "Gen_cps: mean = %f, sd = %f\n%!" m sd);

      let gen_fun () =
        let f = Tree.to_gen t in
        consume_all f
      in

      let m, sd = benchmark gen_fun 5 in
      printf "Gen_eff: mean = %f, sd = %f\n%!" m sd)
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Dummy -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None)
    }
