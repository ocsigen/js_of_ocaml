(* Json conversion *)
open Common

let log_stop = log_start "Json"

let str = String.create 256

let () =
  for i = 0 to 255 do
    str.[i] <- Char.chr i
  done

type t = int list * float option * string deriving (Json)

let test t v =
  if v = Json.unsafe_input (Json.output v)
  then log_success ()
  else log_failure "Not equal";
  if v = Deriving_Json.from_string t (Js.to_string (Json.output v))
  then log_success ()
  else log_failure "Not equal";
  if v = Json.unsafe_input (Js.string (Deriving_Json.to_string t v))
  then log_success ()
  else log_failure "Not equal";
  if v = Deriving_Json.from_string t (Deriving_Json.to_string t v)
  then log_success ()
  else log_failure "Not equal"

let _ = test Json.t<t> ([1;2;3], Some 1., str)

type intseq = Z | S of int * intseq deriving (Json)

let _ = test Json.t<intseq> (S (1, S (2, S (3, Z))))

type 'a seq = ZZ | SS of 'a * 'a seq deriving (Json)

let _ = test Json.t<int seq> (SS (1, SS (2, SS (3, ZZ))))

let () = log_stop()
