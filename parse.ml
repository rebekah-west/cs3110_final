open Str
(*****************************************************)
(* Implementations of parse and it's functions*)
(*****************************************************)
type parsable = string 

let remove_blanks parsbl = 
  parsbl |> Str.global_replace (Str.regexp " ") ""

let parse parsbl = 
  parsbl |> remove_blanks |> String.lowercase_ascii