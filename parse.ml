open Str
open Help
(*****************************************************)
(* Implementations of parse and it's functions*)
(*****************************************************)
type parsable = string 

let remove_blanks parsbl = 
  parsbl |> Str.global_replace (Str.regexp " ") ""

let parse parsbl = 
  let parsed = parsbl |> remove_blanks |> String.lowercase_ascii in 
  if parsed = "help" 
  then 
    Help.help_menu_init(); 
  parsed

(* for the help cmo*)
let rec string_catcher str = 
  try int_of_string str
  with Failure "int_of_string" -> begin
      Printf.printf "Your input was not recognized, please try again";
      read_line () |> parse |> string_catcher
    end
