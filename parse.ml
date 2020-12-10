open Str
open Help
(*****************************************************)
(* Implementations of parse and it's functions*)
(*****************************************************)
type parsable = string 

exception Quit of string

let remove_blanks parsbl = 
  parsbl |> Str.global_replace (Str.regexp " ") ""

let parse parsbl = 
  let parsed = parsbl |> remove_blanks |> String.lowercase_ascii in 
  if parsed = "help" 
  then 
    Help.help_menu_init()
  else if parsed = "quit" then raise (Quit "Quitting the game!");
  (* else if parsed = "scorecard" then print_scorecard() *)
  parsed

(* for the help cmo *)
let rec string_catcher str = 
  try int_of_string str
  with Failure _ -> begin
      Printf.printf "Your input was not recognized, please try again> ";
      read_line () |> parse |> string_catcher
    end


let pp_string s = "\"" ^ s ^ "\""

let pp_tup (k,v) = "(" ^ string_of_float k ^ ", " ^ string_of_float v ^ ")"

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


let pp_array func arr = pp_list func (Array.to_list arr)

