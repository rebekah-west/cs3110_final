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
  if parsed = "3110" then Printf.printf "You entered the special 
  code, all hail the camels \n"; 
  parsed

(* for the help cmo *)

let rec for_int_output message = 
  print_string message;
  let input = read_line() in 
  let parsed = input |> remove_blanks |> String.lowercase_ascii in 
  match parsed with 
  | "help" -> Help.help_menu_init(); for_int_output message
  | "quit" -> raise (Quit "Quitting the game!");
  | "3110" -> Printf.printf "You entered the special code, all hail the camels \n"; for_int_output message
  | _ -> string_catcher parsed message

and string_catcher str message = 
  try int_of_string str
  with Failure _ -> begin
      Printf.printf "Your input was not recognized as a valid int, please try again> ";
      for_int_output message;
    end

let rec for_string_output message = 
  print_string message;
  let input = read_line() in 
  let parsed = input |> remove_blanks |> String.lowercase_ascii in 
  match parsed with 
  | "help" -> Help.help_menu_init(); for_string_output message
  | "quit" -> raise (Quit "Quitting the game!");
  | "3110" -> Printf.printf "You entered the special code, all hail the camels \n"; for_string_output message
  | _ -> parsed



