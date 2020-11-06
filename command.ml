open Str
open Parse
(*****************************************************)
(* Implementations of command and it's functions*)
(*****************************************************)
exception Empty
exception Malformed
exception InvalidInput
exception ValueOutOfRange

type club = 
  | Driver
  | NineIron
  | EightIron
  | Putter
  | PitchingWedge
  | SandWedge

type power = int
type angle = int
type alignment = int

type t = {
  club : club;
  power : power;
  angle : angle;
  alignment : alignment;
}

type command =
  | Swing of t
  | None

(* A recursive helper that either parses a string [clb] to its corresponding 
   club or prompts the user to type another club if they spell it wrong*)
let rec parse_club (clb : string) = 
  let parsed = parse clb in
  match parsed with
  | "driver" -> Driver
  | "nineiron" -> NineIron
  | "eightiron" -> EightIron
  | "putter" -> Putter
  | "pitchingwedge" -> PitchingWedge
  | "sandwedge" -> SandWedge
  | _ -> Printf.printf "The text you entered does not represent a valid club, please check your spelling and try again. \n"; 
    parse_club(read_line())

(*A recursive helper that either parses an int [pow] to it's corresponding 
  power or asks the user to give another power along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_power (pow : int)=
  if (pow >= 0 && pow <= 100) 
  then pow
  else 
  if (pow < 0) 
  then Printf.printf "Your power cannot be negative, please enter an integer between 0 and 100. \n" |> 
       fun () -> read_line() |> int_of_string |> parse_power 
  else 
    Printf.printf "Your power cannot be over 100, please enter an integer between 0 and 100. \n" |>  
    fun () -> read_line() |> int_of_string |> parse_power 

(*A recursive helper that either parses an int [ang] to it's corresponding 
  angle or asks the user to give another angle along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_angle (ang : int) = 
  if (ang >= 0 && ang <= 90) 
  then ang 
  else 
  if (ang < 0) 
  then Printf.printf "Your angle cannot be negative, please enter an integer between 0 and 90. \n" |> 
       fun () -> read_line() |> int_of_string |> parse_angle 
  else 
    Printf.printf "Your power cannot be over 90, please enter an integer between 0 and 90. \n" |>  
    fun () -> read_line() |> int_of_string |> parse_angle 

(*A recursive helper that either parses an int [degrees] to it's corresponding 
  alignment or asks the user to give another alignment along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_alignment (degrees : int) =
  if (degrees >= (-90) && degrees <= 90) 
  then degrees
  else 
  if (degrees < (-90)) 
  then Printf.printf "Your alignment cannot be less than -90, please enter an integer between -90 and 90. \n" |> 
       fun () -> read_line() |> int_of_string |> parse_alignment 
  else 
    Printf.printf "Your alignment cannot be over 90, please enter an integer between -90 and 90. \n" |>  
    fun () -> read_line() |> int_of_string |> parse_alignment 

let parse_swing () =
  Printf.printf "Which club would you like to use? (Driver, Nine Iron, Eight Iron, Putter, Pitching Wedge, Sand Wedge) \n";
  let club = parse_club (read_line ()) in
  Printf.printf "How hard would you like to hit the ball? Enter an int. \n";
  let power = read_line () |> int_of_string |> parse_power in 
  Printf.printf "If the ground represents 0 degrees, which degree up from the ground would you like to hit the ball? Enter an int.\n";
  let angle = read_line () |> int_of_string |> parse_angle in 
  Printf.printf "You are currently pointing directly at the hole. You may pivot up to 90 degrees to your left or right. Enter a negative integer to turn left, 0 to stay, or positive to turn right. \n";
  let alignment = read_line () |> int_of_string |> parse_alignment in
  let swing = {
    club = club;
    power = power;
    angle = angle;
    alignment = alignment;
  } in swing

let get_command command = 
  failwith "Unimplemented"

let get_power t = t.power |> float_of_int

let get_club t = t.club

let get_angle t = t.angle |> float_of_int

let get_align t = t.alignment |> float_of_int

let get_club_adjustments (clb : club) = 
  match clb with
  | Driver -> (1.5, 0.8)
  | NineIron -> (0.9,1.1)
  | EightIron -> (0.8,1.2)
  | Putter -> (0.5,1.3)
  | PitchingWedge -> (0.7, 1.5)
  | SandWedge -> (0.8,1.1)

