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
  | ThreeWood
  | FourHybrid
  | FiveHybrid
  | SixIron
  | SevenIron

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
  | "threewood" -> ThreeWood
  | "fourhybrid" -> FourHybrid
  | "fivehybrid" -> FiveHybrid
  | "sixiron" -> SixIron
  | "seveniron" -> SevenIron
  | _ -> Printf.printf "The text you entered does not represent a valid club, please check your spelling and try again. \n"; 
    read_line() |> Parse.parse |> parse_club

(*A recursive helper that either parses an int [pow] to it's corresponding 
  power or asks the user to give another power along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_power (pow : int)=
  if (pow >= 0 && pow <= 100) 
  then pow
  else 
  if (pow < 0) 
  then let neg_power_message = "Your power cannot be negative, please enter an integer between 0 and 100. > \n" in
    parse_power (for_int_output neg_power_message)
  else 
    let pos_power_message = "Your power cannot be over 100, please enter an integer between 0 and 100. > \n" in
    parse_power (for_int_output pos_power_message)

(*A recursive helper that either parses an int [ang] to it's corresponding 
  angle or asks the user to give another angle along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_angle (ang : int) = 
  if (ang >= 0 && ang <= 90) 
  then ang 
  else 
  if (ang < 0) 
  then let neg_angle_message = "Your angle cannot be negative, please enter an integer between 0 and 90. > \n" in
    parse_angle (for_int_output neg_angle_message)
  else 
    let pos_angle_message =  "Your angle cannot be over 90, please enter an integer between 0 and 90. > \n" in
    parse_angle (for_int_output pos_angle_message)

(*A recursive helper that either parses an int [degrees] to it's corresponding 
  alignment or asks the user to give another alignment along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_alignment (degrees : int) =
  if (degrees >= (-90) && degrees <= 90) 
  then degrees
  else 
  if (degrees < (-90)) 
  then let neg_align_message = "Your alignment cannot be less than -90, please enter an integer between -90 and 90. > \n" in
    parse_alignment (for_int_output neg_align_message)
  else 
    let pos_align_message = "Your alignment cannot be over 90, please enter an integer between -90 and 90. > \n" in
    parse_alignment (for_int_output pos_align_message)

let parse_swing () =
  let club_message =  "Which club would you like to use? (Driver, Nine Iron, Eight Iron, Putter, Pitching Wedge, Sand Wedge, Three Wood, 
  Four Hybrid, Five Hybrid, Six Iron, or Seven Iron) > \n" in
  let club = (for_string_output club_message)|> parse_club in
  let power_message = "How hard would you like to hit the ball? Enter an int between 0 and 100. > \n" in
  let power = (for_int_output power_message)|> parse_power in 
  let angle_message = "If the ground represents 0 degrees, which degree up from the ground would you like to hit the ball? Enter an int. > \n" in 
  let angle = (for_int_output angle_message) |> parse_angle in 
  let align_message = "You are currently pointing directly at the hole. You may pivot up to 90 degrees to your left or right. Enter a negative integer to turn left, 0 to stay, or positive to turn right. > \n" in 
  let alignment = (for_int_output align_message) |> parse_alignment in
  let swing = {
    club = club;
    power = power;
    angle = angle;
    alignment = alignment;
  } in swing

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
  | ThreeWood -> (1.3, 0.85)
  | FourHybrid -> (1.2,0.9)
  | FiveHybrid -> (1.1,0.95)
  | SixIron -> (1., 1.05)
  | SevenIron -> (0.95, 1.075)

