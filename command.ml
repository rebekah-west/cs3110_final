open Str
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

(*remove_blanks does not work*)
let remove_blanks = Str.global_replace (Str.regexp "  ") ""
let parse string = string |> remove_blanks |> String.lowercase_ascii

let parse_club string = 
  let parsed = parse string in
  match parsed with
  | "driver" -> Driver
  | "nineiron" -> NineIron
  | "eightiron" -> EightIron
  | "putter" -> Putter
  | "pitchingwedge" -> PitchingWedge
  | "sandwedge" -> SandWedge
  | _ -> raise (Invalid_argument "That is not a club")

let create_swing (clb: string) (pow : int) (ang : int) (align : int)= {
  club = parse_club (clb);
  power = pow;
  angle = ang;
  alignment = align;
}

let parse_swing () =
  Printf.printf "Which club would you like to use? (Driver, Nine Iron, Eight Iron, Putter, Pitching Wedge, Sand Wedge) \n";
  let club = parse_club (read_line ()) in
  Printf.printf "How hard would you like to hit the ball? Enter an int. \n";
  let power = int_of_string (read_line ()) in 
  Printf.printf "If the ground represents 0 degrees, which degree up from the ground would you like to hit the ball? Enter an int.\n";
  let angle = int_of_string (read_line ()) in 
  Printf.printf "You are currently pointing directly at the hole. You may pivot up to 90 degrees to your left or right. Enter a negative integer to turn left, 0 to stay, or positive to turn right. \n";
  let alignment = int_of_string (read_line ()) in
  let swing = {
    club = club;
    power = power;
    angle = angle;
    alignment = alignment;
  } in swing

let get_command command = 
  failwith "Unimplemented"